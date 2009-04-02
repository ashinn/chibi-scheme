/*  eval.c -- evaluator library implementation           */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "eval.h"

/************************************************************************/

static int scheme_initialized_p = 0;

static sexp continuation_resumer, final_resumer;
static sexp the_interaction_env_symbol;
static sexp the_err_handler_symbol, the_compile_error_symbol;
static sexp the_cur_in_symbol, the_cur_out_symbol, the_cur_err_symbol;

#if USE_DEBUG
#include "debug.c"
#else
#define print_stack(...)
#define print_bytecode(...)
#define disasm(...)
#endif

static sexp analyze (sexp x, sexp context);
static sexp_sint_t sexp_context_make_label (sexp context);
static void sexp_context_patch_label (sexp context, sexp_sint_t label);
static void generate (sexp x, sexp context);
static sexp sexp_make_null_env (sexp version);
static sexp sexp_make_standard_env (sexp version);

/********************** environment utilities ***************************/

static sexp env_cell(sexp e, sexp key) {
  sexp ls;

  do {
    for (ls=sexp_env_bindings(e); sexp_pairp(ls); ls=sexp_cdr(ls))
      if (sexp_caar(ls) == key)
        return sexp_car(ls);
    e = sexp_env_parent(e);
  } while (e);

  return NULL;
}

static sexp env_cell_create(sexp e, sexp key, sexp value) {
  sexp cell = env_cell(e, key);
  if (! cell) {
    cell = sexp_cons(key, value);
    while (sexp_env_parent(e))
      e = sexp_env_parent(e);
    sexp_env_bindings(e) = sexp_cons(cell, sexp_env_bindings(e));
  }
  return cell;
}

static sexp env_global_ref(sexp e, sexp key, sexp dflt) {
  sexp cell;
  while (sexp_env_parent(e))
    e = sexp_env_parent(e);
  cell = env_cell(e, key);
  return (cell ? sexp_cdr(cell) : dflt);
}

static void env_define(sexp e, sexp key, sexp value) {
  sexp cell = sexp_assq(key, sexp_env_bindings(e));
  if (cell != SEXP_FALSE)
    sexp_cdr(cell) = value;
  else
    sexp_push(sexp_env_bindings(e), sexp_cons(key, value));
}

static sexp extend_env (sexp env, sexp vars, sexp value) {
  sexp e = sexp_alloc_type(env, SEXP_ENV);
  sexp_env_parent(e) = env;
  sexp_env_bindings(e) = SEXP_NULL;
  for ( ; sexp_pairp(vars); vars = sexp_cdr(vars))
    sexp_push(sexp_env_bindings(e), sexp_cons(sexp_car(vars), value));
  return e;
}

static sexp sexp_reverse_flatten_dot (sexp ls) {
  sexp res;
  for (res=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_push(res, sexp_car(ls));
  return (sexp_nullp(ls) ? res : sexp_cons(ls, res));
}

static sexp sexp_flatten_dot (sexp ls) {
  return sexp_nreverse(sexp_reverse_flatten_dot(ls));
}

static int sexp_param_index (sexp lambda, sexp name) {
  sexp ls = sexp_lambda_params(lambda);
  int i = 0;
  for (i=0; sexp_pairp(ls); ls=sexp_cdr(ls), i++)
    if (sexp_car(ls) == name)
      return i;
  if (ls == name)
    return i;
  ls = sexp_lambda_locals(lambda);
  for (i=-1; sexp_pairp(ls); ls=sexp_cdr(ls), i--)
    if (sexp_car(ls) == name)
      return i-4;
  return -10000;
}

/************************* bytecode utilities ***************************/

static void shrink_bcode(sexp context, sexp_uint_t i) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(context)) != i) {
    tmp = sexp_alloc_tagged(sexp_sizeof(bytecode) + i, SEXP_BYTECODE);
    sexp_bytecode_length(tmp) = i;
    sexp_bytecode_literals(tmp)
      = sexp_bytecode_literals(sexp_context_bc(context));
    memcpy(sexp_bytecode_data(tmp),
           sexp_bytecode_data(sexp_context_bc(context)),
           i);
    sexp_context_bc(context) = tmp;
  }
}

static void expand_bcode(sexp context, sexp_uint_t size) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(context))
      < (sexp_context_pos(context))+size) {
    tmp = sexp_alloc_tagged(sexp_sizeof(bytecode)
                            + sexp_bytecode_length(sexp_context_bc(context))*2,
                            SEXP_BYTECODE);
    sexp_bytecode_length(tmp)
      = sexp_bytecode_length(sexp_context_bc(context))*2;
    sexp_bytecode_literals(tmp)
      = sexp_bytecode_literals(sexp_context_bc(context));
    memcpy(sexp_bytecode_data(tmp),
           sexp_bytecode_data(sexp_context_bc(context)),
           sexp_bytecode_length(sexp_context_bc(context)));
    sexp_context_bc(context) = tmp;
  }
}

static void emit(char c, sexp context)  {
  expand_bcode(context, 1);
  sexp_bytecode_data(sexp_context_bc(context))[sexp_context_pos(context)++] = c;
}

static void emit_word(sexp_uint_t val, sexp context)  {
  unsigned char *data;
  expand_bcode(context, sizeof(sexp));
  data = sexp_bytecode_data(sexp_context_bc(context));
  *((sexp_uint_t*)(&(data[sexp_context_pos(context)]))) = val;
  sexp_context_pos(context) += sizeof(sexp);
}

static void emit_push(sexp obj, sexp context) {
  emit(OP_PUSH, context);
  emit_word((sexp_uint_t)obj, context);
  if (sexp_pointerp(obj))
    sexp_push(sexp_bytecode_literals(sexp_context_bc(context)), obj);
}

static sexp sexp_make_procedure(sexp flags, sexp num_args,
                                sexp bc, sexp vars) {
  sexp proc = sexp_alloc_type(procedure, SEXP_PROCEDURE);
  sexp_procedure_flags(proc) = (char) (sexp_uint_t) flags;
  sexp_procedure_num_args(proc) = (unsigned short) (sexp_uint_t) num_args;
  sexp_procedure_code(proc) = bc;
  sexp_procedure_vars(proc) = vars;
  return proc;
}

static sexp sexp_make_macro (sexp p, sexp e) {
  sexp mac = sexp_alloc_type(macro, SEXP_MACRO);
  sexp_macro_env(mac) = e;
  sexp_macro_proc(mac) = p;
  return mac;
}

static sexp sexp_make_synclo (sexp env, sexp fv, sexp expr) {
  sexp res;
  if (sexp_synclop(expr))
    return expr;
  res = sexp_alloc_type(synclo, SEXP_SYNCLO);
  sexp_synclo_env(res) = env;
  sexp_synclo_free_vars(res) = fv;
  sexp_synclo_expr(res) = expr;
  return res;
}

/* internal AST */

static sexp sexp_make_lambda(sexp params) {
  sexp res = sexp_alloc_type(lambda, SEXP_LAMBDA);
  sexp_lambda_params(res) = params;
  sexp_lambda_fv(res) = SEXP_NULL;
  sexp_lambda_sv(res) = SEXP_NULL;
  sexp_lambda_locals(res) = SEXP_NULL;
  sexp_lambda_defs(res) = SEXP_NULL;
  return res;
}

static sexp sexp_make_set(sexp var, sexp value) {
  sexp res = sexp_alloc_type(set, SEXP_SET);
  sexp_set_var(res) = var;
  sexp_set_value(res) = value;
  return res;
}

static sexp sexp_make_ref(sexp name, sexp cell) {
  sexp res = sexp_alloc_type(ref, SEXP_REF);
  sexp_ref_name(res) = name;
  sexp_ref_cell(res) = cell;
  return res;
}

static sexp sexp_make_cnd(sexp test, sexp pass, sexp fail) {
  sexp res = sexp_alloc_type(cnd, SEXP_CND);
  sexp_cnd_test(res) = test;
  sexp_cnd_pass(res) = pass;
  sexp_cnd_fail(res) = fail;
  return res;
}

static sexp sexp_make_lit(sexp value) {
  sexp res = sexp_alloc_type(lit, SEXP_LIT);
  sexp_lit_value(res) = value;
  return res;
}

static sexp sexp_make_context(sexp *stack, sexp env) {
  sexp res = sexp_alloc_type(context, SEXP_CONTEXT);
  if (! stack)
    stack = (sexp*) sexp_alloc(sizeof(sexp)*INIT_STACK_SIZE);
  if (! env)
    env = sexp_make_standard_env(sexp_make_integer(5));
  sexp_context_bc(res)
    = sexp_alloc_tagged(sexp_sizeof(bytecode)+INIT_BCODE_SIZE, SEXP_BYTECODE);
  sexp_bytecode_length(sexp_context_bc(res)) = INIT_BCODE_SIZE;
  sexp_bytecode_literals(sexp_context_bc(res)) = SEXP_NULL;
  sexp_context_lambda(res) = SEXP_FALSE;
  sexp_context_stack(res) = stack;
  sexp_context_env(res) = env;
  sexp_context_depth(res) = 0;
  sexp_context_pos(res) = 0;
  sexp_context_top(res) = 0;
  sexp_context_tailp(res) = 1;
  return res;
}

static sexp sexp_child_context(sexp context, sexp lambda) {
  sexp ctx = sexp_make_context(sexp_context_stack(context),
                               sexp_context_env(context));
  sexp_context_lambda(ctx) = lambda;
  sexp_context_env(ctx) = sexp_context_env(context);
  sexp_context_top(ctx) = sexp_context_top(context);
  return ctx;
}

#define sexp_idp(x) (sexp_symbolp(x) || (sexp_synclop(x) && sexp_symbolp(sexp_synclo_expr(x))))

static sexp sexp_identifierp (sexp x) {
  return sexp_make_boolean(sexp_idp(x));
}

static sexp sexp_identifier_eq (sexp e1, sexp id1, sexp e2, sexp id2) {
  sexp cell, lam1=SEXP_FALSE, lam2=SEXP_FALSE;
  if (sexp_synclop(id1)) {
    e1 = sexp_synclo_env(id1);
    id1 = sexp_synclo_expr(id1);
  }
  if (sexp_synclop(id2)) {
    e2 = sexp_synclo_env(id2);
    id2 = sexp_synclo_expr(id2);
  }
  cell = env_cell(e1, id1);
  if (cell && sexp_lambdap(sexp_cdr(cell)))
    lam1 = sexp_cdr(cell);
  cell = env_cell(e2, id2);
  if (cell && sexp_lambdap(sexp_cdr(cell)))
    lam2 = sexp_cdr(cell);
  return sexp_make_boolean((id1 == id2) && (lam1 == lam2));
}

/************************* the compiler ***************************/

static sexp sexp_compile_error(char *message, sexp irritants) {
  return sexp_make_exception(the_compile_error_symbol,
                             sexp_make_string(message),
                             irritants, SEXP_FALSE, SEXP_FALSE);
}

#define analyze_check_exception(x) do {if (sexp_exceptionp(x))          \
                                         return (x);                    \
                                      } while (0)

#define analyze_bind(var, x, context) do {(var) = analyze(x,context);   \
                                          analyze_check_exception(var); \
                                     } while (0)

static sexp analyze_app (sexp x, sexp context) {
  sexp res=SEXP_NULL, tmp;
  for ( ; sexp_pairp(x); x=sexp_cdr(x)) {
    analyze_bind(tmp, sexp_car(x), context);
    sexp_push(res, tmp);
  }
  return sexp_nreverse(res);
}

static sexp analyze_seq (sexp ls, sexp context) {
  sexp res, tmp;
  if (sexp_nullp(ls))
    res = SEXP_UNDEF;
  else if (sexp_nullp(sexp_cdr(ls)))
    res = analyze(sexp_car(ls), context);
  else {
    res = sexp_alloc_type(seq, SEXP_SEQ);
    tmp = analyze_app(ls, context);
    analyze_check_exception(tmp);
    sexp_seq_ls(res) = tmp;
  }
  return res;
}

static sexp analyze_var_ref (sexp x, sexp context) {
  sexp cell = env_cell(sexp_context_env(context), x);
  if (! cell) {
    if (sexp_synclop(x)) {
      cell = env_cell_create(sexp_synclo_env(x),
                             sexp_synclo_expr(x),
                             SEXP_UNDEF);
      x = sexp_synclo_expr(x);
    } else {
      cell = env_cell_create(sexp_context_env(context), x, SEXP_UNDEF);
    }
  }
  return sexp_make_ref(x, cell);
}

static sexp analyze_set (sexp x, sexp context) {
  sexp ref, value;
  ref = analyze_var_ref(sexp_cadr(x), context);
  if (sexp_lambdap(sexp_ref_loc(ref)))
    sexp_insert(sexp_lambda_sv(sexp_ref_loc(ref)), sexp_ref_name(ref));
  analyze_check_exception(ref);
  analyze_bind(value, sexp_caddr(x), context);
  return sexp_make_set(ref, value);
}

static sexp analyze_lambda (sexp x, sexp context) {
  sexp res, body, ls, tmp, name, value, defs=SEXP_NULL;
  /* verify syntax */
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x))))
    return sexp_compile_error("bad lambda syntax", sexp_list1(x));
  for (ls=sexp_cadr(x); sexp_pairp(ls); ls=sexp_cdr(ls))
    if (! sexp_idp(sexp_car(ls)))
      return sexp_compile_error("non-symbol parameter", sexp_list1(x));
    else if (sexp_memq(sexp_car(ls), sexp_cdr(ls)) != SEXP_FALSE)
      return sexp_compile_error("duplicate parameter", sexp_list1(x));
  /* build lambda and analyze body */
  res = sexp_make_lambda(sexp_cadr(x));
  context = sexp_child_context(context, res);
  sexp_context_env(context)
    = extend_env(sexp_context_env(context),
                 sexp_flatten_dot(sexp_lambda_params(res)),
                 res);
  sexp_env_lambda(sexp_context_env(context)) = res;
  body = analyze_seq(sexp_cddr(x), context);
  analyze_check_exception(body);
  /* delayed analyze internal defines */
  for (ls=sexp_lambda_defs(res); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    tmp = sexp_car(ls);
    if (sexp_pairp(sexp_cadr(tmp))) {
      name = sexp_caadr(tmp);
      value = analyze_lambda(sexp_cons(SEXP_UNDEF, sexp_cons(sexp_cdadr(tmp),
                                                             sexp_cddr(tmp))),
                             context);
    } else {
      name = sexp_cadr(tmp);
      value = analyze(sexp_caddr(tmp), context);
    }
    analyze_check_exception(value);
    sexp_push(defs, sexp_make_set(analyze_var_ref(name, context), value));
  }
  if (sexp_pairp(defs)) {
    if (! sexp_seqp(body)) {
      tmp = sexp_alloc_type(seq, SEXP_SEQ);
      sexp_seq_ls(tmp) = sexp_list1(body);
      body = tmp;
    }
    sexp_seq_ls(body) = sexp_append(defs, sexp_seq_ls(body));
  }
  sexp_lambda_body(res) = body;
  return res;
}

static sexp analyze_if (sexp x, sexp context) {
  sexp test, pass, fail, fail_expr;
  analyze_bind(test, sexp_cadr(x), context);
  analyze_bind(pass, sexp_caddr(x), context);
  fail_expr = sexp_pairp(sexp_cdddr(x)) ? sexp_cadddr(x) : SEXP_UNDEF;
  analyze_bind(fail, fail_expr, context);
  return sexp_make_cnd(test, pass, fail);
}

static sexp analyze_define (sexp x, sexp context) {
  sexp ref, name, value, env = sexp_context_env(context);
  name = (sexp_pairp(sexp_cadr(x)) ? sexp_caadr(x) : sexp_cadr(x));
  if (sexp_env_lambda(env) && sexp_lambdap(sexp_env_lambda(env))) {
    sexp_push(sexp_env_bindings(env),
              sexp_cons(name, sexp_context_lambda(context)));
    sexp_push(sexp_lambda_sv(sexp_env_lambda(env)), name);
    sexp_push(sexp_lambda_locals(sexp_env_lambda(env)), name);
    sexp_push(sexp_lambda_defs(sexp_env_lambda(env)), x);
    return SEXP_UNDEF;
  } else {
    env_cell_create(env, name, SEXP_DEF);
  }
  if (sexp_pairp(sexp_cadr(x)))
    value = analyze_lambda(sexp_cons(SEXP_UNDEF,
                                     sexp_cons(sexp_cdadr(x), sexp_cddr(x))),
                           context);
  else
    value = analyze(sexp_caddr(x), context);
  analyze_check_exception(value);
  ref = analyze_var_ref(name, context);
  analyze_check_exception(ref);
  return sexp_make_set(ref, value);
}

static sexp analyze_define_syntax (sexp x, sexp context) {
  sexp name = sexp_cadr(x), cell, proc;
  proc = eval_in_context(sexp_caddr(x), context);
  analyze_check_exception(proc);
  cell = env_cell_create(sexp_context_env(context), name, SEXP_UNDEF);
  sexp_cdr(cell) = sexp_make_macro(proc, sexp_context_env(context));
  return SEXP_UNDEF;
}

static sexp analyze (sexp x, sexp context) {
  sexp op, cell, res;
 loop:
  if (sexp_pairp(x)) {
    if (sexp_listp(x) == SEXP_FALSE) {
      res = sexp_compile_error("dotted list in source", sexp_list1(x));
    } else if (sexp_idp(sexp_car(x))) {
      if (sexp_synclop(sexp_car(x)))
        cell = env_cell(sexp_synclo_env(sexp_car(x)),
                        sexp_synclo_expr(sexp_car(x)));
      else
        cell = env_cell(sexp_context_env(context), sexp_car(x));
      if (! cell) return analyze_app(x, context);
      op = sexp_cdr(cell);
      if (sexp_corep(op)) {
        switch (sexp_core_code(op)) {
        case CORE_DEFINE:
          res = analyze_define(x, context);
          break;
        case CORE_SET:
          res = analyze_set(x, context);
          break;
        case CORE_LAMBDA:
          res = analyze_lambda(x, context);
          break;
        case CORE_IF:
          res = analyze_if(x, context);
          break;
        case CORE_BEGIN:
          res = analyze_seq(x, context);
          break;
        case CORE_QUOTE:
          res = sexp_make_lit(sexp_cadr(x));
          break;
        case CORE_DEFINE_SYNTAX:
          res = analyze_define_syntax(x, context);
          break;
        default:
          res = sexp_compile_error("unknown core form", sexp_list1(op));
          break;
        }
      } else if (sexp_macrop(op)) {
        x = apply(sexp_macro_proc(op),
                  sexp_list3(x, sexp_context_env(context), sexp_macro_env(op)),
                  sexp_child_context(context, sexp_context_lambda(context)));
        /* sexp_debug("expand => ", x, context); */
        goto loop;
      } else if (sexp_opcodep(op)) {
        res = sexp_length(sexp_cdr(x));
        if (sexp_unbox_integer(res) < sexp_opcode_num_args(op)) {
          res = sexp_compile_error("not enough args for opcode", sexp_list1(x));
        } else if ((sexp_unbox_integer(res) > sexp_opcode_num_args(op))
                   && (! sexp_opcode_variadic_p(op))) {
          res = sexp_compile_error("too many args for opcode", sexp_list1(x));
        } else {
          res = analyze_app(sexp_cdr(x), context);
          analyze_check_exception(res);
          sexp_push(res, op);
        }
      } else {
        res = analyze_app(x, context);
      }
    } else {
      res = analyze_app(x, context);
    }
  } else if (sexp_idp(x)) {
    res = analyze_var_ref(x, context);
  } else if (sexp_synclop(x)) {
    context = sexp_child_context(context, sexp_context_lambda(context));
    sexp_context_env(context) = sexp_synclo_env(x);
    x = sexp_synclo_expr(x);
    goto loop;
  } else {
    res = x;
  }
  return res;
}

static sexp_sint_t sexp_context_make_label (sexp context) {
  sexp_sint_t label = sexp_context_pos(context);
  sexp_context_pos(context) += sizeof(sexp_uint_t);
  return label;
}

static void sexp_context_patch_label (sexp context, sexp_sint_t label) {
  sexp bc = sexp_context_bc(context);
  unsigned char *data = sexp_bytecode_data(bc)+label;
  *((sexp_sint_t*)data) = sexp_context_pos(context)-label;
}

static sexp finalize_bytecode (sexp context) {
  emit(OP_RET, context);
  shrink_bcode(context, sexp_context_pos(context));
/*   disasm(sexp_context_bc(context), */
/*          env_global_ref(sexp_context_env(context), */
/*                         the_cur_err_symbol, */
/*                         SEXP_FALSE)); */
  return sexp_context_bc(context);
}

static void generate_lit (sexp value, sexp context) {
  emit_push(value, context);
}

static void generate_seq (sexp app, sexp context) {
  sexp head=app, tail=sexp_cdr(app);
  sexp_uint_t tailp = sexp_context_tailp(context);
  sexp_context_tailp(context) = 0;
  for ( ; sexp_pairp(tail); head=tail, tail=sexp_cdr(tail))
    if (sexp_pointerp(sexp_car(head)) && (! sexp_litp(sexp_car(head)))) {
      generate(sexp_car(head), context);
      emit(OP_DROP, context);
      sexp_context_depth(context)--;
    }
  sexp_context_tailp(context) = tailp;
  generate(sexp_car(head), context);
}

static void generate_cnd (sexp cnd, sexp context) {
  sexp_sint_t label1, label2, tailp=sexp_context_tailp(context);
  sexp_context_tailp(context) = 0;
  generate(sexp_cnd_test(cnd), context);
  sexp_context_tailp(context) = tailp;
  emit(OP_JUMP_UNLESS, context);
  sexp_context_depth(context)--;
  label1 = sexp_context_make_label(context);
  generate(sexp_cnd_pass(cnd), context);
  emit(OP_JUMP, context);
  sexp_context_depth(context)--;
  label2 = sexp_context_make_label(context);
  sexp_context_patch_label(context, label1);
  generate(sexp_cnd_fail(cnd), context);
  sexp_context_patch_label(context, label2);
}

static void generate_non_global_ref (sexp name, sexp cell, sexp lambda,
                                     sexp fv, sexp context, int unboxp) {
  sexp_uint_t i;
  sexp loc = sexp_cdr(cell);
  if (loc == lambda && sexp_lambdap(lambda)) {
    /* local ref */
    emit(OP_LOCAL_REF, context);
    emit_word(sexp_param_index(lambda, name), context);
  } else {
    /* closure ref */
    for (i=0; sexp_pairp(fv); fv=sexp_cdr(fv), i++)
      if ((name == sexp_ref_name(sexp_car(fv)))
          && (loc == sexp_ref_loc(sexp_car(fv))))
        break;
    emit(OP_CLOSURE_REF, context);
    emit_word(i, context);
  }
  if (unboxp && (sexp_memq(name, sexp_lambda_sv(loc)) != SEXP_FALSE))
    emit(OP_CDR, context);
  sexp_context_depth(context)++;
}

static void generate_ref (sexp ref, sexp context, int unboxp) {
  sexp lam;
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global ref */
    emit_push(sexp_ref_cell(ref), context);
    if (unboxp)
      emit(OP_CDR, context);
  } else {
    lam = sexp_context_lambda(context);
    generate_non_global_ref(sexp_ref_name(ref), sexp_ref_cell(ref), lam,
                            sexp_lambda_fv(lam), context, unboxp);
  }
}

static void generate_set (sexp set, sexp context) {
  sexp ref = sexp_set_var(set), lambda;
  /* compile the value */
  sexp_context_tailp(context) = 0;
  generate(sexp_set_value(set), context);
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global vars are set directly */
    emit_push(sexp_ref_cell(ref), context);
    emit(OP_SET_CDR, context);
  } else {
    lambda = sexp_ref_loc(ref);
    if (sexp_memq(sexp_ref_name(ref), sexp_lambda_sv(lambda)) != SEXP_FALSE) {
      /* stack or closure mutable vars are boxed */
      generate_ref(ref, context, 0);
      emit(OP_SET_CDR, context);
    } else {
      /* internally defined variable */
      emit(OP_LOCAL_SET, context);
      emit_word(sexp_param_index(lambda, sexp_ref_name(ref)), context);
    }
  }
  sexp_context_depth(context)--;
}

static void generate_opcode_app (sexp app, sexp context) {
  sexp ls, op = sexp_car(app);
  sexp_sint_t i, num_args = sexp_unbox_integer(sexp_length(sexp_cdr(app)));
  sexp_context_tailp(context) = 0;

  /* maybe push the default for an optional argument */
  if ((num_args == sexp_opcode_num_args(op))
      && sexp_opcode_variadic_p(op)
      && sexp_opcode_data(op)
      && sexp_opcode_opt_param_p(op)) {
    emit_push(sexp_opcode_data(op), context);
    emit(OP_CDR, context);
    sexp_context_depth(context)++;
    num_args++;
  }

  /* push the arguments onto the stack */
  ls = ((sexp_opcode_inverse(op)
         && (sexp_opcode_class(op) != OPC_ARITHMETIC_INV))
        ? sexp_cdr(app) : sexp_reverse(sexp_cdr(app)));
  for ( ; sexp_pairp(ls); ls = sexp_cdr(ls))
    generate(sexp_car(ls), context);

  /* emit the actual operator call */
  switch (sexp_opcode_class(op)) {
  case OPC_ARITHMETIC_INV:
    emit((num_args == 1) ? sexp_opcode_inverse(op)
         : sexp_opcode_code(op), context);
    break;
  case OPC_ARITHMETIC_CMP:
    if (num_args > 2) {
      emit(OP_STACK_REF, context);
      emit_word(2, context);
      emit(OP_STACK_REF, context);
      emit_word(2, context);
      emit(sexp_opcode_code(op), context);
      emit(OP_AND, context);
      for (i=num_args-2; i>0; i--) {
        emit(OP_STACK_REF, context);
        emit_word(3, context);
        emit(OP_STACK_REF, context);
        emit_word(3, context);
        emit(sexp_opcode_code(op), context);
        emit(OP_AND, context);
        emit(OP_AND, context);
      }
    } else
      emit(sexp_opcode_code(op), context);
    break;
  case OPC_FOREIGN:
  case OPC_TYPE_PREDICATE:
    /* push the funtion pointer for foreign calls */
    emit(sexp_opcode_code(op), context);
    if (sexp_opcode_data(op))
      emit_word((sexp_uint_t)sexp_opcode_data(op), context);
    break;
  case OPC_PARAMETER:
    emit_push(sexp_opcode_data(op), context);
    emit(OP_CDR, context);
  default:
    emit(sexp_opcode_code(op), context);
  }

  /* emit optional folding of operator */
  if ((num_args > 2)
      && (sexp_opcode_class(op) == OPC_ARITHMETIC
          || sexp_opcode_class(op) == OPC_ARITHMETIC_INV))
    for (i=num_args-2; i>0; i--)
      emit(sexp_opcode_code(op), context);

  sexp_context_depth(context) -= (num_args-1);
}

static void generate_general_app (sexp app, sexp context) {
  sexp ls;
  sexp_uint_t len = sexp_unbox_integer(sexp_length(sexp_cdr(app))),
    tailp = sexp_context_tailp(context);

  /* push the arguments onto the stack */
  sexp_context_tailp(context) = 0;
  for (ls = sexp_reverse(sexp_cdr(app)); sexp_pairp(ls); ls = sexp_cdr(ls))
    generate(sexp_car(ls), context);

  /* push the operator onto the stack */
  generate(sexp_car(app), context);

  /* maybe overwrite the current frame */
  emit((tailp ? OP_TAIL_CALL : OP_CALL), context);
  emit_word((sexp_uint_t)sexp_make_integer(len), context);

  sexp_context_depth(context) -= len;
}

static void generate_app (sexp app, sexp context) {
  if (sexp_opcodep(sexp_car(app)))
    generate_opcode_app(app, context);
  else
    generate_general_app(app, context);
}

static void generate_lambda (sexp lambda, sexp context) {
  sexp fv, ls, ctx, flags, bc, len, ref, vec, prev_lambda, prev_fv;
  sexp_uint_t k;
  prev_lambda = sexp_context_lambda(context);
  prev_fv = sexp_lambdap(prev_lambda) ? sexp_lambda_fv(prev_lambda) : SEXP_NULL;
  fv = sexp_lambda_fv(lambda);
  ctx = sexp_make_context(sexp_context_stack(context),
                          sexp_context_env(context));
  sexp_context_lambda(ctx) = lambda;
  /* allocate space for local vars */
  for (ls=sexp_lambda_locals(lambda); sexp_pairp(ls); ls=sexp_cdr(ls))
    emit_push(SEXP_UNDEF, ctx);
  /* box mutable vars */
  for (ls=sexp_lambda_sv(lambda); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    k = sexp_param_index(lambda, sexp_car(ls));
    if (k >= 0) {
      emit(OP_LOCAL_REF, ctx);
      emit_word(k, ctx);
      emit_push(sexp_car(ls), ctx);
      emit(OP_CONS, ctx);
      emit(OP_LOCAL_SET, ctx);
      emit_word(k, ctx);
      emit(OP_DROP, ctx);
    }
  }
  generate(sexp_lambda_body(lambda), ctx);
  flags = sexp_make_integer((sexp_listp(sexp_lambda_params(lambda))==SEXP_FALSE)
                            ? 1 : 0);
  len = sexp_length(sexp_lambda_params(lambda));
  bc = finalize_bytecode(ctx);
  if (sexp_nullp(fv)) {
    /* shortcut, no free vars */
    vec = sexp_make_vector(sexp_make_integer(0), SEXP_UNDEF);
    generate_lit(sexp_make_procedure(flags, len, bc, vec), context);
  } else {
    /* push the closed vars */
    emit_push(SEXP_UNDEF, context);
    emit_push(sexp_length(fv), context);
    emit(OP_MAKE_VECTOR, context);
    sexp_context_depth(context)--;
    for (k=0; sexp_pairp(fv); fv=sexp_cdr(fv), k++) {
      ref = sexp_car(fv);
      generate_non_global_ref(sexp_ref_name(ref), sexp_ref_cell(ref),
                              prev_lambda, prev_fv, context, 0);
      emit_push(sexp_make_integer(k), context);
      emit(OP_STACK_REF, context);
      emit_word(3, context);
      emit(OP_VECTOR_SET, context);
      emit(OP_DROP, context);
      sexp_context_depth(context)--;
    }
    /* push the additional procedure info and make the closure */
    emit_push(bc, context);
    emit_push(len, context);
    emit_push(flags, context);
    emit(OP_MAKE_PROCEDURE, context);
  }
}

static void generate (sexp x, sexp context) {
  if (sexp_pointerp(x)) {
    switch (sexp_pointer_tag(x)) {
    case SEXP_PAIR:
      generate_app(x, context);
      break;
    case SEXP_LAMBDA:
      generate_lambda(x, context);
      break;
    case SEXP_CND:
      generate_cnd(x, context);
      break;
    case SEXP_REF:
      generate_ref(x, context, 1);
      break;
    case SEXP_SET:
      generate_set(x, context);
      break;
    case SEXP_SEQ:
      generate_seq(sexp_seq_ls(x), context);
      break;
    case SEXP_LIT:
      generate_lit(sexp_lit_value(x), context);
      break;
    default:
      generate_lit(x, context);
    }
  } else {
    generate_lit(x, context);
  }
}

static sexp insert_free_var (sexp x, sexp fv) {
  sexp name=sexp_ref_name(x), loc=sexp_ref_loc(x), ls;
  for (ls=fv; sexp_pairp(ls); ls=sexp_cdr(ls))
    if ((name == sexp_ref_name(sexp_car(ls)))
        && (loc == sexp_ref_loc(sexp_car(ls))))
      return fv;
  return sexp_cons(x, fv);
}

static sexp union_free_vars (sexp fv1, sexp fv2) {
  if (sexp_nullp(fv2))
    return fv1;
  for ( ; sexp_pairp(fv1); fv1=sexp_cdr(fv1))
    fv2 = insert_free_var(sexp_car(fv1), fv2);
  return fv2;
}

static sexp diff_free_vars (sexp lambda, sexp fv, sexp params) {
  sexp res = SEXP_NULL;
  for ( ; sexp_pairp(fv); fv=sexp_cdr(fv))
    if ((sexp_ref_loc(sexp_car(fv)) != lambda)
        || (sexp_memq(sexp_ref_name(sexp_car(fv)), params) == SEXP_FALSE))
      sexp_push(res, sexp_car(fv));
  return res;
}

static sexp free_vars (sexp x, sexp fv) {
  sexp fv1, fv2;
  if (sexp_lambdap(x)) {
    fv1 = free_vars(sexp_lambda_body(x), SEXP_NULL);
    fv2 = diff_free_vars(x,
                         fv1,
                         sexp_append(sexp_lambda_locals(x),
                                     sexp_flatten_dot(sexp_lambda_params(x))));
    sexp_lambda_fv(x) = fv2;
    fv = union_free_vars(fv2, fv);
  } else if (sexp_pairp(x)) {
    for ( ; sexp_pairp(x); x=sexp_cdr(x))
      fv = free_vars(sexp_car(x), fv);
  } else if (sexp_cndp(x)) {
    fv = free_vars(sexp_cnd_test(x), fv);
    fv = free_vars(sexp_cnd_pass(x), fv);
    fv = free_vars(sexp_cnd_fail(x), fv);
  } else if (sexp_seqp(x)) {
    for (x=sexp_seq_ls(x); sexp_pairp(x); x=sexp_cdr(x))
      fv = free_vars(sexp_car(x), fv);
  } else if (sexp_setp(x)) {
    fv = free_vars(sexp_set_value(x), fv);
    fv = free_vars(sexp_set_var(x), fv);
  } else if (sexp_refp(x) && sexp_lambdap(sexp_ref_loc(x))) {
    fv = insert_free_var(x, fv);
  } else if (sexp_synclop(x)) {
    fv = free_vars(sexp_synclo_expr(x), fv);
  }
  return fv;
}

static sexp make_param_list(sexp_uint_t i) {
  sexp res = SEXP_NULL;
  char sym[2]="a";
  for (sym[0]+=i; i>0; i--) {
    sym[0] = sym[0]-1;
    res = sexp_cons(sexp_intern(sym), res);
  }
  return res;
}

static sexp make_opcode_procedure (sexp op, sexp_uint_t i, sexp env,
                                   sexp *stack, sexp_sint_t top) {
  sexp context, lambda, params, refs, ls, bc, res;
  if (i == sexp_opcode_num_args(op) && sexp_opcode_proc(op))
    return sexp_opcode_proc(op);
  params = make_param_list(i);
  lambda = sexp_make_lambda(params);
  env = extend_env(env, params, lambda);
  context = sexp_make_context(stack, env);
  sexp_context_lambda(context) = lambda;
  sexp_context_top(context) = top;
  for (ls=params, refs=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_push(refs, sexp_make_ref(sexp_car(ls), env_cell(env, sexp_car(ls))));
  generate_opcode_app(sexp_cons(op, sexp_reverse(refs)), context);
  bc = finalize_bytecode(context);
  res = sexp_make_procedure(sexp_make_integer(0),
                            sexp_make_integer(i),
                            bc,
                            SEXP_UNDEF);
  if (i == sexp_opcode_num_args(op))
    sexp_opcode_proc(op) = res;
  return res;
}

/*********************** the virtual machine **************************/

static sexp sexp_save_stack(sexp *stack, sexp_uint_t to) {
  sexp res, *data;
  sexp_uint_t i;
  res = sexp_make_vector(sexp_make_integer(to), SEXP_UNDEF);
  data = sexp_vector_data(res);
  for (i=0; i<to; i++)
    data[i] = stack[i];
  return res;
}

static sexp_uint_t sexp_restore_stack(sexp saved, sexp *current) {
  sexp_uint_t len = sexp_vector_length(saved), i;
  sexp *from = sexp_vector_data(saved);
  for (i=0; i<len; i++)
    current[i] = from[i];
  return len;
}

#define _ARG1 stack[top-1]
#define _ARG2 stack[top-2]
#define _ARG3 stack[top-3]
#define _ARG4 stack[top-4]
#define _ARG5 stack[top-5]
#define _PUSH(x) (stack[top++]=(x))
#define _WORD0 ((sexp*)ip)[0]
#define _UWORD0 ((sexp_uint_t*)ip)[0]
#define _SWORD0 ((sexp_sint_t*)ip)[0]

#define sexp_raise(msg, args) do {stack[top]=sexp_compile_error(msg, args); \
                                  top++;                                    \
                                  goto call_error_handler;}                 \
                               while (0)

#define sexp_check_exception() do {if (sexp_exceptionp(_ARG1)) \
                                     goto call_error_handler;} \
                                while (0)

sexp vm(sexp bc, sexp cp, sexp context, sexp* stack, sexp_sint_t top) {
  unsigned char *ip=sexp_bytecode_data(bc);
  sexp tmp1, tmp2, env=sexp_context_env(context);
  sexp_sint_t i, j, k, fp=top-4;

 loop:
/*   print_stack(stack, top, fp, env_global_ref(env, the_cur_err_symbol, SEXP_FALSE)); */
/*   fprintf(stderr, "%s\n", (*ip<=71)?reverse_opcode_names[*ip]:"UNKNOWN"); */
  switch (*ip++) {
  case OP_NOOP:
    fprintf(stderr, "<<<NOOP>>>\n");
    break;
  case OP_ERROR:
  call_error_handler:
    tmp1 = env_global_ref(env, the_cur_err_symbol, SEXP_FALSE);
    sexp_print_exception(_ARG1, tmp1);
    tmp1 = env_global_ref(env, the_err_handler_symbol, SEXP_FALSE);
    stack[top] = (sexp) 1;
    stack[top+1] = sexp_make_integer(ip+4);
    stack[top+2] = cp;
    top += 3;
    bc = sexp_procedure_code(tmp1);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
    break;
  case OP_RESUMECC:
    tmp1 = stack[fp-1];
    top = sexp_restore_stack(sexp_vector_ref(cp, 0), stack);
    fp = sexp_unbox_integer(_ARG1);
    cp = _ARG2;
    ip = (unsigned char*) sexp_unbox_integer(_ARG3);
    i = sexp_unbox_integer(_ARG4);
    top -= 4;
    _ARG1 = tmp1;
    break;
  case OP_CALLCC:
    tmp1 = _ARG1;
    i = 1;
    stack[top] = sexp_make_integer(1);
    stack[top+1] = sexp_make_integer(ip);
    stack[top+2] = cp;
    stack[top+3] = sexp_make_integer(fp);
    tmp2 = sexp_vector(1, sexp_save_stack(stack, top+4));
    _ARG1 = sexp_make_procedure(sexp_make_integer(0),
                                sexp_make_integer(1),
                                continuation_resumer,
                                tmp2);
    top++;
    ip -= sizeof(sexp);
    goto make_call;
    break;
  case OP_APPLY1:
    tmp1 = _ARG1;
    tmp2 = _ARG2;
    i = sexp_unbox_integer(sexp_length(tmp2));
    top += (i-2);
    for ( ; sexp_pairp(tmp2); tmp2=sexp_cdr(tmp2), top--)
      _ARG1 = sexp_car(tmp2);
    top += i+1;
    ip -= sizeof(sexp);
    goto make_call;
  case OP_TAIL_CALL:
    i = sexp_unbox_integer(_WORD0);    /* number of params */
    tmp1 = _ARG1;                              /* procedure to call */
    /* save frame info */
    j = sexp_unbox_integer(stack[fp]);
    ip = ((unsigned char*) sexp_unbox_integer(stack[fp+1])) - sizeof(sexp);
    cp = stack[fp+2];
    /* copy new args into place */
    for (k=0; k<i; k++)
      stack[fp-j+k] = stack[top-1-i+k];
    top = fp+i-j+1;
    fp = sexp_unbox_integer(stack[fp+3]);
    goto make_call;
  case OP_CALL:
    if (top >= INIT_STACK_SIZE)
      sexp_raise("out of stack space", SEXP_NULL);
    i = sexp_unbox_integer(_WORD0);
    tmp1 = _ARG1;
  make_call:
    if (sexp_opcodep(tmp1)) {
      /* compile non-inlined opcode applications on the fly */
      tmp1 = make_opcode_procedure(tmp1, i, env, stack, top);
      if (sexp_exceptionp(tmp1)) {
        _ARG1 = tmp1;
        goto call_error_handler;
      }
    }
    if (! sexp_procedurep(tmp1))
      sexp_raise("non procedure application", sexp_list1(tmp1));
    j = i - sexp_unbox_integer(sexp_procedure_num_args(tmp1));
    if (j < 0)
      sexp_raise("not enough args", sexp_list2(tmp1, sexp_make_integer(i)));
    if (j > 0) {
      if (sexp_procedure_variadic_p(tmp1)) {
        stack[top-i-1] = sexp_cons(stack[top-i-1], SEXP_NULL);
        for (k=top-i; k<top-(i-j)-1; k++)
          stack[top-i-1] = sexp_cons(stack[k], stack[top-i-1]);
        for ( ; k<top; k++)
          stack[k-j+1] = stack[k];
        top -= (j-1);
        i -= (j-1);
      } else {
        sexp_raise("too many args", sexp_list2(tmp1, sexp_make_integer(i)));
      }
    } else if (sexp_procedure_variadic_p(tmp1)) {
      /* shift stack, set extra arg to null */
      for (k=top; k>=top-i; k--)
        stack[k] = stack[k-1];
      stack[top-i-1] = SEXP_NULL;
      top++;
      i++;
    }
    _ARG1 = sexp_make_integer(i);
    stack[top] = sexp_make_integer(ip+sizeof(sexp));
    stack[top+1] = cp;
    stack[top+2] = sexp_make_integer(fp);
    top += 3;
    bc = sexp_procedure_code(tmp1);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
    fp = top-4;
    break;
  case OP_FCALL0:
    _PUSH(((sexp_proc0)_UWORD0)());
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL1:
    _ARG1 = ((sexp_proc1)_UWORD0)(_ARG1);
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL2:
    _ARG2 = ((sexp_proc2)_UWORD0)(_ARG1, _ARG2);
    top--;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL3:
    _ARG3 =((sexp_proc3)_UWORD0)(_ARG1, _ARG2, _ARG3);
    top -= 2;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL4:
    _ARG4 =((sexp_proc4)_UWORD0)(_ARG1, _ARG2, _ARG3, _ARG4);
    top -= 3;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_EVAL:
    sexp_context_top(context) = top;
    _ARG1 = eval_in_context(_ARG1, context);
    sexp_check_exception();
    break;
  case OP_JUMP_UNLESS:
    if (stack[--top] == SEXP_FALSE)
      ip += _SWORD0;
    else
      ip += sizeof(sexp_sint_t);
    break;
  case OP_JUMP:
    ip += _SWORD0;
    break;
  case OP_PUSH:
    _PUSH(_WORD0);
    ip += sizeof(sexp);
    break;
  case OP_DROP:
    top--;
    break;
  case OP_STACK_REF:            /* `pick' in forth */
    stack[top] = stack[top - _SWORD0];
    ip += sizeof(sexp);
    top++;
    break;
  case OP_LOCAL_REF:
    stack[top] = stack[fp - 1 - _SWORD0];
    ip += sizeof(sexp);
    top++;
    break;
  case OP_LOCAL_SET:
    stack[fp - 1 - _SWORD0] = _ARG1;
    _ARG1 = SEXP_UNDEF;
    ip += sizeof(sexp);
    break;
  case OP_CLOSURE_REF:
    _PUSH(sexp_vector_ref(cp, sexp_make_integer(_WORD0)));
    ip += sizeof(sexp);
    break;
  case OP_VECTOR_REF:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-ref: not a vector", sexp_list1(_ARG1));
    _ARG2 = sexp_vector_ref(_ARG1, _ARG2);
    top--;
    break;
  case OP_VECTOR_SET:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-set!: not a vector", sexp_list1(_ARG1));
    sexp_vector_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_UNDEF;
    top-=2;
    break;
  case OP_VECTOR_LENGTH:
    _ARG1 = sexp_make_integer(sexp_vector_length(_ARG1));
    break;
  case OP_STRING_REF:
    _ARG2 = sexp_string_ref(_ARG1, _ARG2);
    top--;
    break;
  case OP_STRING_SET:
    sexp_string_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_UNDEF;
    top-=2;
    break;
  case OP_STRING_LENGTH:
    _ARG1 = sexp_make_integer(sexp_string_length(_ARG1));
    break;
  case OP_MAKE_PROCEDURE:
    _ARG4 = sexp_make_procedure(_ARG1, _ARG2, _ARG3, _ARG4);
    top-=3;
    break;
  case OP_MAKE_VECTOR:
    _ARG2 = sexp_make_vector(_ARG1, _ARG2);
    top--;
    break;
  case OP_AND:
    _ARG2 = sexp_make_boolean((_ARG1 != SEXP_FALSE) && (_ARG2 != SEXP_FALSE));
    top--;
    break;
  case OP_EOFP:
    _ARG1 = sexp_make_boolean(_ARG1 == SEXP_EOF); break;
  case OP_NULLP:
    _ARG1 = sexp_make_boolean(sexp_nullp(_ARG1)); break;
  case OP_INTEGERP:
    _ARG1 = sexp_make_boolean(sexp_integerp(_ARG1)); break;
  case OP_SYMBOLP:
    _ARG1 = sexp_make_boolean(sexp_symbolp(_ARG1)); break;
  case OP_CHARP:
    _ARG1 = sexp_make_boolean(sexp_charp(_ARG1)); break;
  case OP_TYPEP:
    _ARG1 = sexp_make_boolean(sexp_pointerp(_ARG1)
                              && (sexp_pointer_tag(_ARG1)
                                  == _UWORD0));
    ip += sizeof(sexp);
    break;
  case OP_CAR:
    if (! sexp_pairp(_ARG1)) sexp_raise("car: not a pair", sexp_list1(_ARG1));
    _ARG1 = sexp_car(_ARG1); break;
  case OP_CDR:
    if (! sexp_pairp(_ARG1)) sexp_raise("cdr: not a pair", sexp_list1(_ARG1));
    _ARG1 = sexp_cdr(_ARG1); break;
  case OP_SET_CAR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-car!: not a pair", sexp_list1(_ARG1));
    sexp_car(_ARG1) = _ARG2;
    _ARG2 = SEXP_UNDEF;
    top--;
    break;
  case OP_SET_CDR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-cdr!: not a pair", sexp_list1(_ARG1));
    sexp_cdr(_ARG1) = _ARG2;
    _ARG2 = SEXP_UNDEF;
    top--;
    break;
  case OP_CONS:
    _ARG2 = sexp_cons(_ARG1, _ARG2);
    top--;
    break;
  case OP_ADD:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_add(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_add(_ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fp_add(_ARG1, sexp_integer_to_flonum(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_add(sexp_integer_to_flonum(_ARG1), _ARG2);
#endif
    else sexp_raise("+: not a number", sexp_list2(_ARG1, _ARG2));
    top--;
    break;
  case OP_SUB:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_sub(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_sub(_ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fp_sub(_ARG1, sexp_integer_to_flonum(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_sub(sexp_integer_to_flonum(_ARG1), _ARG2);
#endif
    else sexp_raise("-: not a number", sexp_list2(_ARG1, _ARG2));
    top--;
    break;
  case OP_MUL:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_mul(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_mul(_ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fp_mul(_ARG1, sexp_integer_to_flonum(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_mul(sexp_integer_to_flonum(_ARG1), _ARG2);
#endif
    else sexp_raise("*: not a number", sexp_list2(_ARG1, _ARG2));
    top--;
    break;
  case OP_DIV:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fp_div(sexp_integer_to_flonum(_ARG1),
                          sexp_integer_to_flonum(_ARG2));
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_div(_ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fp_div(_ARG1, sexp_integer_to_flonum(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_div(sexp_integer_to_flonum(_ARG1), _ARG2);
#endif
    else sexp_raise("/: not a number", sexp_list2(_ARG1, _ARG2));
    top--;
    break;
  case OP_QUOT:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2)) {
      _ARG2 = sexp_fx_div(_ARG1, _ARG2);
      top--;
    }
    else sexp_raise("quotient: not a number", sexp_list2(_ARG1, _ARG2));
    break;
  case OP_MOD:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2)) {
      _ARG2 = sexp_fx_mod(_ARG1, _ARG2);
      top--;
    }
    else sexp_raise("modulo: not a number", sexp_list2(_ARG1, _ARG2));
    break;
  case OP_NEG:
    if (sexp_integerp(_ARG1))
      _ARG1 = sexp_make_integer(-sexp_unbox_integer(_ARG1));
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1))
      _ARG1 = sexp_make_flonum(-sexp_flonum_value(_ARG1));
#endif
    else sexp_raise("-: not a number", sexp_list1(_ARG1));
    break;
  case OP_INV:
    if (sexp_integerp(_ARG1))
      _ARG1 = sexp_make_flonum(1/(double)sexp_unbox_integer(_ARG1));
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1))
      _ARG1 = sexp_make_flonum(1/sexp_flonum_value(_ARG1));
#endif
    else sexp_raise("/: not a number", sexp_list1(_ARG1));
    break;
  case OP_LT:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      i = _ARG1 < _ARG2;
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      i = sexp_flonum_value(_ARG1) < sexp_flonum_value(_ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      i = sexp_flonum_value(_ARG1) < (double)sexp_unbox_integer(_ARG2);
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      i = (double)sexp_unbox_integer(_ARG1) < sexp_flonum_value(_ARG2);
#endif
    else sexp_raise("<: not a number", sexp_list2(_ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
    top--;
    break;
  case OP_LE:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      i = _ARG1 <= _ARG2;
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      i = sexp_flonum_value(_ARG1) <= sexp_flonum_value(_ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      i = sexp_flonum_value(_ARG1) <= (double)sexp_unbox_integer(_ARG2);
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      i = (double)sexp_unbox_integer(_ARG1) <= sexp_flonum_value(_ARG2);
#endif
    else sexp_raise("<=: not a number", sexp_list2(_ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
    top--;
    break;
  case OP_EQN:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      i = _ARG1 == _ARG2;
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      i = sexp_flonum_value(_ARG1) == sexp_flonum_value(_ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      i = sexp_flonum_value(_ARG1) == (double)sexp_unbox_integer(_ARG2);
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      i = (double)sexp_unbox_integer(_ARG1) == sexp_flonum_value(_ARG2);
#endif
    else sexp_raise("=: not a number", sexp_list2(_ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
    top--;
    break;
  case OP_EQ:
    _ARG2 = sexp_make_boolean(_ARG1 == _ARG2);
    top--;
    break;
  case OP_FIX2FLO:
    if (sexp_integerp(_ARG1))
      _ARG1 = sexp_integer_to_flonum(_ARG1);
    else
#if USE_FLONUMS
      if (! sexp_flonump(_ARG1))
#endif
        sexp_raise("exact->inexact: not a number", sexp_list1(_ARG1));
    break;
  case OP_FLO2FIX:
#if USE_FLONUMS
    if (sexp_flonump(_ARG1))
      _ARG1 = sexp_make_integer((sexp_sint_t)sexp_flonum_value(_ARG1));
    else
#endif
      if (! sexp_integerp(_ARG1))
        sexp_raise("inexact->exact: not a number", sexp_list1(_ARG1));
    break;
  case OP_CHAR2INT:
    _ARG1 = sexp_make_integer(sexp_unbox_character(_ARG1));
    break;
  case OP_INT2CHAR:
    _ARG1 = sexp_make_character(sexp_unbox_integer(_ARG1));
    break;
  case OP_CHAR_UPCASE:
    _ARG1 = sexp_make_character(toupper(sexp_unbox_character(_ARG1)));
    break;
  case OP_CHAR_DOWNCASE:
    _ARG1 = sexp_make_character(tolower(sexp_unbox_character(_ARG1)));
    break;
  case OP_DISPLAY:
    if (sexp_stringp(_ARG1)) {
      sexp_write_string(sexp_string_data(_ARG1), _ARG2);
      _ARG2 = SEXP_UNDEF;
      top--;
      break;
    } else if (sexp_charp(_ARG1)) {
      sexp_write_char(sexp_unbox_character(_ARG1), _ARG2);
      _ARG2 = SEXP_UNDEF;
      top--;
      break;
    }
    /* ... FALLTHROUGH ... */
  case OP_WRITE:
    sexp_write(_ARG1, _ARG2);
    _ARG2 = SEXP_UNDEF;
    top--;
    break;
  case OP_WRITE_CHAR:
    sexp_write_char(sexp_unbox_character(_ARG1), _ARG2);
    _ARG2 = SEXP_UNDEF;
    top--;
    break;
  case OP_NEWLINE:
    sexp_write_char('\n', _ARG1);
    _ARG1 = SEXP_UNDEF;
    break;
  case OP_FLUSH_OUTPUT:
    sexp_flush(_ARG1);
    _ARG1 = SEXP_UNDEF;
    break;
  case OP_READ:
    _ARG1 = sexp_read(_ARG1);
    sexp_check_exception();
    break;
  case OP_READ_CHAR:
    i = sexp_read_char(_ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case OP_PEEK_CHAR:
    i = sexp_read_char(_ARG1);
    sexp_push_char(i, _ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case OP_RET:
    i = sexp_unbox_integer(stack[fp]);
    stack[fp-i] = _ARG1;
    top = fp-i+1;
    ip = (unsigned char*) sexp_unbox_integer(stack[fp+1]);
    cp = stack[fp+2];
    fp = sexp_unbox_integer(stack[fp+3]);
    break;
  case OP_DONE:
    goto end_loop;
  default:
    sexp_raise("unknown opcode", sexp_list1(sexp_make_integer(*(ip-1))));
  }
  goto loop;

 end_loop:
  return _ARG1;
}

/************************ library procedures **************************/

sexp sexp_open_input_file (sexp path) {
  return sexp_make_input_port(fopen(sexp_string_data(path), "r"));
}

sexp sexp_open_output_file (sexp path) {
  return sexp_make_input_port(fopen(sexp_string_data(path), "w"));
}

sexp sexp_close_port (sexp port) {
  fclose(sexp_port_stream(port));
  return SEXP_UNDEF;
}

sexp sexp_load (sexp source, sexp env) {
  sexp obj, res, in, context = sexp_make_context(NULL, env);
  in = sexp_open_input_file(source);
  while ((obj=sexp_read(in)) != (sexp) SEXP_EOF) {
    res = eval_in_context(obj, context);
    if (sexp_exceptionp(res))
      break;
  }
  if (obj == SEXP_EOF)
    res = SEXP_UNDEF;
  sexp_close_port(in);
  return res;
}

#if USE_MATH

static sexp sexp_math_exception (char *message, sexp obj) {
  return sexp_make_exception(sexp_intern("type-error"),
                             sexp_make_string(message),
                             sexp_list1(obj), SEXP_FALSE, SEXP_FALSE);
}

#define define_math_op(name, cname)       \
  static sexp name (sexp z) {             \
    double d;                             \
    if (sexp_flonump(z))                  \
      d = sexp_flonum_value(z);           \
    else if (sexp_integerp(z))            \
      d = (double)sexp_unbox_integer(z);  \
    else                                  \
      return sexp_math_exception("not a number", z); \
    return sexp_make_flonum(cname(d));    \
  }

define_math_op(sexp_exp, exp)
define_math_op(sexp_log, log)
define_math_op(sexp_sin, sin)
define_math_op(sexp_cos, cos)
define_math_op(sexp_tan, tan)
define_math_op(sexp_asin, asin)
define_math_op(sexp_acos, acos)
define_math_op(sexp_atan, atan)
define_math_op(sexp_sqrt, sqrt)
define_math_op(sexp_round, round)
define_math_op(sexp_trunc, trunc)
define_math_op(sexp_floor, floor)
define_math_op(sexp_ceiling, ceil)

#endif

static sexp sexp_expt (sexp x, sexp e) {
  double res, x1, e1;
  if (sexp_integerp(x))
    x1 = (double)sexp_unbox_integer(x);
#if USE_FLONUMS
  else if (sexp_flonump(x))
    x1 = sexp_flonum_value(x);
#endif
  else
    return sexp_math_exception("not a number", x);
  if (sexp_integerp(e))
    e1 = (double)sexp_unbox_integer(e);
#if USE_FLONUMS
  else if (sexp_flonump(e))
    e1 = sexp_flonum_value(e);
#endif
  else
    return sexp_math_exception("not a number", e);
  res = pow(x1, e1);
#if USE_FLONUMS
  if ((res > SEXP_MAX_INT) || sexp_flonump(x) || sexp_flonump(e))
    return sexp_make_flonum(res);
#endif
  return sexp_make_integer((sexp_sint_t)round(res));
}

/*********************** standard environment *************************/

static struct sexp_struct core_forms[] = {
  {.tag=SEXP_CORE, .value={.core={CORE_DEFINE, "define"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_SET, "set!"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_LAMBDA, "lambda"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_IF, "if"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_BEGIN, "begin"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_QUOTE, "quote"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_DEFINE_SYNTAX, "define-syntax"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_LET_SYNTAX, "let-syntax"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_LETREC_SYNTAX, "letrec-syntax"}}},
};

#include "opcodes.c"

static int standard_env_syms_interned_p = 0;

static sexp sexp_make_null_env (sexp version) {
  sexp_uint_t i;
  sexp e = sexp_alloc_type(env, SEXP_ENV);
  sexp_env_parent(e) = NULL;
  sexp_env_bindings(e) = SEXP_NULL;
  for (i=0; i<(sizeof(core_forms)/sizeof(core_forms[0])); i++)
    env_define(e, sexp_intern(sexp_core_name(&core_forms[i])), &core_forms[i]);
  return e;
}

static sexp sexp_make_standard_env (sexp version) {
  sexp_uint_t i;
  sexp e = sexp_make_null_env(version), op, cell, sym;
  for (i=0; i<(sizeof(opcodes)/sizeof(opcodes[0])); i++) {
    op = &opcodes[i];
    if ((! standard_env_syms_interned_p)
        && sexp_opcode_opt_param_p(op)
        && sexp_opcode_data(op)) {
      sym = sexp_intern((char*)sexp_opcode_data(op));
      cell = env_cell_create(e, sym, SEXP_UNDEF);
      sexp_opcode_data(op) = cell;
    }
    env_define(e, sexp_intern(sexp_opcode_name(op)), op);
  }
  env_define(e, the_cur_in_symbol, sexp_make_input_port(stdin));
  env_define(e, the_cur_out_symbol, sexp_make_output_port(stdout));
  env_define(e, the_cur_err_symbol, sexp_make_output_port(stderr));
  env_define(e, the_interaction_env_symbol, e);
  standard_env_syms_interned_p = 1;
  return e;
}

/************************** eval interface ****************************/

sexp apply(sexp proc, sexp args, sexp context) {
  sexp *stack = sexp_context_stack(context), ls;
  sexp_sint_t top = sexp_context_top(context), offset;
  offset = top + sexp_unbox_integer(sexp_length(args));
  for (ls=args; sexp_pairp(ls); ls=sexp_cdr(ls), top++)
    stack[--offset] = sexp_car(ls);
  stack[top] = sexp_make_integer(top);
  top++;
  stack[top++] = sexp_make_integer(sexp_bytecode_data(final_resumer));
  stack[top++] = sexp_make_vector(0, SEXP_UNDEF);
  stack[top++] = sexp_make_integer(0);
  return vm(sexp_procedure_code(proc),
            sexp_procedure_vars(proc),
            context,
            stack,
            top);
}

sexp compile (sexp x, sexp context) {
  sexp ast, ctx;
  analyze_bind(ast, x, context);
  free_vars(ast, SEXP_NULL);    /* should return SEXP_NULL */
  ctx = sexp_make_context(sexp_context_stack(context),
                          sexp_context_env(context));
  generate(ast, ctx);
  return sexp_make_procedure(sexp_make_integer(0),
                             sexp_make_integer(0),
                             finalize_bytecode(ctx),
                             sexp_make_vector(0, SEXP_UNDEF));
}

sexp eval_in_context (sexp obj, sexp context) {
  sexp thunk = compile(obj, context);
  if (sexp_exceptionp(thunk)) {
    sexp_print_exception(thunk, env_global_ref(sexp_context_env(context),
                                               the_cur_err_symbol,
                                               SEXP_FALSE));
    return SEXP_UNDEF;
  }
  return apply(thunk, SEXP_NULL, context);
}

sexp eval (sexp obj, sexp env) {
  sexp context = sexp_make_context(NULL, NULL);
  sexp_context_env(context) = env;
  return eval_in_context(obj, context);
}

void scheme_init () {
  sexp context;
  if (! scheme_initialized_p) {
    scheme_initialized_p = 1;
    sexp_init();
    the_compile_error_symbol = sexp_intern("compile-error");
    the_err_handler_symbol = sexp_intern("*current-error-handler*");
    the_cur_in_symbol = sexp_intern("*current-input-port*");
    the_cur_out_symbol = sexp_intern("*current-output-port*");
    the_cur_err_symbol = sexp_intern("*current-error-port*");
    the_interaction_env_symbol = sexp_intern("*interaction-environment*");
#if USE_BOEHM
    GC_add_roots((char*)&continuation_resumer,
                 ((char*)&continuation_resumer)+sizeof(continuation_resumer)+1);
    GC_add_roots((char*)&final_resumer,
                 ((char*)&final_resumer)+sizeof(continuation_resumer)+1);
    GC_add_roots((char*)&opcodes, ((char*)&opcodes)+sizeof(opcodes)+1);
#endif
    context = sexp_make_context(NULL, NULL);
    emit(OP_RESUMECC, context);
    continuation_resumer = finalize_bytecode(context);
    context = sexp_child_context(context, NULL);
    emit(OP_DONE, context);
    final_resumer = finalize_bytecode(context);
  }
}

void repl (sexp context) {
  sexp obj, res, env, in, out, err;
  env = sexp_context_env(context);
  in = env_global_ref(env, the_cur_in_symbol, SEXP_FALSE);
  out = env_global_ref(env, the_cur_out_symbol, SEXP_FALSE);
  err = env_global_ref(env, the_cur_err_symbol, SEXP_FALSE);
  while (1) {
    sexp_write_string("> ", out);
    sexp_flush(out);
    obj = sexp_read(in);
    if (obj == SEXP_EOF)
      break;
    if (sexp_exceptionp(obj)) {
      sexp_print_exception(obj, err);
    } else {
      res = eval_in_context(obj, context);
      if (res != SEXP_UNDEF) {
        sexp_write(res, out);
        sexp_write_char('\n', out);
      }
    }
  }
}

void run_main (int argc, char **argv) {
  sexp env, obj, out=NULL, res, context, err_handler;
  sexp_uint_t i, quit=0, init_loaded=0;

  env = sexp_make_standard_env(sexp_make_integer(5));
  context = sexp_make_context(NULL, env);
  sexp_context_tailp(context) = 0;
  emit_push(SEXP_UNDEF, context);
  emit(OP_DONE, context);
  err_handler = sexp_make_procedure(sexp_make_integer(0),
                                    sexp_make_integer(0),
                                    finalize_bytecode(context),
                                    sexp_make_vector(0, SEXP_UNDEF));
  env_define(env, the_err_handler_symbol, err_handler);

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'e':
    case 'p':
      if (! init_loaded) {
        sexp_load(sexp_make_string(sexp_init_file), env);
        init_loaded = 1;
      }
      obj = sexp_read_from_string(argv[i+1]);
      res = eval_in_context(obj, context);
      if (argv[i][1] == 'p') {
        if (! out)
          out = env_global_ref(env, the_cur_out_symbol, SEXP_FALSE);
        sexp_write(res, out);
        sexp_write_char('\n', out);
      }
      quit=1;
      i++;
      break;
    case 'q':
      init_loaded = 1;
      break;
    default:
      errx(1, "unknown option: %s", argv[i]);
    }
  }

  if (! quit) {
    if (! init_loaded)
      sexp_load(sexp_make_string(sexp_init_file), env);
    if (i < argc)
      for ( ; i < argc; i++)
        sexp_load(sexp_make_string(argv[i]), env);
    else
      repl(context);
  }
}

int main (int argc, char **argv) {
  scheme_init();
  run_main(argc, argv);
  return 0;
}

