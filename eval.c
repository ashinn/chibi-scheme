/*  eval.c -- evaluator library implementation           */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "eval.h"

/************************************************************************/

static int scheme_initialized_p = 0;

static sexp cur_input_port, cur_output_port, cur_error_port;
static sexp exception_handler_cell;
static sexp continuation_resumer, final_resumer;
static sexp interaction_environment;
static sexp the_compile_error_symbol;

#if USE_DEBUG
#include "debug.c"
#else
#define print_stack(...)
#define print_bytecode(...)
#define disasm(...)
#endif

/*************************** prototypes *******************************/

sexp analyze (sexp x, sexp env);
sexp analyze_lambda (sexp x, sexp env);
sexp analyze_seq (sexp ls, sexp env);
sexp analyze_if (sexp x, sexp env);
sexp analyze_app (sexp x, sexp env);
sexp analyze_define (sexp x, sexp env);
sexp analyze_var_ref (sexp x, sexp env);
sexp analyze_set (sexp x, sexp env);

sexp_sint_t sexp_context_make_label (sexp context);
void sexp_context_patch_label (sexp context, sexp_sint_t label);
void compile_one (sexp x, sexp context);
void compile_lit (sexp value, sexp context);
void compile_seq (sexp app, sexp context);
void compile_cnd (sexp cnd, sexp context);
void compile_ref (sexp ref, sexp context, int unboxp);
void compile_non_global_ref (sexp name, sexp loc, sexp lambda, sexp fv,
                             sexp context, int unboxp);
void compile_set (sexp set, sexp context);
void compile_app (sexp app, sexp context);
void compile_opcode_app (sexp app, sexp context);
void compile_general_app (sexp app, sexp context);
void compile_lambda (sexp lambda, sexp context);

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

/* static int env_global_p (sexp e, sexp id) { */
/*   while (sexp_env_parent(e)) { */
/*     if (sexp_assq(id, sexp_env_bindings(e)) != SEXP_FALSE) */
/*       return 0; */
/*     else */
/*       e = sexp_env_parent(e); */
/*   } */
/*   return 1; */
/* } */

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

static int sexp_param_index (sexp params, sexp name) {
  int i=0;
  while (sexp_pairp(params)) {
    if (sexp_car(params) == name)
      return i;
    params = sexp_cdr(params);
    i++;
  }
  if (params == name)
    return i;
  return -1;
}

/************************* bytecode utilities ***************************/

static void shrink_bcode(sexp context, sexp_uint_t i) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(context)) != i) {
    tmp = sexp_alloc_tagged(sexp_sizeof(bytecode) + i, SEXP_BYTECODE);
    sexp_bytecode_length(tmp) = i;
    memcpy(sexp_bytecode_data(tmp), sexp_bytecode_data(sexp_context_bc(context)), i);
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
  expand_bcode(context, sizeof(sexp));
  *((sexp_uint_t*)(&(sexp_bytecode_data(sexp_context_bc(context))[sexp_context_pos(context)]))) = val;
  sexp_context_pos(context) += sizeof(sexp);
}

static void emit_push(sexp obj, sexp context) {
  emit(OP_PUSH, context);
  emit_word((sexp_uint_t)obj, context);
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

/* static sexp sexp_make_macro (sexp p, sexp e) { */
/*   sexp mac = sexp_alloc_type(macro, SEXP_MACRO); */
/*   sexp_macro_env(mac) = e; */
/*   sexp_macro_proc(mac) = p; */
/*   return mac; */
/* } */

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

static sexp sexp_new_context(sexp *stack) {
  sexp res = sexp_alloc_type(context, SEXP_CONTEXT);
  if (! stack)
    stack = (sexp*) sexp_alloc(sizeof(sexp)*INIT_STACK_SIZE);
  sexp_context_bc(res)
    = sexp_alloc_tagged(sexp_sizeof(bytecode)+INIT_BCODE_SIZE, SEXP_BYTECODE);
  sexp_bytecode_length(sexp_context_bc(res)) = INIT_BCODE_SIZE;
  sexp_context_lambda(res) = SEXP_FALSE;
  sexp_context_stack(res) = stack;
  sexp_context_depth(res) = 0;
  sexp_context_pos(res) = 0;
  return res;
}

static sexp sexp_extend_context(sexp context, sexp lambda) {
  sexp ctx = sexp_new_context(sexp_context_stack(context));
  sexp_context_lambda(ctx) = lambda;
  return ctx;
}

static int sexp_idp (sexp x) {
  while (sexp_synclop(x))
    x = sexp_synclo_expr(x);
  return sexp_symbolp(x);
}

/************************* the compiler ***************************/

static sexp sexp_compile_error(char *message, sexp irritants) {
  return sexp_make_exception(the_compile_error_symbol,
                             sexp_make_string(message),
                             irritants, SEXP_FALSE, SEXP_FALSE);
}

#define analyze_check_exception(x) do {if (sexp_exceptionp(x))       \
                                         return (x);                 \
                                      } while (0)

#define analyze_bind(var, x, env) do {(var) = analyze(x,env);        \
                                      analyze_check_exception(var);  \
                                     } while (0)

sexp analyze (sexp x, sexp env) {
  sexp op, cell, res;
 loop:
  fprintf(stderr, "analyze: ");
  sexp_write(x, cur_error_port);
  fprintf(stderr, "\n");
  if (sexp_pairp(x)) {
    if (sexp_idp(sexp_car(x))) {
      cell = env_cell(env, sexp_car(x));
      if (! cell) return analyze_app(x, env);
      op = sexp_cdr(cell);
      if (sexp_corep(op)) {
        switch (sexp_core_code(op)) {
        case CORE_DEFINE:
          res = analyze_define(x, env);
          break;
        case CORE_SET:
          res = analyze_set(x, env);
          break;
        case CORE_LAMBDA:
          res = analyze_lambda(x, env);
          break;
        case CORE_IF:
          res = analyze_if(x, env);
          break;
        case CORE_BEGIN:
          res = analyze_seq(x, env);
          break;
        case CORE_QUOTE:
          res = sexp_make_lit(sexp_cadr(x));
          break;
        default:
          res = sexp_compile_error("unknown core form", sexp_list1(op));
          break;
        }
      } else if (sexp_macrop(op)) {
        /* x = expand_macro(op, x, env); */
        /* goto loop; */
        res = sexp_compile_error("macros not yet supported", sexp_list1(x));
      } else if (sexp_opcodep(op)) {
        res = analyze_app(sexp_cdr(x), env);
        analyze_check_exception(res);
        sexp_push(res, op);
      } else {
        res = analyze_app(x, env);
      }
    } else {
      res = analyze_app(x, env);
    }
  } else if (sexp_symbolp(x)) {
    res = analyze_var_ref(x, env);
  } else if (sexp_synclop(x)) {
    env = sexp_synclo_env(x);
    x = sexp_synclo_expr(x);
    goto loop;
  } else {
    res = x;
  }
  return res;
}

sexp analyze_lambda (sexp x, sexp env) {
  sexp res, body;
  /* XXXX verify syntax */
  res = sexp_alloc_type(lambda, SEXP_LAMBDA);
  sexp_lambda_params(res) = sexp_cadr(x);
  sexp_lambda_fv(res) = SEXP_NULL;
  sexp_lambda_sv(res) = SEXP_NULL;
  env = extend_env(env, sexp_flatten_dot(sexp_lambda_params(res)), res);
  sexp_env_lambda(env) = res;
  body = analyze_seq(sexp_cddr(x), env);
  analyze_check_exception(body);
  sexp_lambda_body(res) = body;
  return res;
}

sexp analyze_seq (sexp ls, sexp env) {
  sexp res, tmp;
  if (sexp_nullp(ls))
    res = SEXP_UNDEF;
  else if (sexp_nullp(sexp_cdr(ls)))
    res = analyze(sexp_car(ls), env);
  else {
    res = sexp_alloc_type(seq, SEXP_SEQ);
    tmp = analyze_app(ls, env);
    analyze_check_exception(tmp);
    sexp_seq_ls(res) = tmp;
  }
  return res;
}

sexp analyze_if (sexp x, sexp env) {
  sexp test, pass, fail;
  analyze_bind(test, sexp_cadr(x), env);
  analyze_bind(pass, sexp_caddr(x), env);
  analyze_bind(fail, sexp_pairp(sexp_cdddr(x))?sexp_cadddr(x):SEXP_UNDEF, env);
  return sexp_make_cnd(test, pass, fail);
}

sexp analyze_app (sexp x, sexp env) {
  sexp res=SEXP_NULL, tmp;
  for ( ; sexp_pairp(x); x=sexp_cdr(x)) {
    analyze_bind(tmp, sexp_car(x), env);
    sexp_push(res, tmp);
  }
  return sexp_nreverse(res);
}

sexp analyze_define (sexp x, sexp env) {
  sexp ref, name, value;
  name = (sexp_pairp(sexp_cadr(x)) ? sexp_caadr(x) : sexp_cadr(x));
  if (sexp_env_lambda(env) && sexp_lambdap(sexp_env_lambda(env)))
    sexp_push(sexp_lambda_locals(sexp_env_lambda(env)), name);
  if (sexp_pairp(sexp_cadr(x)))
    value = analyze_lambda(sexp_cons(SEXP_UNDEF,
                                     sexp_cons(sexp_cdadr(x), sexp_cddr(x))),
                           env);
  else
    value = analyze(sexp_caddr(x), env);
  analyze_check_exception(value);
  ref = analyze_var_ref(name, env);
  analyze_check_exception(ref);
  env_cell_create(env, name, SEXP_DEF);
  return sexp_make_set(ref, value);
}

sexp analyze_var_ref (sexp x, sexp env) {
  sexp cell = env_cell_create(env, x, SEXP_UNDEF);
  if (! cell)
    fprintf(stderr, "can't happen, env_cell_create => NULL\n");
  return sexp_make_ref(x, cell);
}

sexp analyze_set (sexp x, sexp env) {
  sexp ref, value;
  ref = analyze_var_ref(sexp_cadr(x), env);
  if (sexp_lambdap(sexp_ref_loc(ref)))
    sexp_insert(sexp_lambda_sv(sexp_ref_loc(ref)), sexp_ref_name(ref));
  analyze_check_exception(ref);
  analyze_bind(value, sexp_caddr(x), env);
  return sexp_make_set(ref, value);
}

sexp_sint_t sexp_context_make_label (sexp context) {
  sexp_sint_t label = sexp_context_pos(context);
  sexp_context_pos(context) += sizeof(sexp_uint_t);
  return label;
}

void sexp_context_patch_label (sexp context, sexp_sint_t label) {
  sexp bc = sexp_context_bc(context);
  unsigned char *data = sexp_bytecode_data(bc)+label;
  *((sexp_sint_t*)data) = sexp_context_pos(context)-label;
}

static sexp finalize_bytecode (sexp context) {
  emit(OP_RET, context);
  shrink_bcode(context, sexp_context_pos(context));
  disasm(sexp_context_bc(context));
  return sexp_context_bc(context);
}

void compile_one (sexp x, sexp context) {
  if (sexp_pointerp(x)) {
    switch (sexp_pointer_tag(x)) {
    case SEXP_PAIR:
      compile_app(x, context);
      break;
    case SEXP_LAMBDA:
      compile_lambda(x, context);
      break;
    case SEXP_CND:
      compile_cnd(x, context);
      break;
    case SEXP_REF:
      compile_ref(x, context, 1);
      break;
    case SEXP_SET:
      compile_set(x, context);
      break;
    case SEXP_SEQ:
      compile_seq(sexp_seq_ls(x), context);
      break;
    case SEXP_LIT:
      compile_lit(sexp_lit_value(x), context);
      break;
    default:
      compile_lit(x, context);
    }
  } else {
    compile_lit(x, context);
  }
}

void compile_lit (sexp value, sexp context) {
  emit_push(value, context);
}

void compile_seq (sexp app, sexp context) {
  sexp head=app, tail=sexp_cdr(app);
  for ( ; sexp_pairp(tail); head=tail, tail=sexp_cdr(tail)) {
    compile_one(sexp_car(head), context);
    emit(OP_DROP, context);
    sexp_context_depth(context)--;
  }
  compile_one(sexp_car(head), context);
}

void compile_cnd (sexp cnd, sexp context) {
  sexp_sint_t label1, label2;
  compile_one(sexp_cnd_test(cnd), context);
  emit(OP_JUMP_UNLESS, context);
  sexp_context_depth(context)--;
  label1 = sexp_context_make_label(context);
  compile_one(sexp_cnd_pass(cnd), context);
  emit(OP_JUMP, context);
  sexp_context_depth(context)--;
  label2 = sexp_context_make_label(context);
  sexp_context_patch_label(context, label1);
  compile_one(sexp_cnd_fail(cnd), context);
  sexp_context_patch_label(context, label2);
}

void compile_ref (sexp ref, sexp context, int unboxp) {
  sexp lam;
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global ref */
    emit_push(sexp_ref_cell(ref), context);
    if (unboxp)
      emit(OP_CDR, context);
  } else {
    lam = sexp_context_lambda(context);
    compile_non_global_ref(sexp_ref_name(ref), sexp_ref_cell(ref), lam,
                           sexp_lambda_fv(lam), context, unboxp);
  }
}

void compile_non_global_ref (sexp name, sexp cell, sexp lambda, sexp fv,
                             sexp context, int unboxp) {
  sexp_uint_t i;
  sexp loc = sexp_cdr(cell);
  sexp_debug("cell: ", cell);
  if (loc == lambda && sexp_lambdap(lambda)) {
    /* local ref */
    sexp_debug("params: ", sexp_lambda_params(lambda));
    emit(OP_LOCAL_REF, context);
    emit_word(sexp_param_index(sexp_lambda_params(lambda), name), context);
  } else {
    /* closure ref */
    for (i=0; sexp_pairp(fv); fv=sexp_cdr(fv), i++)
      if (name == sexp_car(fv) && loc == sexp_cdr(fv))
        break;
    emit(OP_CLOSURE_REF, context);
    emit_word(i, context);
  }
  if (unboxp && (sexp_list_index(sexp_lambda_sv(loc), name) >= 0))
    emit(OP_CDR, context);
  sexp_context_depth(context)++;
}

void compile_set (sexp set, sexp context) {
  sexp ref = sexp_set_var(set);
  /* compile the value */
  compile_one(sexp_set_value(set), context);
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global vars are set directly */
    emit_push(sexp_ref_cell(ref), context);
  } else {
    /* stack or closure mutable vars are boxed */
    compile_ref(ref, context, 0);
  }
  emit(OP_SET_CDR, context);
  sexp_context_depth(context)--;
}

void compile_app (sexp app, sexp context) {
  if (sexp_opcodep(sexp_car(app)))
    compile_opcode_app(app, context);
  else
    compile_general_app(app, context);
}

void compile_opcode_app (sexp app, sexp context) {
  sexp ls, op = sexp_car(app);
  sexp_sint_t i, num_args = sexp_unbox_integer(sexp_length(sexp_cdr(app)));

  /* maybe push the default for an optional argument */
  if ((num_args == sexp_opcode_num_args(op))
      && sexp_opcode_variadic_p(op) && sexp_opcode_data(op)) {
    emit(OP_PARAMETER, context);
    emit_word((sexp_uint_t)sexp_opcode_data(op), context);
    if (! sexp_opcode_opt_param_p(op)) {
      emit(OP_CALL, context);
      emit_word((sexp_uint_t)sexp_make_integer(0), context);
    }
    sexp_context_depth(context)++;
    num_args++;
  }

  /* push the arguments onto the stack */
  ls = (sexp_opcode_inverse(op)
        && ! sexp_opcode_class(op) == OPC_ARITHMETIC_INV)
    ? sexp_cdr(app) : sexp_reverse(sexp_cdr(app));
  for ( ; sexp_pairp(ls); ls = sexp_cdr(ls))
    compile_one(sexp_car(ls), context);

  /* emit the actual operator call */
  if (sexp_opcode_class(op) == OPC_ARITHMETIC_INV) {
    emit((num_args == 1) ? sexp_opcode_inverse(op)
         : sexp_opcode_code(op), context);
  } else {
    if (sexp_opcode_class(op) == OPC_FOREIGN)
      /* push the funtion pointer for foreign calls */
      emit_push(sexp_opcode_data(op), context);
    emit(sexp_opcode_inverse(op) ? sexp_opcode_inverse(op)
         : sexp_opcode_code(op),
         context);
  }

  /* emit optional folding of operator */
  if (num_args > 2) {
    if (sexp_opcode_class(op) == OPC_ARITHMETIC
        || sexp_opcode_class(op) == OPC_ARITHMETIC_INV) {
      for (i=num_args-2; i>0; i--)
        emit(sexp_opcode_code(op), context);
    } else if (sexp_opcode_class(op) == OPC_ARITHMETIC_CMP) {
      /* XXXX handle folding of comparisons */
    }
  }

  if (sexp_opcode_class(op) == OPC_PARAMETER)
    emit_word((sexp_uint_t)sexp_opcode_data(op), context);

  sexp_context_depth(context) -= (num_args-1);
}

void compile_general_app (sexp app, sexp context) {
  sexp ls;
  sexp_uint_t len = sexp_unbox_integer(sexp_length(sexp_cdr(app)));

  /* push the arguments onto the stack */
  for (ls = sexp_reverse(sexp_cdr(app)); sexp_pairp(ls); ls = sexp_cdr(ls))
    compile_one(sexp_car(ls), context);

  /* push the operator onto the stack */
  compile_one(sexp_car(app), context);

  /* maybe overwrite the current frame */
  if (sexp_context_tailp(context)) {
    emit(OP_TAIL_CALL, context);
    emit_word(sexp_context_depth(context), context);
    emit_word((sexp_uint_t)sexp_make_integer(len), context);
  } else {
    /* normal call */
    emit(OP_CALL, context);
    emit_word((sexp_uint_t)sexp_make_integer(len), context);
  }

  sexp_context_depth(context) -= len;
}

void compile_lambda (sexp lambda, sexp context) {
  sexp fv, ls, ctx, flags, bc, len, ref, vec, prev_lambda, prev_fv;
  sexp_uint_t k;
  prev_lambda = sexp_context_lambda(context);
  prev_fv = sexp_lambdap(prev_lambda) ? sexp_lambda_fv(prev_lambda) : SEXP_NULL;
  fv = sexp_lambda_fv(lambda);
  ctx = sexp_new_context(sexp_context_stack(context));
  sexp_context_lambda(ctx) = lambda;
  /* box mutable vars */
  for (ls=sexp_lambda_sv(lambda); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    k = sexp_param_index(sexp_lambda_params(lambda), sexp_car(ls));
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
  compile_one(sexp_lambda_body(lambda), ctx);
  flags = sexp_make_integer(sexp_listp(sexp_lambda_params(lambda)) ? 0 : 1);
  len = sexp_length(sexp_lambda_params(lambda));
  bc = finalize_bytecode(ctx);
  if (sexp_nullp(fv)) {
    /* shortcut, no free vars */
    vec = sexp_make_vector(sexp_make_integer(0), SEXP_UNDEF);
    compile_lit(sexp_make_procedure(flags, len, bc, vec), context);
  } else {
    /* push the closed vars */
    emit_push(SEXP_UNDEF, context);
    emit_push(sexp_length(fv), context);
    emit(OP_MAKE_VECTOR, context);
    sexp_context_depth(context)--;
    for (k=0; sexp_pairp(fv); fv=sexp_cdr(fv), k++) {
      ref = sexp_car(fv);
      compile_non_global_ref(sexp_ref_name(ref), sexp_ref_cell(ref),
                             prev_lambda, prev_fv, context, 0);
      emit_push(sexp_make_integer(k), context);
      emit(OP_LOCAL_REF, context);
      emit_word(-5, context);
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

sexp insert_free_var (sexp x, sexp fv) {
  sexp name=sexp_ref_name(x), loc=sexp_ref_loc(x), ls;
  for (ls=fv; sexp_pairp(ls); ls=sexp_cdr(ls))
    if ((name == sexp_ref_name(sexp_car(ls)))
        && (loc == sexp_ref_loc(sexp_car(ls))))
      return fv;
  return sexp_cons(x, fv);
}

sexp union_free_vars (sexp fv1, sexp fv2) {
  if (sexp_nullp(fv2))
    return fv1;
  for ( ; sexp_pairp(fv1); fv1=sexp_cdr(fv1))
    fv2 = insert_free_var(sexp_car(fv1), fv2);
  return fv2;
}

sexp diff_free_vars (sexp fv, sexp params) {
  sexp res = SEXP_NULL;
/*   sexp_debug("diff-free-vars: ", fv); */
/*   sexp_debug("params: ", params); */
  for ( ; sexp_pairp(fv); fv=sexp_cdr(fv))
    if (sexp_param_index(params, sexp_ref_name(sexp_car(fv))) < 0)
      sexp_push(res, sexp_car(fv));
/*   sexp_debug(" => ", res); */
  return res;
}

sexp free_vars (sexp x, sexp fv) {
  sexp fv1, fv2;
  if (sexp_lambdap(x)) {
    fv1 = free_vars(sexp_lambda_body(x), SEXP_NULL);
    fv2 = diff_free_vars(fv1, sexp_lambda_params(x));
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
  }
  return fv;
}

sexp make_param_list(sexp_uint_t i) {
  sexp res = SEXP_NULL;
  char sym[2]="a";
  for (sym[0]+=i; i>0; i--) {
    sym[0] = sym[0]-1;
    res = sexp_cons(sexp_intern(sym), res);
  }
  return res;
}

sexp make_opcode_procedure(sexp op, sexp_uint_t i, sexp e) {
/*   sexp bc, params, res; */
/*   sexp_uint_t pos=0, d=0; */
/*   if (i == sexp_opcode_num_args(op) && sexp_opcode_proc(op)) */
/*     return sexp_opcode_proc(op); */
/*   bc = sexp_alloc_tagged(sexp_sizeof(bytecode)+INIT_BCODE_SIZE, SEXP_BYTECODE); */
/*   params = make_param_list(i); */
/*   e = extend_env(e, params, SEXP_UNDEF); */
/*   sexp_bytecode_length(bc) = INIT_BCODE_SIZE; */
/*   analyze_opcode(op, sexp_cons(op, params), &bc, &pos, e, params, */
/*                  SEXP_NULL, SEXP_NULL, &d, 0); */
/*   emit(&bc, &pos, OP_RET); */
/*   shrink_bcode(&bc, pos); */
/*   /\* disasm(bc); *\/ */
/*   res = sexp_make_procedure(sexp_make_integer(0), sexp_make_integer(i), bc, SEXP_UNDEF); */
/*   if (i == sexp_opcode_num_args(op)) */
/*     sexp_opcode_proc(op) = res; */
/*   return res; */
  return SEXP_UNDEF;
}

/*********************** the virtual machine **************************/

sexp sexp_save_stack(sexp *stack, sexp_uint_t to) {
  sexp res, *data;
  sexp_uint_t i;
  res = sexp_make_vector(sexp_make_integer(to), SEXP_UNDEF);
  data = sexp_vector_data(res);
  for (i=0; i<to; i++)
    data[i] = stack[i];
  return res;
}

sexp_uint_t sexp_restore_stack(sexp saved, sexp *current) {
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
#define _POP() (stack[--top])

#define sexp_raise(msg, args) do {stack[top]=sexp_compile_error(msg, args); top++; goto call_error_handler;} while (0)

sexp vm(sexp bc, sexp cp, sexp e, sexp* stack, sexp_sint_t top) {
  unsigned char *ip=sexp_bytecode_data(bc);
  sexp tmp1, tmp2;
  sexp_sint_t i, j, k, fp=top-4;

 loop:
  print_stack(stack, top, fp);
  fprintf(stderr, "OP: %s (%d)\n", (*ip<=71) ? reverse_opcode_names[*ip] : "<unknown>", *ip);
  switch (*ip++) {
  case OP_NOOP:
    fprintf(stderr, "noop\n");
    break;
  case OP_LOCAL_REF:
    stack[top] = stack[fp - 1 - (sexp_sint_t) ((sexp*)ip)[0]];
    ip += sizeof(sexp);
    top++;
    break;
  case OP_LOCAL_SET:
    stack[fp - 1 - (sexp_sint_t) ((sexp*)ip)[0]] = _ARG1;
    _ARG1 = SEXP_UNDEF;
    ip += sizeof(sexp);
    break;
  case OP_CLOSURE_REF:
    _PUSH(sexp_vector_ref(cp, ((sexp*)ip)[0]));
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
  case OP_STRING_REF:
    _ARG2 = sexp_string_ref(_ARG1, _ARG2);
    top--;
    break;
  case OP_STRING_SET:
    sexp_string_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_UNDEF;
    top-=2;
    break;
  case OP_MAKE_PROCEDURE:
    _ARG4 = sexp_make_procedure(_ARG1, _ARG2, _ARG3, _ARG4);
    top-=3;
    break;
  case OP_MAKE_VECTOR:
    _ARG2 = sexp_make_vector(_ARG1, _ARG2);
    top--;
    break;
  case OP_PUSH:
    _PUSH(((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_DROP:
    top--;
    break;
  case OP_PARAMETER:
    _PUSH(*(sexp*)((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_PAIRP:
    _ARG1 = sexp_make_boolean(sexp_pairp(_ARG1)); break;
  case OP_NULLP:
    _ARG1 = sexp_make_boolean(sexp_nullp(_ARG1)); break;
  case OP_CHARP:
    _ARG1 = sexp_make_boolean(sexp_charp(_ARG1)); break;
  case OP_INTEGERP:
    _ARG1 = sexp_make_boolean(sexp_integerp(_ARG1)); break;
  case OP_SYMBOLP:
    _ARG1 = sexp_make_boolean(sexp_symbolp(_ARG1)); break;
  case OP_STRINGP:
    _ARG1 = sexp_make_boolean(sexp_stringp(_ARG1)); break;
  case OP_VECTORP:
    _ARG1 = sexp_make_boolean(sexp_vectorp(_ARG1)); break;
  case OP_PROCEDUREP:
    _ARG1 = sexp_make_boolean(sexp_procedurep(_ARG1)); break;
  case OP_IPORTP:
    _ARG1 = sexp_make_boolean(sexp_iportp(_ARG1)); break;
  case OP_OPORTP:
    _ARG1 = sexp_make_boolean(sexp_oportp(_ARG1)); break;
  case OP_EOFP:
    _ARG1 = sexp_make_boolean(_ARG1 == SEXP_EOF); break;
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
  case OP_LT:
    _ARG2 = sexp_make_boolean(_ARG1 < _ARG2);
    top--;
    break;
  case OP_LE:
    _ARG2 = sexp_make_boolean(_ARG1 <= _ARG2);
    top--;
    break;
  case OP_GT:
    _ARG2 = sexp_make_boolean(_ARG1 > _ARG2);
    top--;
    break;
  case OP_GE:
    _ARG2 = sexp_make_boolean(_ARG1 >= _ARG2);
    top--;
    break;
  case OP_EQ:
  case OP_EQN:
    _ARG2 = sexp_make_boolean(_ARG1 == _ARG2);
    top--;
    break;
  case OP_TAIL_CALL:
    /* old-args ... n ret-ip ret-cp new-args ...   proc  */
    /* [================= j ===========================] */
    /*                              [==== i =====]       */
    j = sexp_unbox_integer(((sexp*)ip)[0]);    /* current depth */
    i = sexp_unbox_integer(((sexp*)ip)[1]);    /* number of params */
    tmp1 = _ARG1;                       /* procedure to call */
    /* save frame info */
    ip = ((unsigned char*) sexp_unbox_integer(stack[top-i-3])) - sizeof(sexp);
    cp = stack[top-i-2];
    /* copy new args into place */
    for (k=0; k<i; k++)
      stack[top-j+k] = stack[top-i-1+k];
    top -= (j-i-1);
    goto make_call;
  case OP_CALL:
    if (top >= INIT_STACK_SIZE)
      sexp_raise("out of stack space", SEXP_NULL);
    i = sexp_unbox_integer(((sexp*)ip)[0]);
    tmp1 = _ARG1;
  make_call:
    if (sexp_opcodep(tmp1)) {
      /* compile non-inlined opcode applications on the fly */
      tmp1 = make_opcode_procedure(tmp1, i, e);
      if (sexp_exceptionp(tmp1)) {
        _ARG1 = tmp1;
        goto call_error_handler;
      }
    }
    if (! sexp_procedurep(tmp1))
      sexp_raise("non procedure application", sexp_list1(tmp1));
    j = i - sexp_unbox_integer(sexp_procedure_num_args(tmp1));
    fprintf(stderr, "arg difference: %ld-%ld = %ld\n", i, sexp_unbox_integer(sexp_procedure_num_args(tmp1)), j);
    if (j < 0)
      sexp_raise("not enough args", sexp_list2(tmp1, sexp_make_integer(i)));
    if (j > 0) {
      if (sexp_procedure_variadic_p(tmp1)) {
        fprintf(stderr, "unrolling args\n");
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
    top+=3;
    bc = sexp_procedure_code(tmp1);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
    fp = top-4;
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
  case OP_CALLCC:
    tmp1 = _ARG1;
    i = 1;
    stack[top] = sexp_make_integer(1);
    stack[top+1] = sexp_make_integer(ip);
    stack[top+2] = cp;
    stack[top+3] = sexp_make_integer(fp);
    _ARG1
      = sexp_make_procedure(sexp_make_integer(0), sexp_make_integer(1),
                            continuation_resumer,
                            sexp_vector(1, sexp_save_stack(stack, top+4)));
    top++;
    ip -= sizeof(sexp);
    goto make_call;
    break;
  case OP_RESUMECC:
    tmp1 = _ARG5;
    top = sexp_restore_stack(sexp_vector_ref(cp, 0), stack);
    fp = sexp_unbox_integer(_ARG1);
    cp = _ARG2;
    ip = (unsigned char*) sexp_unbox_integer(_ARG3);
    i = sexp_unbox_integer(_ARG4);
    top -= 4;
    _ARG1 = tmp1;
    break;
  case OP_ERROR:
  call_error_handler:
    sexp_print_exception(_ARG1, cur_error_port);
    tmp1 = sexp_cdr(exception_handler_cell);
    stack[top] = (sexp) 1;
    stack[top+1] = sexp_make_integer(ip+4);
    stack[top+2] = cp;
    top+=3;
    bc = sexp_procedure_code(tmp1);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
    break;
  case OP_FCALL0:
    _ARG1 = ((sexp_proc0)_ARG1)();
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_FCALL1:
    _ARG2 = ((sexp_proc1)_ARG1)(_ARG2);
    top--;
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_FCALL2:
    _ARG3 = ((sexp_proc2)_ARG1)(_ARG2, _ARG3);
    top-=2;
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_FCALL3:
    _ARG4 =((sexp_proc3)_ARG1)(_ARG2, _ARG3, _ARG4);
    top-=3;
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_JUMP_UNLESS:
    if (stack[--top] == SEXP_FALSE) {
      ip += ((sexp_sint_t*)ip)[0];
    } else {
      ip += sizeof(sexp_sint_t);
    }
    break;
  case OP_JUMP:
    ip += ((sexp_sint_t*)ip)[0];
    break;
  case OP_DISPLAY:
    if (sexp_stringp(_ARG1)) {
      sexp_write_string(sexp_string_data(_ARG1), _ARG2);
      _ARG2 = SEXP_UNDEF;
      top--;
      break;
    }
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
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_READ_CHAR:
    i = sexp_read_char(_ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case OP_RET:
    fp = sexp_unbox_integer(_ARG2);
    cp = _ARG3;
    ip = (unsigned char*) sexp_unbox_integer(_ARG4);
    i = sexp_unbox_integer(_ARG5);
    stack[top-i-5] = _ARG1;
    top = top-i-4;
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

sexp sexp_load (sexp source) {
  sexp obj, res, context = sexp_new_context(NULL);
  int closep = 0;
  if (sexp_stringp(source)) {
    source = sexp_open_input_file(source);
    closep = 1;
  }
  while ((obj=sexp_read(source)) != (sexp) SEXP_EOF) {
    res = eval_in_context(obj, interaction_environment, context);
    if (sexp_exceptionp(res)) goto done;
  }
  res = SEXP_UNDEF;
 done:
  if (closep) sexp_close_port(source);
  sexp_free(stack);
  return res;
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

static struct sexp_struct opcodes[] = {
#define _OP(c,o,n,m,t,u,i,s,d,p) {.tag=SEXP_OPCODE, .value={.opcode={c, o, n, m, t, u, i, s, d, p}}}
#define _FN(o,n,t,u,s,f) _OP(OPC_FOREIGN, o, n, 0, t, u, 0, s, (sexp)f, NULL)
#define _FN0(s, f) _FN(OP_FCALL0, 0, 0, 0, s, f)
#define _FN1(t, s, f) _FN(OP_FCALL1, 1, t, 0, s, f)
#define _FN2(t, u, s, f) _FN(OP_FCALL2, 2, t, u, s, f)
#define _PARAM(n,a,t) _OP(OPC_PARAMETER, OP_PARAMETER, 0, 1, t, 0, 0, n, a, NULL)
_OP(OPC_ACCESSOR, OP_CAR, 1, 0, SEXP_PAIR, 0, 0, "car", NULL, NULL),
_OP(OPC_ACCESSOR, OP_SET_CAR, 2, 0, SEXP_PAIR, 0, 0, "set-car!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_CDR, 1, 0, SEXP_PAIR, 0, 0, "cdr", NULL, NULL),
_OP(OPC_ACCESSOR, OP_SET_CDR, 2, 0, SEXP_PAIR, 0, 0, "set-cdr!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_VECTOR_REF,2,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-ref", NULL, NULL),
_OP(OPC_ACCESSOR, OP_VECTOR_SET,3,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-set!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_STRING_REF,2,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-ref", NULL, NULL),
_OP(OPC_ACCESSOR, OP_STRING_SET,3,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-set!", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_ADD, 0, 1, SEXP_FIXNUM, 0, 0, "+", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_MUL, 0, 1, SEXP_FIXNUM, 0, 0, "*", NULL, NULL),
_OP(OPC_ARITHMETIC_INV, OP_SUB, 0, 1, SEXP_FIXNUM, 0, OP_NEG, "-", NULL, NULL),
_OP(OPC_ARITHMETIC_INV, OP_DIV, 0, 1, SEXP_FIXNUM, 0, OP_INV, "/", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_QUOT, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, 0, "quotient", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_MOD, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, 0, "modulo", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_LT,  0, 1, SEXP_FIXNUM, 0, 0, "<", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_LE,  0, 1, SEXP_FIXNUM, 0, 0, "<=", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_GT,  0, 1, SEXP_FIXNUM, 0, OP_LE, ">", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_GE,  0, 1, SEXP_FIXNUM, 0, OP_LT, ">=", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_EQ,  0, 1, SEXP_FIXNUM, 0, 0, "=", NULL, NULL),
_OP(OPC_PREDICATE,      OP_EQ,  2, 0, 0, 0, 0, "eq?", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_CONS, 2, 0, 0, 0, 0, "cons", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_MAKE_VECTOR, 2, 0, SEXP_FIXNUM, 0, 0, "make-vector", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_MAKE_PROCEDURE, 4, 0, 0, 0, 0, "make-procedure", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_PAIRP,  1, 0, 0, 0, 0, "pair?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_NULLP,  1, 0, 0, 0, 0, "null?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_STRINGP,  1, 0, 0, 0, 0, "string?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_SYMBOLP,  1, 0, 0, 0, 0, "symbol?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_CHARP,  1, 0, 0, 0, 0, "char?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_VECTORP,  1, 0, 0, 0, 0, "vector?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_PROCEDUREP,  1, 0, 0, 0, 0, "procedure?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_IPORTP,  1, 0, 0, 0, 0, "input-port?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_OPORTP,  1, 0, 0, 0, 0, "output-port?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_EOFP,  1, 0, 0, 0, 0, "eof-object?", NULL, NULL),
_OP(OPC_GENERIC, OP_APPLY1, 2, 0, SEXP_PROCEDURE, SEXP_PAIR, 0, "apply1", NULL, NULL),
_OP(OPC_GENERIC, OP_CALLCC, 1, SEXP_PROCEDURE, 0, 0, 0, "call-with-current-continuation", NULL, NULL),
_OP(OPC_GENERIC, OP_ERROR, 1, SEXP_STRING, 0, 0, 0, "error", NULL, NULL),
_OP(OPC_IO, OP_WRITE, 1, 3, 0, SEXP_OPORT, 0, "write", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_DISPLAY, 1, 3, 0, SEXP_OPORT, 0, "display", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_WRITE_CHAR, 1, 3, 0, SEXP_OPORT, 0, "write-char", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_NEWLINE, 0, 3, 0, SEXP_OPORT, 0, "newline", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_FLUSH_OUTPUT, 0, 3, 0, SEXP_OPORT, 0, "flush-output", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_READ, 0, 3, 0, SEXP_IPORT, 0, "read", (sexp)&cur_input_port, NULL),
_OP(OPC_IO, OP_READ_CHAR, 0, 3, 0, SEXP_IPORT, 0, "read-char", (sexp)&cur_input_port, NULL),
_FN1(SEXP_PAIR, "length", sexp_length),
_FN1(SEXP_PAIR, "reverse", sexp_reverse),
_FN1(SEXP_PAIR, "list->vector", sexp_list_to_vector),
_FN1(SEXP_STRING, "open-input-file", sexp_open_input_file),
_FN1(SEXP_STRING, "open-output-file", sexp_open_output_file),
_FN1(SEXP_IPORT, "close-input-port", sexp_close_port),
_FN1(SEXP_OPORT, "close-output-port", sexp_close_port),
_FN1(0, "load", sexp_load),
_FN2(0, SEXP_PAIR, "memq", sexp_memq),
_FN2(0, SEXP_PAIR, "assq", sexp_assq),
_FN2(SEXP_PAIR, SEXP_PAIR, "diffq", sexp_lset_diff),
_PARAM("current-input-port", (sexp)&cur_input_port, SEXP_IPORT),
_PARAM("current-output-port", (sexp)&cur_output_port, SEXP_OPORT),
_PARAM("current-error-port", (sexp)&cur_error_port, SEXP_OPORT),
_PARAM("interaction-environment", (sexp)&interaction_environment, SEXP_ENV),
#undef _OP
#undef _FN
#undef _FN0
#undef _FN1
#undef _FN2
#undef _PARAM
};

sexp make_standard_env () {
  sexp_uint_t i;
  sexp e = sexp_alloc_type(env, SEXP_ENV);
  sexp_env_parent(e) = NULL;
  sexp_env_bindings(e) = SEXP_NULL;
  for (i=0; i<(sizeof(core_forms)/sizeof(core_forms[0])); i++)
    env_define(e, sexp_intern(sexp_core_name(&core_forms[i])), &core_forms[i]);
  for (i=0; i<(sizeof(opcodes)/sizeof(opcodes[0])); i++)
    env_define(e, sexp_intern(sexp_opcode_name(&opcodes[i])), &opcodes[i]);
  return e;
}

/************************** eval interface ****************************/

/* args ... n ret-ip ret-cp ret-fp */
sexp apply(sexp proc, sexp args, sexp env, sexp context) {
  sexp *stack = sexp_context_stack(context), ls;
  sexp_sint_t top=0;
  for (ls=args; sexp_pairp(ls); ls=sexp_cdr(ls))
    stack[top++] = sexp_car(ls);
  stack[top] = sexp_make_integer(top);
  top++;
  stack[top++] = sexp_make_integer(sexp_bytecode_data(final_resumer));
  stack[top++] = sexp_make_vector(0, SEXP_UNDEF);
  stack[top++] = sexp_make_integer(0);
  return
    vm(sexp_procedure_code(proc), sexp_procedure_vars(proc), env, stack, top);
}

sexp compile (sexp x, sexp env, sexp context) {
  sexp ast, ctx;
  analyze_bind(ast, x, env);
  free_vars(ast, SEXP_NULL);    /* should return SEXP_NULL */
  ctx = sexp_new_context(sexp_context_stack(context));
  compile_one(ast, ctx);
  return sexp_make_procedure(sexp_make_integer(0),
                             sexp_make_integer(0),
                             finalize_bytecode(ctx),
                             sexp_make_vector(0, SEXP_UNDEF));
}

sexp eval_in_context (sexp obj, sexp env, sexp context) {
  sexp thunk = compile(obj, env, context);
  return apply(thunk, SEXP_NULL, env, context);
}

sexp eval (sexp obj, sexp env) {
  sexp context = sexp_new_context(NULL);
  sexp res = eval_in_context(obj, env, context);
  return res;
}

void scheme_init () {
  sexp context;
  if (! scheme_initialized_p) {
    scheme_initialized_p = 1;
    sexp_init();
    cur_input_port = sexp_make_input_port(stdin);
    cur_output_port = sexp_make_output_port(stdout);
    cur_error_port = sexp_make_output_port(stderr);
    the_compile_error_symbol = sexp_intern("compile-error");
    context = sexp_new_context(NULL);
    emit(OP_RESUMECC, context);
    continuation_resumer = finalize_bytecode(context);
    context = sexp_extend_context(context, NULL);
    emit(OP_DONE, context);
    final_resumer = finalize_bytecode(context);
  }
}

void repl (sexp env, sexp context) {
  sexp obj, res;
  while (1) {
    sexp_write_string("> ", cur_output_port);
    sexp_flush(cur_output_port);
    obj = sexp_read(cur_input_port);
    if (obj == SEXP_EOF)
      break;
    res = eval_in_context(obj, env, context);
    if (res != SEXP_UNDEF) {
      sexp_write(res, cur_output_port);
      sexp_write_char('\n', cur_output_port);
    }
  }
}

int main (int argc, char **argv) {
  sexp env, obj, res, context, err_handler, err_handler_sym;
  sexp_uint_t i, quit=0, init_loaded=0;

  scheme_init();
  env = make_standard_env();
  interaction_environment = env;
  context = sexp_new_context(NULL);
  emit_push(SEXP_UNDEF, context);
  emit(OP_DONE, context);
  err_handler = sexp_make_procedure(sexp_make_integer(0),
                                    sexp_make_integer(0),
                                    finalize_bytecode(context),
                                    sexp_make_vector(0, SEXP_UNDEF));
  err_handler_sym = sexp_intern("*error-handler*");
  env_define(env, err_handler_sym, err_handler);
  exception_handler_cell = env_cell(env, err_handler_sym);

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'e':
    case 'p':
      if (! init_loaded) {
        sexp_load(sexp_make_string(sexp_init_file));
        init_loaded = 1;
      }
      obj = sexp_read_from_string(argv[i+1]);
      res = eval_in_context(obj, env, context);
      if (argv[i][1] == 'p') {
        sexp_write(res, cur_output_port);
        sexp_write_char('\n', cur_output_port);
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
      sexp_load(sexp_make_string(sexp_init_file));
    if (i < argc)
      for ( ; i < argc; i++)
        sexp_load(sexp_make_string(argv[i]));
    else
      repl(env, context);
  }
  return 0;
}

