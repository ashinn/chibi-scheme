/*  eval.c -- evaluator library implementation           */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "chibi/eval.h"

/************************************************************************/

static int scheme_initialized_p = 0;

sexp continuation_resumer, final_resumer;
static sexp the_interaction_env_symbol;
static sexp the_err_handler_symbol, the_compile_error_symbol;
static sexp the_cur_in_symbol, the_cur_out_symbol, the_cur_err_symbol;

#define sexp_current_error_port(ctx) env_global_ref(sexp_context_env(ctx),the_cur_out_symbol,SEXP_FALSE)
#define sexp_debug(ctx, msg, obj) (sexp_write_string(ctx, msg, sexp_current_error_port(ctx)), sexp_write(ctx, obj, sexp_current_error_port(ctx)), sexp_write_char(ctx, '\n', sexp_current_error_port(ctx)))

#if USE_DEBUG
#include "debug.c"
#else
#define print_stack(...)
#define print_bytecode(...)
#define sexp_disasm(...)
#endif

static sexp analyze (sexp ctx, sexp x);
static void generate (sexp ctx, sexp x);
static sexp sexp_make_null_env (sexp ctx, sexp version);
static sexp sexp_make_standard_env (sexp ctx, sexp version);

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

static sexp env_cell_create(sexp ctx, sexp e, sexp key, sexp value) {
  sexp_gc_var(ctx, cell, s_cell);
  cell = env_cell(e, key);
  if (! cell) {
    sexp_gc_preserve(ctx, cell, s_cell);
    cell = sexp_cons(ctx, key, value);
    while (sexp_env_parent(e))
      e = sexp_env_parent(e);
    sexp_env_bindings(e) = sexp_cons(ctx, cell, sexp_env_bindings(e));
    sexp_gc_release(ctx, cell, s_cell);
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

static void env_define(sexp ctx, sexp e, sexp key, sexp value) {
  sexp cell = sexp_assq(ctx, key, sexp_env_bindings(e));
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  if (sexp_truep(cell))
    sexp_cdr(cell) = value;
  else {
    tmp = sexp_cons(ctx, key, value);
    sexp_push(ctx, sexp_env_bindings(e), tmp);
  }
  sexp_gc_release(ctx, tmp, s_tmp);
}

static sexp extend_env (sexp ctx, sexp env, sexp vars, sexp value) {
  sexp_gc_var(ctx, e, s_e);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, e, s_e);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  e = sexp_alloc_type(ctx, env, SEXP_ENV);
  sexp_env_parent(e) = env;
  sexp_env_bindings(e) = SEXP_NULL;
  for ( ; sexp_pairp(vars); vars = sexp_cdr(vars)) {
    tmp = sexp_cons(ctx, sexp_car(vars), value);
    sexp_push(ctx, sexp_env_bindings(e), tmp);
  }
  sexp_gc_release(ctx, e, s_e);
  return e;
}

static sexp sexp_chain_env (sexp ctx, sexp env1, sexp env2) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_preserve(ctx, res, s_res);
  res = env2;
  if (env1 && sexp_envp(env1)) {
    res = sexp_alloc_type(ctx, env, SEXP_ENV);
    sexp_env_parent(res) = sexp_chain_env(ctx, sexp_env_parent(env1), env2);
    sexp_env_bindings(res) = sexp_env_bindings(env1);
    sexp_env_lambda(res) = sexp_env_lambda(env1);
  }
  sexp_gc_release(ctx, res, s_res);
  return res;
}

static sexp sexp_reverse_flatten_dot (sexp ctx, sexp ls) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_preserve(ctx, res, s_res);
  for (res=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_push(ctx, res, sexp_car(ls));
  sexp_gc_release(ctx, res, s_res);
  return (sexp_nullp(ls) ? res : sexp_cons(ctx, ls, res));
}

static sexp sexp_flatten_dot (sexp ctx, sexp ls) {
  return sexp_nreverse(ctx, sexp_reverse_flatten_dot(ctx, ls));
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

static void shrink_bcode(sexp ctx, sexp_uint_t i) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(ctx)) != i) {
    tmp = sexp_alloc_tagged(ctx, sexp_sizeof(bytecode) + i, SEXP_BYTECODE);
    sexp_bytecode_name(tmp) = SEXP_FALSE;
    sexp_bytecode_length(tmp) = i;
    sexp_bytecode_literals(tmp)
      = sexp_bytecode_literals(sexp_context_bc(ctx));
    memcpy(sexp_bytecode_data(tmp),
           sexp_bytecode_data(sexp_context_bc(ctx)),
           i);
    sexp_context_bc(ctx) = tmp;
  }
}

static void expand_bcode(sexp ctx, sexp_uint_t size) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(ctx))
      < (sexp_context_pos(ctx))+size) {
    tmp = sexp_alloc_tagged(ctx,
                            sexp_sizeof(bytecode)
                            + sexp_bytecode_length(sexp_context_bc(ctx))*2,
                            SEXP_BYTECODE);
    sexp_bytecode_name(tmp) = SEXP_FALSE;
    sexp_bytecode_length(tmp)
      = sexp_bytecode_length(sexp_context_bc(ctx))*2;
    sexp_bytecode_literals(tmp)
      = sexp_bytecode_literals(sexp_context_bc(ctx));
    memcpy(sexp_bytecode_data(tmp),
           sexp_bytecode_data(sexp_context_bc(ctx)),
           sexp_bytecode_length(sexp_context_bc(ctx)));
    sexp_context_bc(ctx) = tmp;
  }
}

static void emit(sexp ctx, char c)  {
  expand_bcode(ctx, 1);
  sexp_bytecode_data(sexp_context_bc(ctx))[sexp_context_pos(ctx)++] = c;
}

static void emit_word(sexp ctx, sexp_uint_t val)  {
  unsigned char *data;
  expand_bcode(ctx, sizeof(sexp));
  data = sexp_bytecode_data(sexp_context_bc(ctx));
  *((sexp_uint_t*)(&(data[sexp_context_pos(ctx)]))) = val;
  sexp_context_pos(ctx) += sizeof(sexp);
}

static void emit_push(sexp ctx, sexp obj) {
  emit(ctx, OP_PUSH);
  emit_word(ctx, (sexp_uint_t)obj);
  if (sexp_pointerp(obj))
    sexp_push(ctx, sexp_bytecode_literals(sexp_context_bc(ctx)), obj);
}

static sexp sexp_make_procedure(sexp ctx, sexp flags, sexp num_args,
                                sexp bc, sexp vars) {
  sexp proc = sexp_alloc_type(ctx, procedure, SEXP_PROCEDURE);
  sexp_procedure_flags(proc) = (char) (sexp_uint_t) flags;
  sexp_procedure_num_args(proc) = (unsigned short) (sexp_uint_t) num_args;
  sexp_procedure_code(proc) = bc;
  sexp_procedure_vars(proc) = vars;
  return proc;
}

static sexp sexp_make_macro (sexp ctx, sexp p, sexp e) {
  sexp mac = sexp_alloc_type(ctx, macro, SEXP_MACRO);
  sexp_macro_env(mac) = e;
  sexp_macro_proc(mac) = p;
  return mac;
}

static sexp sexp_make_synclo (sexp ctx, sexp env, sexp fv, sexp expr) {
  sexp res;
  if (sexp_synclop(expr))
    return expr;
  res = sexp_alloc_type(ctx, synclo, SEXP_SYNCLO);
  sexp_synclo_env(res) = env;
  sexp_synclo_free_vars(res) = fv;
  sexp_synclo_expr(res) = expr;
  return res;
}

/* internal AST */

static sexp sexp_make_lambda(sexp ctx, sexp params) {
  sexp res = sexp_alloc_type(ctx, lambda, SEXP_LAMBDA);
  sexp_lambda_name(res) = SEXP_FALSE;
  sexp_lambda_params(res) = params;
  sexp_lambda_fv(res) = SEXP_NULL;
  sexp_lambda_sv(res) = SEXP_NULL;
  sexp_lambda_locals(res) = SEXP_NULL;
  sexp_lambda_defs(res) = SEXP_NULL;
  return res;
}

static sexp sexp_make_set(sexp ctx, sexp var, sexp value) {
  sexp res = sexp_alloc_type(ctx, set, SEXP_SET);
  sexp_set_var(res) = var;
  sexp_set_value(res) = value;
  return res;
}

static sexp sexp_make_ref(sexp ctx, sexp name, sexp cell) {
  sexp res = sexp_alloc_type(ctx, ref, SEXP_REF);
  sexp_ref_name(res) = name;
  sexp_ref_cell(res) = cell;
  return res;
}

static sexp sexp_make_cnd(sexp ctx, sexp test, sexp pass, sexp fail) {
  sexp res = sexp_alloc_type(ctx, cnd, SEXP_CND);
  sexp_cnd_test(res) = test;
  sexp_cnd_pass(res) = pass;
  sexp_cnd_fail(res) = fail;
  return res;
}

static sexp sexp_make_lit(sexp ctx, sexp value) {
  sexp res = sexp_alloc_type(ctx, lit, SEXP_LIT);
  sexp_lit_value(res) = value;
  return res;
}

#define SEXP_STACK_SIZE (sexp_sizeof(stack)+sizeof(sexp)*INIT_STACK_SIZE)

sexp sexp_make_context(sexp ctx, sexp stack, sexp env) {
  sexp_gc_var(ctx, res, save_res);
  if (ctx) sexp_gc_preserve(ctx, res, save_res);
  res = sexp_alloc_type(ctx, context, SEXP_CONTEXT);
  if ((! stack) || (stack == SEXP_FALSE)) {
    stack = sexp_alloc_tagged(ctx, SEXP_STACK_SIZE, SEXP_STACK);
    sexp_stack_length(stack) = INIT_STACK_SIZE;
    sexp_stack_top(stack) = 0;
  }
  sexp_context_stack(res) = stack;
  sexp_context_env(res)
    = (env ? env : sexp_make_standard_env(res, sexp_make_integer(5)));
  sexp_context_bc(res)
    = sexp_alloc_tagged(ctx, sexp_sizeof(bytecode)+INIT_BCODE_SIZE,
                        SEXP_BYTECODE);
  sexp_bytecode_name(sexp_context_bc(res)) = SEXP_FALSE;
  sexp_bytecode_length(sexp_context_bc(res)) = INIT_BCODE_SIZE;
  sexp_bytecode_literals(sexp_context_bc(res)) = SEXP_NULL;
  sexp_context_parent(res) = ctx;
  sexp_context_lambda(res) = SEXP_FALSE;
  sexp_context_fv(res) = SEXP_NULL;
  sexp_context_saves(res) = 0;
  sexp_context_depth(res) = 0;
  sexp_context_pos(res) = 0;
  sexp_context_top(res) = 0;
  sexp_context_tailp(res) = 1;
  sexp_context_tracep(res) = 0;
  if (ctx) sexp_gc_release(ctx, res, save_res);
  return res;
}

sexp sexp_make_child_context(sexp context, sexp lambda) {
  sexp ctx = sexp_make_context(context,
                               sexp_context_stack(context),
                               sexp_context_env(context));
  sexp_context_parent(ctx) = context;
  sexp_context_lambda(ctx) = lambda;
  sexp_context_env(ctx) = sexp_context_env(context);
  sexp_context_top(ctx) = sexp_context_top(context);
  sexp_context_fv(ctx) = sexp_context_fv(context);
  sexp_context_tracep(ctx) = sexp_context_tracep(context);
  return ctx;
}

static sexp sexp_identifierp (sexp ctx, sexp x) {
  return sexp_make_boolean(sexp_idp(x));
}

static sexp sexp_syntactic_closure_expr (sexp ctx, sexp x) {
  return (sexp_synclop(x) ? sexp_synclo_expr(x) : x);
}

static sexp sexp_strip_synclos (sexp ctx, sexp x) {
  sexp res;
  sexp_gc_var(ctx, kar, s_kar);
  sexp_gc_var(ctx, kdr, s_kdr);
  sexp_gc_preserve(ctx, kar, s_kar);
  sexp_gc_preserve(ctx, kdr, s_kdr);
 loop:
  if (sexp_synclop(x)) {
    x = sexp_synclo_expr(x);
    goto loop;
  } else if (sexp_pairp(x)) {
    kar = sexp_strip_synclos(ctx, sexp_car(x));
    kdr = sexp_strip_synclos(ctx, sexp_cdr(x));
    res = sexp_cons(ctx, kar, kdr);
    sexp_immutablep(res) = sexp_immutablep(x);
  } else {
    res = x;
  }
  sexp_gc_release(ctx, kar, s_kar);
  return res;
}

static sexp sexp_identifier_eq(sexp ctx, sexp e1, sexp id1, sexp e2, sexp id2) {
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

static sexp sexp_compile_error(sexp ctx, char *message, sexp obj) {
  sexp exn;
  sexp_gc_var(ctx, irritants, s_irr);
  sexp_gc_var(ctx, msg, s_msg);
  sexp_gc_preserve(ctx, irritants, s_irr);
  sexp_gc_preserve(ctx, msg, s_msg);
  irritants = sexp_list1(ctx, obj);
  msg = sexp_c_string(ctx, message, -1);
  exn = sexp_make_exception(ctx, the_compile_error_symbol, msg, irritants,
                            SEXP_FALSE, (sexp_pairp(obj) ?
                                         sexp_pair_source(obj) : SEXP_FALSE));
  sexp_gc_release(ctx, irritants, s_irr);
  return exn;
}

static sexp analyze_app (sexp ctx, sexp x) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, res, s_res);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  for (res=SEXP_NULL; sexp_pairp(x); x=sexp_cdr(x)) {
    sexp_push(ctx, res, SEXP_FALSE);
    tmp = analyze(ctx, sexp_car(x));
    if (sexp_exceptionp(tmp)) {
      res = tmp;
      break;
    } else {
      sexp_car(res) = tmp;
    }
  }
  sexp_gc_release(ctx, res, s_res);
  return (sexp_pairp(res) ? sexp_nreverse(ctx, res) : res);
}

static sexp analyze_seq (sexp ctx, sexp ls) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, res, s_res);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  if (sexp_nullp(ls))
    res = SEXP_VOID;
  else if (sexp_nullp(sexp_cdr(ls)))
    res = analyze(ctx, sexp_car(ls));
  else {
    res = sexp_alloc_type(ctx, seq, SEXP_SEQ);
    tmp = analyze_app(ctx, ls);
    if (sexp_exceptionp(tmp))
      res = tmp;
    else
      sexp_seq_ls(res) = tmp;
  }
  sexp_gc_release(ctx, res, s_res);
  return res;
}

static sexp analyze_var_ref (sexp ctx, sexp x) {
  sexp env = sexp_context_env(ctx), res;
  sexp_gc_var(ctx, cell, s_cell);
  sexp_gc_preserve(ctx, cell, s_cell);
  cell = env_cell(env, x);
  if (! cell) {
    if (sexp_synclop(x)) {
      if (sexp_truep(sexp_memq(ctx, x, sexp_context_fv(ctx))))
        env = sexp_synclo_env(x);
      x = sexp_synclo_expr(x);
    }
    cell = env_cell_create(ctx, env, x, SEXP_UNDEF);
  }
  if (sexp_macrop(sexp_cdr(cell)) || sexp_corep(sexp_cdr(cell)))
    res = sexp_compile_error(ctx, "invalid use of syntax as value", x);
  else 
    res = sexp_make_ref(ctx, x, cell);
  sexp_gc_release(ctx, cell, s_cell);
  return res;
}

static sexp analyze_set (sexp ctx, sexp x) {
  sexp res;
  sexp_gc_var(ctx, ref, s_ref);
  sexp_gc_var(ctx, value, s_value);
  sexp_gc_preserve(ctx, ref, s_ref);
  sexp_gc_preserve(ctx, value, s_value);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x))
         && sexp_nullp(sexp_cdddr(x)) && sexp_idp(sexp_cadr(x)))) {
    res = sexp_compile_error(ctx, "bad set! syntax", x);
  } else {
    ref = analyze_var_ref(ctx, sexp_cadr(x));
    if (sexp_lambdap(sexp_ref_loc(ref)))
      sexp_insert(ctx, sexp_lambda_sv(sexp_ref_loc(ref)), sexp_ref_name(ref));
    value = analyze(ctx, sexp_caddr(x));
    if (sexp_exceptionp(ref))
      res = ref;
    else if (sexp_exceptionp(value))
      res = value;
    else
      res = sexp_make_set(ctx, ref, value);
  }
  sexp_gc_release(ctx, ref, s_ref);
  return res;
}

#define sexp_return(res, val) do {res=val; goto cleanup;} while (0)

static sexp analyze_lambda (sexp ctx, sexp x) {
  sexp name, ls;
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_var(ctx, body, s_body);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_var(ctx, value, s_value);
  sexp_gc_var(ctx, defs, s_defs);
  sexp_gc_var(ctx, ctx2, s_ctx2);
  sexp_gc_preserve(ctx, res, s_res);
  sexp_gc_preserve(ctx, body, s_body);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, value, s_value);
  sexp_gc_preserve(ctx, defs, s_defs);
  sexp_gc_preserve(ctx, ctx2, s_ctx2);
  /* verify syntax */
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x))))
    sexp_return(res, sexp_compile_error(ctx, "bad lambda syntax", x));
  for (ls=sexp_cadr(x); sexp_pairp(ls); ls=sexp_cdr(ls))
    if (! sexp_idp(sexp_car(ls)))
      sexp_return(res, sexp_compile_error(ctx, "non-symbol parameter", x));
    else if (sexp_truep(sexp_memq(ctx, sexp_car(ls), sexp_cdr(ls))))
      sexp_return(res, sexp_compile_error(ctx, "duplicate parameter", x));
  /* build lambda and analyze body */
  res = sexp_make_lambda(ctx, sexp_cadr(x));
  ctx2 = sexp_make_child_context(ctx, res);
  tmp = sexp_flatten_dot(ctx2, sexp_lambda_params(res));
  sexp_context_env(ctx2) = extend_env(ctx2, sexp_context_env(ctx2), tmp, res);
  sexp_env_lambda(sexp_context_env(ctx2)) = res;
  body = analyze_seq(ctx2, sexp_cddr(x));
  if (sexp_exceptionp(body)) sexp_return(res, body);
  /* delayed analyze internal defines */
  defs = SEXP_NULL;
  for (ls=sexp_lambda_defs(res); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    tmp = sexp_car(ls);
    if (sexp_pairp(sexp_cadr(tmp))) {
      name = sexp_caadr(tmp);
      tmp = sexp_cons(ctx2, sexp_cdadr(tmp), sexp_cddr(tmp));
      value = analyze_lambda(ctx2, sexp_cons(ctx2, SEXP_VOID, tmp));
    } else {
      name = sexp_cadr(tmp);
      value = analyze(ctx2, sexp_caddr(tmp));
    }
    if (sexp_exceptionp(value)) sexp_return(res, value);
    sexp_push(ctx2, defs,
              sexp_make_set(ctx2, analyze_var_ref(ctx2, name), value));
  }
  if (sexp_pairp(defs)) {
    if (! sexp_seqp(body)) {
      tmp = sexp_alloc_type(ctx2, seq, SEXP_SEQ);
      sexp_seq_ls(tmp) = sexp_list1(ctx2, body);
      body = tmp;
    }
    sexp_seq_ls(body) = sexp_append2(ctx2, defs, sexp_seq_ls(body));
  }
  sexp_lambda_body(res) = body;
 cleanup:
  sexp_gc_release(ctx, res, s_res);
  return res;
}

static sexp analyze_if (sexp ctx, sexp x) {
  sexp res, fail_expr;
  sexp_gc_var(ctx, test, s_test);
  sexp_gc_var(ctx, pass, s_pass);
  sexp_gc_var(ctx, fail, s_fail);
  sexp_gc_preserve(ctx, test, s_test);
  sexp_gc_preserve(ctx, pass, s_pass);
  sexp_gc_preserve(ctx, fail, s_fail);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x)))) {
    res = sexp_compile_error(ctx, "bad if syntax", x);
  } else {
    test = analyze(ctx, sexp_cadr(x));
    pass = analyze(ctx, sexp_caddr(x));
    fail_expr = sexp_pairp(sexp_cdddr(x)) ? sexp_cadddr(x) : SEXP_VOID;
    fail = analyze(ctx, fail_expr);
    res = (sexp_exceptionp(test) ? test : sexp_exceptionp(pass) ? pass :
           sexp_exceptionp(fail) ? fail : sexp_make_cnd(ctx, test, pass, fail));
  }
  sexp_gc_release(ctx, test, s_test);
  return res;
}

static sexp analyze_define (sexp ctx, sexp x) {
  sexp name, res;
  sexp_gc_var(ctx, ref, s_ref);
  sexp_gc_var(ctx, value, s_value);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_var(ctx, env, s_env);
  sexp_gc_preserve(ctx, ref, s_ref);
  sexp_gc_preserve(ctx, value, s_value);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, env, s_env);
  env = sexp_context_env(ctx);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x)))) {
    res = sexp_compile_error(ctx, "bad define syntax", x);
  } else {
    name = (sexp_pairp(sexp_cadr(x)) ? sexp_caadr(x) : sexp_cadr(x));
    if (! sexp_idp(name)) {
      res = sexp_compile_error(ctx, "can't define a non-symbol", x);
    } else if (sexp_env_lambda(env) && sexp_lambdap(sexp_env_lambda(env))) {
      tmp = sexp_cons(ctx, name, sexp_context_lambda(ctx));
      sexp_push(ctx, sexp_env_bindings(env), tmp);
      sexp_push(ctx, sexp_lambda_sv(sexp_env_lambda(env)), name);
      sexp_push(ctx, sexp_lambda_locals(sexp_env_lambda(env)), name);
      sexp_push(ctx, sexp_lambda_defs(sexp_env_lambda(env)), x);
      res = SEXP_VOID;
    } else {
      env_cell_create(ctx, env, name, SEXP_VOID);
      if (sexp_pairp(sexp_cadr(x))) {
        tmp = sexp_cons(ctx, sexp_cdadr(x), sexp_cddr(x));
        tmp = sexp_cons(ctx, SEXP_VOID, tmp);
        value = analyze_lambda(ctx, tmp);
      } else
        value = analyze(ctx, sexp_caddr(x));
      ref = analyze_var_ref(ctx, name);
      if (sexp_exceptionp(ref))
        res = ref;
      else if (sexp_exceptionp(value))
        res = value;
      else
        res = sexp_make_set(ctx, ref, value);
    }
  }
  sexp_gc_release(ctx, ref, s_ref);
  return res;
}

static sexp analyze_bind_syntax (sexp ls, sexp eval_ctx, sexp bind_ctx) {
  sexp res = SEXP_VOID;
  sexp_gc_var(eval_ctx, proc, s_proc);
  sexp_gc_var(eval_ctx, mac, s_mac);
  sexp_gc_var(eval_ctx, tmp, s_tmp);
  sexp_gc_preserve(eval_ctx, proc, s_proc);
  sexp_gc_preserve(eval_ctx, mac, s_mac);
  sexp_gc_preserve(eval_ctx, tmp, s_tmp);
  for ( ; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    if (! (sexp_pairp(sexp_car(ls)) && sexp_pairp(sexp_cdar(ls))
           && sexp_nullp(sexp_cddar(ls)))) {
      res = sexp_compile_error(eval_ctx, "bad syntax binding", sexp_car(ls));
    } else {
      proc = sexp_eval(eval_ctx, sexp_cadar(ls));
      if (sexp_procedurep(proc)) {
        mac = sexp_make_macro(eval_ctx, proc, sexp_context_env(bind_ctx));
        tmp = sexp_cons(eval_ctx, sexp_caar(ls), mac);
        sexp_push(eval_ctx, sexp_env_bindings(sexp_context_env(bind_ctx)), tmp);
      } else {
        res = (sexp_exceptionp(proc) ? proc
               : sexp_compile_error(eval_ctx, "non-procedure macro:", proc));
        break;
      }
    }
  }
  sexp_gc_release(eval_ctx, proc, s_proc);
  return res;
}

static sexp analyze_define_syntax (sexp ctx, sexp x) {
  sexp res;
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  tmp = sexp_list1(ctx, sexp_cdr(x));
  res = analyze_bind_syntax(tmp, ctx, ctx);
  sexp_gc_release(ctx, tmp, s_tmp);
  return res;
}

static sexp analyze_let_syntax (sexp ctx, sexp x) {
  sexp res;
  sexp_gc_var(ctx, env, s_env);
  sexp_gc_var(ctx, ctx2, s_ctx2);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, env, s_env);
  sexp_gc_preserve(ctx, ctx2, s_ctx2);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x)))) {
    res = sexp_compile_error(ctx, "bad let-syntax", x);
  } else {
    env = sexp_alloc_type(ctx, env, SEXP_ENV);
    sexp_env_parent(env) = sexp_env_parent(sexp_context_env(ctx));
    sexp_env_bindings(env) = sexp_env_bindings(sexp_context_env(ctx));
    ctx2 = sexp_make_child_context(ctx, sexp_context_lambda(ctx));
    sexp_context_env(ctx2) = env;
    tmp = analyze_bind_syntax(sexp_cadr(x), ctx, ctx2);
    res = (sexp_exceptionp(tmp) ? tmp : analyze_seq(ctx2, sexp_cddr(x)));
  }
  sexp_gc_release(ctx, env, s_env);
  return res;
}

static sexp analyze_letrec_syntax (sexp ctx, sexp x) {
  sexp res;
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x)))) {
    res = sexp_compile_error(ctx, "bad letrec-syntax", x);
  } else {
    tmp = analyze_bind_syntax(sexp_cadr(x), ctx, ctx);
    res = (sexp_exceptionp(tmp) ? tmp : analyze_seq(ctx, sexp_cddr(x)));
  }
  sexp_gc_release(ctx, tmp, s_tmp);
  return res;
}

static sexp analyze (sexp ctx, sexp object) {
  sexp op;
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_var(ctx, x, s_x);
  sexp_gc_var(ctx, cell, s_cell);
  sexp_gc_preserve(ctx, res, s_res);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, x, s_x);
  sexp_gc_preserve(ctx, cell, s_cell);
  x = object;
 loop:
  if (sexp_pairp(x)) {
    if (sexp_not(sexp_listp(ctx, x))) {
      res = sexp_compile_error(ctx, "dotted list in source", x);
    } else if (sexp_idp(sexp_car(x))) {
      cell = env_cell(sexp_context_env(ctx), sexp_car(x));
      if (! cell && sexp_synclop(sexp_car(x)))
        cell = env_cell(sexp_synclo_env(sexp_car(x)),
                        sexp_synclo_expr(sexp_car(x)));
      if (! cell) {
        res = analyze_app(ctx, x);
      } else {
        op = sexp_cdr(cell);
        if (sexp_corep(op)) {
          switch (sexp_core_code(op)) {
          case CORE_DEFINE:
            res = analyze_define(ctx, x); break;
          case CORE_SET:
            res = analyze_set(ctx, x); break;
          case CORE_LAMBDA:
            res = analyze_lambda(ctx, x); break;
          case CORE_IF:
            res = analyze_if(ctx, x); break;
          case CORE_BEGIN:
            res = analyze_seq(ctx, sexp_cdr(x)); break;
          case CORE_QUOTE:
            res = sexp_make_lit(ctx, sexp_strip_synclos(ctx, sexp_cadr(x)));
            break;
          case CORE_DEFINE_SYNTAX:
            res = analyze_define_syntax(ctx, x); break;
          case CORE_LET_SYNTAX:
            res = analyze_let_syntax(ctx, x); break;
          case CORE_LETREC_SYNTAX:
            res = analyze_letrec_syntax(ctx, x); break;
          default:
            res = sexp_compile_error(ctx, "unknown core form", op); break;
          }
        } else if (sexp_macrop(op)) {
          tmp = sexp_cons(ctx, sexp_macro_env(op), SEXP_NULL);
          tmp = sexp_cons(ctx, sexp_context_env(ctx), tmp);
          tmp = sexp_cons(ctx, x, tmp);
          x = sexp_make_child_context(ctx, sexp_context_lambda(ctx));
          x = sexp_apply(x, sexp_macro_proc(op), tmp);
          goto loop;
          /* XXXX need to handle free vars, simplify */
/*           tmp = sexp_make_child_context(ctx, sexp_context_lambda(ctx)); */
/*           sexp_context_env(tmp) */
/*             = sexp_chain_env(ctx, sexp_macro_env(op), sexp_context_env(tmp)); */
/*           res = analyze(tmp, x); */
        } else if (sexp_opcodep(op)) {
          res = sexp_length(ctx, sexp_cdr(x));
          if (sexp_unbox_integer(res) < sexp_opcode_num_args(op)) {
            res = sexp_compile_error(ctx, "not enough args for opcode", x);
          } else if ((sexp_unbox_integer(res) > sexp_opcode_num_args(op))
                     && (! sexp_opcode_variadic_p(op))) {
            res = sexp_compile_error(ctx, "too many args for opcode", x);
          } else {
            res = analyze_app(ctx, sexp_cdr(x));
            if (! sexp_exceptionp(res))
              sexp_push(ctx, res, op);
          }
        } else {
          res = analyze_app(ctx, x);
        }
      }
    } else if (sexp_truep(sexp_listp(ctx, sexp_car(x)))
               || (sexp_synclop(sexp_car(x))
                   && sexp_truep(sexp_listp(ctx,
                                            sexp_synclo_expr(sexp_car(x)))))) {
      res = analyze_app(ctx, x);
    } else {
      res = sexp_compile_error(ctx, "invalid operand in application", x);
    }
  } else if (sexp_idp(x)) {
    res = analyze_var_ref(ctx, x);
  } else if (sexp_synclop(x)) {
    ctx = sexp_make_child_context(ctx, sexp_context_lambda(ctx));
    sexp_context_env(ctx) = sexp_synclo_env(x);
    sexp_context_fv(ctx) = sexp_append2(ctx,
                                        sexp_synclo_free_vars(x),
                                        sexp_context_fv(ctx));
    x = sexp_synclo_expr(x);
    goto loop;
  } else {
    res = x;
  }
  sexp_gc_release(ctx, res, s_res);
  return res;
}

static sexp_sint_t sexp_context_make_label (sexp ctx) {
  sexp_sint_t label = sexp_context_pos(ctx);
  sexp_context_pos(ctx) += sizeof(sexp_uint_t);
  return label;
}

static void sexp_context_patch_label (sexp ctx, sexp_sint_t label) {
  sexp bc = sexp_context_bc(ctx);
  unsigned char *data = sexp_bytecode_data(bc)+label;
  *((sexp_sint_t*)data) = sexp_context_pos(ctx)-label;
}

static sexp finalize_bytecode (sexp ctx) {
  emit(ctx, OP_RET);
  shrink_bcode(ctx, sexp_context_pos(ctx));
  return sexp_context_bc(ctx);
}

static void generate_lit (sexp ctx, sexp value) {
  emit_push(ctx, value);
}

static void generate_seq (sexp ctx, sexp app) {
  sexp head=app, tail=sexp_cdr(app);
  sexp_uint_t tailp = sexp_context_tailp(ctx);
  sexp_context_tailp(ctx) = 0;
  for ( ; sexp_pairp(tail); head=tail, tail=sexp_cdr(tail))
    if (sexp_pointerp(sexp_car(head)) && (! sexp_litp(sexp_car(head)))) {
      generate(ctx, sexp_car(head));
      emit(ctx, OP_DROP);
      sexp_context_depth(ctx)--;
    }
  sexp_context_tailp(ctx) = tailp;
  generate(ctx, sexp_car(head));
}

static void generate_cnd (sexp ctx, sexp cnd) {
  sexp_sint_t label1, label2, tailp=sexp_context_tailp(ctx);
  sexp_context_tailp(ctx) = 0;
  generate(ctx, sexp_cnd_test(cnd));
  sexp_context_tailp(ctx) = tailp;
  emit(ctx, OP_JUMP_UNLESS);
  sexp_context_depth(ctx)--;
  label1 = sexp_context_make_label(ctx);
  generate(ctx, sexp_cnd_pass(cnd));
  emit(ctx, OP_JUMP);
  sexp_context_depth(ctx)--;
  label2 = sexp_context_make_label(ctx);
  sexp_context_patch_label(ctx, label1);
  generate(ctx, sexp_cnd_fail(cnd));
  sexp_context_patch_label(ctx, label2);
}

static void generate_non_global_ref (sexp ctx, sexp name, sexp cell,
                                     sexp lambda, sexp fv, int unboxp) {
  sexp_uint_t i;
  sexp loc = sexp_cdr(cell);
  if (loc == lambda && sexp_lambdap(lambda)) {
    /* local ref */
    emit(ctx, OP_LOCAL_REF);
    emit_word(ctx, sexp_param_index(lambda, name));
  } else {
    /* closure ref */
    for (i=0; sexp_pairp(fv); fv=sexp_cdr(fv), i++)
      if ((name == sexp_ref_name(sexp_car(fv)))
          && (loc == sexp_ref_loc(sexp_car(fv))))
        break;
    emit(ctx, OP_CLOSURE_REF);
    emit_word(ctx, i);
  }
  if (unboxp && (sexp_memq(ctx, name, sexp_lambda_sv(loc)) != SEXP_FALSE))
    emit(ctx, OP_CDR);
  sexp_context_depth(ctx)++;
}

static void generate_ref (sexp ctx, sexp ref, int unboxp) {
  sexp lam;
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global ref */
    if (unboxp) {
      emit(ctx,
           (sexp_cdr(sexp_ref_cell(ref)) == SEXP_UNDEF)
           ? OP_GLOBAL_REF : OP_GLOBAL_KNOWN_REF);
      emit_word(ctx, (sexp_uint_t)sexp_ref_cell(ref));
    } else
      emit_push(ctx, sexp_ref_cell(ref));
  } else {
    lam = sexp_context_lambda(ctx);
    generate_non_global_ref(ctx, sexp_ref_name(ref), sexp_ref_cell(ref),
                            lam, sexp_lambda_fv(lam), unboxp);
  }
}

static void generate_set (sexp ctx, sexp set) {
  sexp ref = sexp_set_var(set), lambda;
  /* compile the value */
  sexp_context_tailp(ctx) = 0;
  if (sexp_lambdap(sexp_set_value(set)))
    sexp_lambda_name(sexp_set_value(set)) = sexp_ref_name(ref);
  generate(ctx, sexp_set_value(set));
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global vars are set directly */
    emit_push(ctx, sexp_ref_cell(ref));
    emit(ctx, OP_SET_CDR);
  } else {
    lambda = sexp_ref_loc(ref);
    if (sexp_memq(ctx, sexp_ref_name(ref), sexp_lambda_sv(lambda))
        != SEXP_FALSE) {
      /* stack or closure mutable vars are boxed */
      generate_ref(ctx, ref, 0);
      emit(ctx, OP_SET_CDR);
    } else {
      /* internally defined variable */
      emit(ctx, OP_LOCAL_SET);
      emit_word(ctx, sexp_param_index(lambda, sexp_ref_name(ref)));
    }
  }
  sexp_context_depth(ctx)--;
}

static void generate_opcode_app (sexp ctx, sexp app) {
  sexp op = sexp_car(app);
  sexp_sint_t i, num_args;
  sexp_gc_var(ctx, ls, s_ls);
  sexp_gc_preserve(ctx, ls, s_ls);

  num_args = sexp_unbox_integer(sexp_length(ctx, sexp_cdr(app)));
  sexp_context_tailp(ctx) = 0;

  /* maybe push the default for an optional argument */
  if ((num_args == sexp_opcode_num_args(op))
      && sexp_opcode_variadic_p(op)
      && sexp_opcode_default(op)
      && (sexp_opcode_class(op) != OPC_PARAMETER)) {
    emit_push(ctx, sexp_opcode_default(op));
    if (sexp_opcode_opt_param_p(op))
      emit(ctx, OP_CDR);
    sexp_context_depth(ctx)++;
    num_args++;
  }

  /* push the arguments onto the stack */
  ls = ((sexp_opcode_inverse(op)
         && (sexp_opcode_class(op) != OPC_ARITHMETIC_INV))
        ? sexp_cdr(app) : sexp_reverse(ctx, sexp_cdr(app)));
  for ( ; sexp_pairp(ls); ls = sexp_cdr(ls))
    generate(ctx, sexp_car(ls));

  /* emit the actual operator call */
  switch (sexp_opcode_class(op)) {
  case OPC_ARITHMETIC:
    if (num_args > 1)
      emit(ctx, sexp_opcode_code(op));
    break;
  case OPC_ARITHMETIC_INV:
    emit(ctx, (num_args==1) ? sexp_opcode_inverse(op) : sexp_opcode_code(op));
    break;
  case OPC_ARITHMETIC_CMP:
    if (num_args > 2) {
      emit(ctx, OP_STACK_REF);
      emit_word(ctx, 2);
      emit(ctx, OP_STACK_REF);
      emit_word(ctx, 2);
      emit(ctx, sexp_opcode_code(op));
      emit(ctx, OP_AND);
      for (i=num_args-2; i>0; i--) {
        emit(ctx, OP_STACK_REF);
        emit_word(ctx, 3);
        emit(ctx, OP_STACK_REF);
        emit_word(ctx, 3);
        emit(ctx, sexp_opcode_code(op));
        emit(ctx, OP_AND);
        emit(ctx, OP_AND);
      }
    } else
      emit(ctx, sexp_opcode_code(op));
    break;
  case OPC_FOREIGN:
  case OPC_TYPE_PREDICATE:
    /* push the funtion pointer for foreign calls */
    emit(ctx, sexp_opcode_code(op));
    if (sexp_opcode_data(op))
      emit_word(ctx, (sexp_uint_t)sexp_opcode_data(op));
    break;
  case OPC_PARAMETER:
    emit_push(ctx, sexp_opcode_default(op));
    emit(ctx, ((num_args == 0) ? OP_CDR : OP_SET_CDR));
    break;
  default:
    emit(ctx, sexp_opcode_code(op));
  }

  /* emit optional folding of operator */
  if ((num_args > 2)
      && (sexp_opcode_class(op) == OPC_ARITHMETIC
          || sexp_opcode_class(op) == OPC_ARITHMETIC_INV))
    for (i=num_args-2; i>0; i--)
      emit(ctx, sexp_opcode_code(op));

  sexp_context_depth(ctx) -= (num_args-1);
  sexp_gc_release(ctx, ls, s_ls);
}

static void generate_general_app (sexp ctx, sexp app) {
  sexp_uint_t len = sexp_unbox_integer(sexp_length(ctx, sexp_cdr(app))),
    tailp = sexp_context_tailp(ctx);
  sexp_gc_var(ctx, ls, s_ls);
  sexp_gc_preserve(ctx, ls, s_ls);

  /* push the arguments onto the stack */
  sexp_context_tailp(ctx) = 0;
  for (ls=sexp_reverse(ctx, sexp_cdr(app)); sexp_pairp(ls); ls=sexp_cdr(ls))
    generate(ctx, sexp_car(ls));

  /* push the operator onto the stack */
  generate(ctx, sexp_car(app));

  /* maybe overwrite the current frame */
  emit(ctx, (tailp ? OP_TAIL_CALL : OP_CALL));
  emit_word(ctx, (sexp_uint_t)sexp_make_integer(len));

  sexp_context_depth(ctx) -= len;
  sexp_gc_release(ctx, ls, s_ls);
}

static void generate_app (sexp ctx, sexp app) {
  if (sexp_opcodep(sexp_car(app)))
    generate_opcode_app(ctx, app);
  else
    generate_general_app(ctx, app);
}

static void generate_lambda (sexp ctx, sexp lambda) {
  sexp ctx2, fv, ls, flags, len, ref, prev_lambda, prev_fv;
  sexp_uint_t k;
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_var(ctx, bc, s_bc);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, bc, s_bc);
  prev_lambda = sexp_context_lambda(ctx);
  prev_fv = sexp_lambdap(prev_lambda) ? sexp_lambda_fv(prev_lambda) : SEXP_NULL;
  fv = sexp_lambda_fv(lambda);
  ctx2 = sexp_make_context(ctx, sexp_context_stack(ctx), sexp_context_env(ctx));
  sexp_context_lambda(ctx2) = lambda;
  /* allocate space for local vars */
  for (ls=sexp_lambda_locals(lambda); sexp_pairp(ls); ls=sexp_cdr(ls))
    emit_push(ctx2, SEXP_VOID);
  /* box mutable vars */
  for (ls=sexp_lambda_sv(lambda); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    k = sexp_param_index(lambda, sexp_car(ls));
    if (k >= 0) {
      emit(ctx2, OP_LOCAL_REF);
      emit_word(ctx2, k);
      emit_push(ctx2, sexp_car(ls));
      emit(ctx2, OP_CONS);
      emit(ctx2, OP_LOCAL_SET);
      emit_word(ctx2, k);
      emit(ctx2, OP_DROP);
    }
  }
  sexp_context_tailp(ctx2) = 1;
  generate(ctx2, sexp_lambda_body(lambda));
  flags = sexp_make_integer((sexp_listp(ctx2, sexp_lambda_params(lambda))
                             == SEXP_FALSE) ? 1uL : 0uL);
  len = sexp_length(ctx2, sexp_lambda_params(lambda));
  bc = finalize_bytecode(ctx2);
  sexp_bytecode_name(bc) = sexp_lambda_name(lambda);
  if (sexp_nullp(fv)) {
    /* shortcut, no free vars */
    tmp = sexp_make_vector(ctx2, sexp_make_integer(0), SEXP_VOID);
    tmp = sexp_make_procedure(ctx2, flags, len, bc, tmp);
    sexp_push(ctx, sexp_bytecode_literals(sexp_context_bc(ctx)), tmp);
    generate_lit(ctx, tmp);
  } else {
    /* push the closed vars */
    emit_push(ctx, SEXP_VOID);
    emit_push(ctx, sexp_length(ctx, fv));
    emit(ctx, OP_MAKE_VECTOR);
    sexp_context_depth(ctx)--;
    for (k=0; sexp_pairp(fv); fv=sexp_cdr(fv), k++) {
      ref = sexp_car(fv);
      generate_non_global_ref(ctx, sexp_ref_name(ref), sexp_ref_cell(ref),
                              prev_lambda, prev_fv, 0);
      emit_push(ctx, sexp_make_integer(k));
      emit(ctx, OP_STACK_REF);
      emit_word(ctx, 3);
      emit(ctx, OP_VECTOR_SET);
      emit(ctx, OP_DROP);
      sexp_context_depth(ctx)--;
    }
    /* push the additional procedure info and make the closure */
    emit_push(ctx, bc);
    emit_push(ctx, len);
    emit_push(ctx, flags);
    emit(ctx, OP_MAKE_PROCEDURE);
  }
  sexp_gc_release(ctx, tmp, s_tmp);
}

static void generate (sexp ctx, sexp x) {
  if (sexp_pointerp(x)) {
    switch (sexp_pointer_tag(x)) {
    case SEXP_PAIR:   generate_app(ctx, x); break;
    case SEXP_LAMBDA: generate_lambda(ctx, x); break;
    case SEXP_CND:    generate_cnd(ctx, x); break;
    case SEXP_REF:    generate_ref(ctx, x, 1); break;
    case SEXP_SET:    generate_set(ctx, x); break;
    case SEXP_SEQ:    generate_seq(ctx, sexp_seq_ls(x)); break;
    case SEXP_LIT:    generate_lit(ctx, sexp_lit_value(x)); break;
    default:          generate_lit(ctx, x);
    }
  } else {
    generate_lit(ctx, x);
  }
}

static sexp insert_free_var (sexp ctx, sexp x, sexp fv) {
  sexp name=sexp_ref_name(x), loc=sexp_ref_loc(x), ls;
  for (ls=fv; sexp_pairp(ls); ls=sexp_cdr(ls))
    if ((name == sexp_ref_name(sexp_car(ls)))
        && (loc == sexp_ref_loc(sexp_car(ls))))
      return fv;
  return sexp_cons(ctx, x, fv);
}

static sexp union_free_vars (sexp ctx, sexp fv1, sexp fv2) {
  sexp_gc_var(ctx, res, s_res);
  if (sexp_nullp(fv2))
    return fv1;
  sexp_gc_preserve(ctx, res, s_res);
  for (res=fv2; sexp_pairp(fv1); fv1=sexp_cdr(fv1))
    res = insert_free_var(ctx, sexp_car(fv1), res);
  sexp_gc_release(ctx, res, s_res);
  return res;
}

static sexp diff_free_vars (sexp ctx, sexp lambda, sexp fv, sexp params) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_preserve(ctx, res, s_res);
  res = SEXP_NULL;
  for ( ; sexp_pairp(fv); fv=sexp_cdr(fv))
    if ((sexp_ref_loc(sexp_car(fv)) != lambda)
        || (sexp_memq(NULL, sexp_ref_name(sexp_car(fv)), params)
            == SEXP_FALSE))
      sexp_push(ctx, res, sexp_car(fv));
  sexp_gc_release(ctx, res, s_res);
  return res;
}

static sexp free_vars (sexp ctx, sexp x, sexp fv) {
  sexp_gc_var(ctx, fv1, s_fv1);
  sexp_gc_var(ctx, fv2, s_fv2);
  sexp_gc_preserve(ctx, fv1, s_fv1);
  sexp_gc_preserve(ctx, fv2, s_fv2);
  fv1 = fv;
  if (sexp_lambdap(x)) {
    fv1 = free_vars(ctx, sexp_lambda_body(x), SEXP_NULL);
    fv2 = sexp_flatten_dot(ctx, sexp_lambda_params(x));
    fv2 = sexp_append2(ctx, sexp_lambda_locals(x), fv2);
    fv2 = diff_free_vars(ctx, x, fv1, fv2);
    sexp_lambda_fv(x) = fv2;
    fv1 = union_free_vars(ctx, fv2, fv);
  } else if (sexp_pairp(x)) {
    for ( ; sexp_pairp(x); x=sexp_cdr(x))
      fv1 = free_vars(ctx, sexp_car(x), fv1);
  } else if (sexp_cndp(x)) {
    fv1 = free_vars(ctx, sexp_cnd_test(x), fv);
    fv1 = free_vars(ctx, sexp_cnd_pass(x), fv1);
    fv1 = free_vars(ctx, sexp_cnd_fail(x), fv1);
  } else if (sexp_seqp(x)) {
    for (x=sexp_seq_ls(x); sexp_pairp(x); x=sexp_cdr(x))
      fv1 = free_vars(ctx, sexp_car(x), fv1);
  } else if (sexp_setp(x)) {
    fv1 = free_vars(ctx, sexp_set_value(x), fv);
    fv1 = free_vars(ctx, sexp_set_var(x), fv1);
  } else if (sexp_refp(x) && sexp_lambdap(sexp_ref_loc(x))) {
    fv1 = insert_free_var(ctx, x, fv);
  } else if (sexp_synclop(x)) {
    fv1 = free_vars(ctx, sexp_synclo_expr(x), fv);
  }
  sexp_gc_release(ctx, fv1, s_fv1);
  return fv1;
}

static sexp make_param_list(sexp ctx, sexp_uint_t i) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_preserve(ctx, res, s_res);
  res = SEXP_NULL;
  for ( ; i>0; i--)
    res = sexp_cons(ctx, sexp_make_integer(i), res);
  sexp_gc_release(ctx, res, s_res);
  return res;
}

static sexp make_opcode_procedure (sexp ctx, sexp op, sexp_uint_t i) {
  sexp ls, bc, res, env;
  sexp_gc_var(ctx, params, s_params);
  sexp_gc_var(ctx, ref, s_ref);
  sexp_gc_var(ctx, refs, s_refs);
  sexp_gc_var(ctx, lambda, s_lambda);
  sexp_gc_var(ctx, ctx2, s_ctx2);
  if (i == sexp_opcode_num_args(op) && sexp_opcode_proc(op))
    return sexp_opcode_proc(op); /* return before preserving */
  sexp_gc_preserve(ctx, params, s_params);
  sexp_gc_preserve(ctx, ref, s_ref);
  sexp_gc_preserve(ctx, refs, s_refs);
  sexp_gc_preserve(ctx, lambda, s_lambda);
  sexp_gc_preserve(ctx, ctx2, s_ctx2);
  params = make_param_list(ctx, i);
  lambda = sexp_make_lambda(ctx, params);
  ctx2 = sexp_make_child_context(ctx, lambda);
  env = extend_env(ctx2, sexp_context_env(ctx), params, lambda);
  sexp_context_env(ctx2) = env;
  for (ls=params, refs=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    ref = sexp_make_ref(ctx2, sexp_car(ls), env_cell(env, sexp_car(ls)));
    sexp_push(ctx2, refs, ref);
  }
  refs = sexp_reverse(ctx2, refs);
  refs = sexp_cons(ctx2, op, refs);
  generate_opcode_app(ctx2, refs);
  bc = finalize_bytecode(ctx2);
  sexp_bytecode_name(bc) = sexp_c_string(ctx2, sexp_opcode_name(op), -1);
  res = sexp_make_procedure(ctx2, sexp_make_integer(0), sexp_make_integer(i),
                            bc, SEXP_VOID);
  if (i == sexp_opcode_num_args(op))
    sexp_opcode_proc(op) = res;
  sexp_gc_release(ctx, params, s_params);
  return res;
}

/*********************** the virtual machine **************************/

static sexp sexp_save_stack(sexp ctx, sexp *stack, sexp_uint_t to) {
  sexp res, *data;
  sexp_uint_t i;
  res = sexp_make_vector(ctx, sexp_make_integer(to), SEXP_VOID);
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
#define _ARG6 stack[top-6]
#define _PUSH(x) (stack[top++]=(x))
#define _WORD0 ((sexp*)ip)[0]
#define _UWORD0 ((sexp_uint_t*)ip)[0]
#define _SWORD0 ((sexp_sint_t*)ip)[0]

#define sexp_raise(msg, args)                                       \
  do {sexp_context_top(ctx) = top+1;                                \
      stack[top] = args;                                            \
      stack[top] = sexp_user_exception(ctx, self, msg, stack[top]); \
      top++;                                                        \
      goto call_error_handler;}                                     \
  while (0)

#define sexp_check_exception() do {if (sexp_exceptionp(_ARG1)) \
                                     goto call_error_handler;} \
                                while (0)

sexp sexp_vm (sexp ctx, sexp proc) {
  sexp bc = sexp_procedure_code(proc), cp = sexp_procedure_vars(proc);
  sexp env = sexp_context_env(ctx),
    *stack = sexp_stack_data(sexp_context_stack(ctx));
  unsigned char *ip = sexp_bytecode_data(bc);
  sexp_sint_t i, j, k, fp, top = sexp_stack_top(sexp_context_stack(ctx));
#if USE_BIGNUMS
  sexp_lsint_t prod;
#endif
  fp = top - 4;
  sexp_gc_var(ctx, self, s_self);
  sexp_gc_var(ctx, tmp1, s_tmp1);
  sexp_gc_var(ctx, tmp2, s_tmp2);
  sexp_gc_preserve(ctx, self, s_self);
  sexp_gc_preserve(ctx, tmp1, s_tmp1);
  sexp_gc_preserve(ctx, tmp2, s_tmp2);
  self = proc;

 loop:
#ifdef DEBUG_VM
  if (sexp_context_tracep(ctx)) {
    sexp_print_stack(stack, top, fp,
                     env_global_ref(env, the_cur_err_symbol, SEXP_FALSE));
    fprintf(stderr, "%s\n", (*ip<=71)?reverse_opcode_names[*ip]:"UNKNOWN");
  }
#endif
  switch (*ip++) {
  case OP_NOOP:
    break;
  case OP_RAISE:
  call_error_handler:
    stack[top] = (sexp) 1;
    stack[top+1] = sexp_make_integer(ip-sexp_bytecode_data(bc));
    stack[top+2] = self;
    stack[top+3] = sexp_make_integer(fp);
    top += 4;
    self = env_global_ref(env, the_err_handler_symbol, SEXP_FALSE);
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(self);
    fp = top-4;
    break;
  case OP_RESUMECC:
    tmp1 = stack[fp-1];
    top = sexp_restore_stack(sexp_vector_ref(cp, 0), stack);
    fp = sexp_unbox_integer(_ARG1);
    self = _ARG2;
    bc = sexp_procedure_code(self);
    cp = sexp_procedure_vars(self);
    ip = sexp_bytecode_data(bc) + sexp_unbox_integer(_ARG3);
    i = sexp_unbox_integer(_ARG4);
    top -= 4;
    _ARG1 = tmp1;
    break;
  case OP_CALLCC:
    stack[top] = sexp_make_integer(1);
    stack[top+1] = sexp_make_integer(ip-sexp_bytecode_data(bc));
    stack[top+2] = self;
    stack[top+3] = sexp_make_integer(fp);
    tmp1 = _ARG1;
    i = 1;
    sexp_context_top(ctx) = top;
    tmp2 = sexp_make_vector(ctx, sexp_make_integer(1), SEXP_UNDEF);
    sexp_vector_set(tmp2,
                    sexp_make_integer(0),
                    sexp_save_stack(ctx, stack, top+4));
    _ARG1 = sexp_make_procedure(ctx, sexp_make_integer(0),
                                sexp_make_integer(1), continuation_resumer,
                                tmp2);
    top++;
    ip -= sizeof(sexp);
    goto make_call;
  case OP_APPLY1:
    tmp1 = _ARG1;
    tmp2 = _ARG2;
    i = sexp_unbox_integer(sexp_length(ctx, tmp2));
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
    tmp2 = stack[fp+3];
    j = sexp_unbox_integer(stack[fp]);
    self = stack[fp+2];
    bc = sexp_procedure_vars(self);
    cp = sexp_procedure_vars(self);
    ip = (sexp_bytecode_data(bc)
          + sexp_unbox_integer(stack[fp+1])) - sizeof(sexp);
    /* copy new args into place */
    for (k=0; k<i; k++)
      stack[fp-j+k] = stack[top-1-i+k];
    top = fp+i-j+1;
    fp = sexp_unbox_integer(tmp2);
    goto make_call;
  case OP_CALL:
#if USE_CHECK_STACK
    if (top+16 >= INIT_STACK_SIZE)
      errx(70, "out of stack space at %ld", top);
#endif
    i = sexp_unbox_integer(_WORD0);
    tmp1 = _ARG1;
  make_call:
    if (sexp_opcodep(tmp1)) {
      /* compile non-inlined opcode applications on the fly */
      sexp_context_top(ctx) = top;
      tmp1 = make_opcode_procedure(ctx, tmp1, i);
      if (sexp_exceptionp(tmp1)) {
        _ARG1 = tmp1;
        goto call_error_handler;
      }
    }
    if (! sexp_procedurep(tmp1))
      sexp_raise("non procedure application", sexp_list1(ctx, tmp1));
    j = i - sexp_unbox_integer(sexp_procedure_num_args(tmp1));
    if (j < 0)
      sexp_raise("not enough args",
                 sexp_list2(ctx, tmp1, sexp_make_integer(i)));
    if (j > 0) {
      if (sexp_procedure_variadic_p(tmp1)) {
        stack[top-i-1] = sexp_cons(ctx, stack[top-i-1], SEXP_NULL);
        for (k=top-i; k<top-(i-j)-1; k++)
          stack[top-i-1] = sexp_cons(ctx, stack[k], stack[top-i-1]);
        for ( ; k<top; k++)
          stack[k-j+1] = stack[k];
        top -= (j-1);
        i -= (j-1);
      } else {
        sexp_raise("too many args",
                   sexp_list2(ctx, tmp1, sexp_make_integer(i)));
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
    stack[top] = sexp_make_integer(ip+sizeof(sexp)-sexp_bytecode_data(bc));
    stack[top+1] = self;
    stack[top+2] = sexp_make_integer(fp);
    top += 3;
    self = tmp1;
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(self);
    fp = top-4;
    break;
  case OP_FCALL0:
    sexp_context_top(ctx) = top;
    _PUSH(((sexp_proc1)_UWORD0)(ctx));
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL1:
    sexp_context_top(ctx) = top;
    _ARG1 = ((sexp_proc2)_UWORD0)(ctx, _ARG1);
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL2:
    sexp_context_top(ctx) = top;
    _ARG2 = ((sexp_proc3)_UWORD0)(ctx, _ARG1, _ARG2);
    top--;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL3:
    sexp_context_top(ctx) = top;
    _ARG3 =((sexp_proc4)_UWORD0)(ctx, _ARG1, _ARG2, _ARG3);
    top -= 2;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL4:
    sexp_context_top(ctx) = top;
    _ARG4 =((sexp_proc5)_UWORD0)(ctx, _ARG1, _ARG2, _ARG3, _ARG4);
    top -= 3;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL5:
    sexp_context_top(ctx) = top;
    _ARG5 =((sexp_proc6)_UWORD0)(ctx, _ARG1, _ARG2, _ARG3, _ARG4, _ARG5);
    top -= 4;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_FCALL6:
    sexp_context_top(ctx) = top;
    _ARG6 =((sexp_proc7)_UWORD0)(ctx, _ARG1, _ARG2, _ARG3, _ARG4, _ARG5, _ARG6);
    top -= 5;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case OP_EVAL:
    sexp_context_top(ctx) = top;
    _ARG1 = sexp_eval(ctx, _ARG1);
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
  case OP_GLOBAL_REF:
    if (sexp_cdr(_WORD0) == SEXP_UNDEF)
      sexp_raise("undefined variable", sexp_list1(ctx, sexp_car(_WORD0)));
    /* ... FALLTHROUGH ... */
  case OP_GLOBAL_KNOWN_REF:
    _PUSH(sexp_cdr(_WORD0));
    ip += sizeof(sexp);
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
    _ARG1 = SEXP_VOID;
    ip += sizeof(sexp);
    break;
  case OP_CLOSURE_REF:
    _PUSH(sexp_vector_ref(cp, sexp_make_integer(_WORD0)));
    ip += sizeof(sexp);
    break;
  case OP_VECTOR_REF:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-ref: not a vector", sexp_list1(ctx, _ARG1));
    _ARG2 = sexp_vector_ref(_ARG1, _ARG2);
    top--;
    break;
  case OP_VECTOR_SET:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-set!: not a vector", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("vector-set!: immutable vector", sexp_list1(ctx, _ARG1));
    sexp_vector_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_VOID;
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
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-set!: not a string", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("string-set!: immutable string", sexp_list1(ctx, _ARG1));
    fprintf(stderr, "string-set! %p (immutable: %d)\n", _ARG1, sexp_immutablep(_ARG1));
    sexp_string_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_VOID;
    top-=2;
    break;
  case OP_STRING_LENGTH:
    _ARG1 = sexp_make_integer(sexp_string_length(_ARG1));
    break;
  case OP_MAKE_PROCEDURE:
    sexp_context_top(ctx) = top;
    _ARG4 = sexp_make_procedure(ctx, _ARG1, _ARG2, _ARG3, _ARG4);
    top-=3;
    break;
  case OP_MAKE_VECTOR:
    sexp_context_top(ctx) = top;
    _ARG2 = sexp_make_vector(ctx, _ARG1, _ARG2);
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
    j = sexp_integerp(_ARG1);
#if USE_BIGNUMS
    if (! j) j = sexp_bignump(_ARG1);
#endif
#if USE_FLONUMS
    if (! j)
      j = (sexp_flonump(_ARG1)
           && (sexp_flonum_value(_ARG1) == trunc(sexp_flonum_value(_ARG1))));
#endif
    _ARG1 = sexp_make_boolean(j);
    break;
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
    if (! sexp_pairp(_ARG1))
      sexp_raise("car: not a pair", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_car(_ARG1); break;
  case OP_CDR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("cdr: not a pair", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_cdr(_ARG1); break;
  case OP_SET_CAR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-car!: not a pair", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("set-car!: immutable pair", sexp_list1(ctx, _ARG1));
    sexp_car(_ARG1) = _ARG2;
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case OP_SET_CDR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-cdr!: not a pair", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("set-cdr!: immutable pair", sexp_list1(ctx, _ARG1));
    sexp_cdr(_ARG1) = _ARG2;
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case OP_CONS:
    sexp_context_top(ctx) = top;
    _ARG2 = sexp_cons(ctx, _ARG1, _ARG2);
    top--;
    break;
  case OP_ADD:
#if USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    if (sexp_integerp(tmp1) && sexp_integerp(tmp2)) {
      j = sexp_unbox_integer(tmp1) + sexp_unbox_integer(tmp2);
      if ((j < SEXP_MIN_FIXNUM) || (j > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_add(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_integer(j);
    }
    else
      _ARG2 = sexp_add(ctx, tmp1, tmp2);
#else
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_add(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_add(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) + (double)sexp_unbox_integer(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_integer(_ARG1) + sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("+: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case OP_SUB:
#if USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    if (sexp_integerp(tmp1) && sexp_integerp(tmp2)) {
      j = sexp_unbox_integer(tmp1) - sexp_unbox_integer(tmp2);
      if ((j < SEXP_MIN_FIXNUM) || (j > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_sub(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_integer(j);
    }
    else
      _ARG2 = sexp_sub(ctx, tmp1, tmp2);
#else
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_sub(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_sub(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) - (double)sexp_unbox_integer(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_integer(_ARG1) - sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("-: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case OP_MUL:
#if USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    if (sexp_integerp(tmp1) && sexp_integerp(tmp2)) {
      prod = sexp_unbox_integer(tmp1) * sexp_unbox_integer(tmp2);
      if ((prod < SEXP_MIN_FIXNUM) || (prod > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_mul(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_integer(prod);
    }
    else
      _ARG2 = sexp_mul(ctx, tmp1, tmp2);
#else
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_mul(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_mul(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) * (double)sexp_unbox_integer(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_integer(_ARG1) * sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("*: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case OP_DIV:
    if (_ARG2 == sexp_make_integer(0))
      sexp_raise("divide by zero", SEXP_NULL);
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2)) {
#if USE_FLONUMS
      _ARG1 = sexp_integer_to_flonum(ctx, _ARG1);
      _ARG2 = sexp_integer_to_flonum(ctx, _ARG2);
      _ARG2 = sexp_fp_div(ctx, _ARG1, _ARG2);
#else
      _ARG2 = sexp_fx_div(_ARG1, _ARG2);
#endif
    }
#if USE_BIGNUMS
    else
      _ARG2 = sexp_div(ctx, _ARG1, _ARG2);
#else
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_div(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) / (double)sexp_unbox_integer(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_integer(_ARG1) / sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("/: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case OP_QUOTIENT:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2)) {
      if (_ARG2 == sexp_make_integer(0))
        sexp_raise("divide by zero", SEXP_NULL);
      _ARG2 = sexp_fx_div(_ARG1, _ARG2);
      top--;
    }
#if USE_BIGNUMS
    else {
      _ARG2 = sexp_quotient(ctx, _ARG1, _ARG2);
      top--;
    }
#else
    else sexp_raise("quotient: not an integer", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    break;
  case OP_REMAINDER:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2)) {
      if (_ARG2 == sexp_make_integer(0))
        sexp_raise("divide by zero", SEXP_NULL);
      tmp1 = sexp_fx_rem(_ARG1, _ARG2);
      top--;
      _ARG1 = tmp1;
    }
#if USE_BIGNUMS
    else {
      _ARG2 = sexp_remainder(ctx, _ARG1, _ARG2);
      top--;
    }
#else
    else sexp_raise("remainder: not an integer", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    break;
  case OP_NEGATIVE:
    if (sexp_integerp(_ARG1))
      _ARG1 = sexp_make_integer(-sexp_unbox_integer(_ARG1));
#if USE_BIGNUMS
    else if (sexp_bignump(_ARG1)) {
      _ARG1 = sexp_copy_bignum(ctx, NULL, _ARG1, 0);
      sexp_bignum_sign(_ARG1) = -sexp_bignum_sign(_ARG1);
    }
#endif
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1))
      _ARG1 = sexp_make_flonum(ctx, -sexp_flonum_value(_ARG1));
#endif
    else sexp_raise("-: not a number", sexp_list1(ctx, _ARG1));
    break;
  case OP_INVERSE:
    if (sexp_integerp(_ARG1))
      _ARG1 = sexp_make_flonum(ctx, 1/(double)sexp_unbox_integer(_ARG1));
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1))
      _ARG1 = sexp_make_flonum(ctx, 1/sexp_flonum_value(_ARG1));
#endif
    else sexp_raise("/: not a number", sexp_list1(ctx, _ARG1));
    break;
  case OP_LT:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2)) {
      i = (sexp_sint_t)_ARG1 < (sexp_sint_t)_ARG2;
#if USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_integerp(tmp1)
        ? sexp_make_boolean(sexp_unbox_integer(tmp1) < 0) : tmp1;
    }
#else
#if USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      i = sexp_flonum_value(_ARG1) < sexp_flonum_value(_ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      i = sexp_flonum_value(_ARG1) < (double)sexp_unbox_integer(_ARG2);
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      i = (double)sexp_unbox_integer(_ARG1) < sexp_flonum_value(_ARG2);
#endif
    else sexp_raise("<: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case OP_LE:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2)) {
      i = (sexp_sint_t)_ARG1 <= (sexp_sint_t)_ARG2;
#if USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_integerp(tmp1)
        ? sexp_make_boolean(sexp_unbox_integer(tmp1) <= 0) : tmp1;
    }
#else
#if USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      i = sexp_flonum_value(_ARG1) <= sexp_flonum_value(_ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      i = sexp_flonum_value(_ARG1) <= (double)sexp_unbox_integer(_ARG2);
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      i = (double)sexp_unbox_integer(_ARG1) <= sexp_flonum_value(_ARG2);
#endif
    else sexp_raise("<=: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case OP_EQN:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2)) {
      i = _ARG1 == _ARG2;
#if USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_integerp(tmp1)
        ? sexp_make_boolean(sexp_unbox_integer(tmp1) == 0) : tmp1;
    }
#else
#if USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      i = sexp_flonum_value(_ARG1) == sexp_flonum_value(_ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      i = sexp_flonum_value(_ARG1) == (double)sexp_unbox_integer(_ARG2);
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      i = (double)sexp_unbox_integer(_ARG1) == sexp_flonum_value(_ARG2);
#endif
    else sexp_raise("=: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case OP_EQ:
    _ARG2 = sexp_make_boolean(_ARG1 == _ARG2);
    top--;
    break;
  case OP_FIX2FLO:
    if (sexp_integerp(_ARG1))
      _ARG1 = sexp_integer_to_flonum(ctx, _ARG1);
#if USE_BIGNUMS
    else if (sexp_bignump(_ARG1))
      _ARG1 = sexp_make_flonum(ctx, sexp_bignum_to_double(_ARG1));
#endif
    else if (! sexp_flonump(_ARG1))
      sexp_raise("exact->inexact: not a number", sexp_list1(ctx, _ARG1));
    break;
  case OP_FLO2FIX:
    if (sexp_flonump(_ARG1))
      _ARG1 = sexp_make_integer((sexp_sint_t)sexp_flonum_value(_ARG1));
    else if (! sexp_integerp(_ARG1))
      sexp_raise("inexact->exact: not a number", sexp_list1(ctx, _ARG1));
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
      sexp_write_string(ctx, sexp_string_data(_ARG1), _ARG2);
      _ARG2 = SEXP_VOID;
      top--;
      break;
    } else if (sexp_charp(_ARG1)) {
      sexp_write_char(ctx, sexp_unbox_character(_ARG1), _ARG2);
      _ARG2 = SEXP_VOID;
      top--;
      break;
    }
    /* ... FALLTHROUGH ... */
  case OP_WRITE:
    sexp_write(ctx, _ARG1, _ARG2);
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case OP_WRITE_CHAR:
    sexp_write_char(ctx, sexp_unbox_character(_ARG1), _ARG2);
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case OP_NEWLINE:
    sexp_newline(ctx, _ARG1);
    _ARG1 = SEXP_VOID;
    break;
  case OP_FLUSH_OUTPUT:
    sexp_flush(ctx, _ARG1);
    _ARG1 = SEXP_VOID;
    break;
  case OP_READ:
    sexp_context_top(ctx) = top;
    _ARG1 = sexp_read(ctx, _ARG1);
    sexp_check_exception();
    break;
  case OP_READ_CHAR:
    i = sexp_read_char(ctx, _ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case OP_PEEK_CHAR:
    i = sexp_read_char(ctx, _ARG1);
    sexp_push_char(ctx, i, _ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case OP_RET:
    i = sexp_unbox_integer(stack[fp]);
    stack[fp-i] = _ARG1;
    top = fp-i+1;
    self = stack[fp+2];
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc) + sexp_unbox_integer(stack[fp+1]);
    cp = sexp_procedure_vars(self);
    fp = sexp_unbox_integer(stack[fp+3]);
    break;
  case OP_DONE:
    goto end_loop;
  default:
    sexp_raise("unknown opcode", sexp_list1(ctx, sexp_make_integer(*(ip-1))));
  }
  goto loop;

 end_loop:
  sexp_gc_release(ctx, self, s_self);
  sexp_context_top(ctx) = top;
  return _ARG1;
}

/************************ library procedures **************************/

static sexp sexp_exception_type_func (sexp ctx, sexp exn) {
  if (sexp_exceptionp(exn))
    return sexp_exception_kind(exn);
  else
    return sexp_type_exception(ctx, "not an exception", exn);
}

static sexp sexp_open_input_file (sexp ctx, sexp path) {
  FILE *in;
  if (! sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);
  in = fopen(sexp_string_data(path), "r");
  if (! in)
    return
      sexp_user_exception(ctx, SEXP_FALSE, "couldn't open input file", path);
  return sexp_make_input_port(ctx, in, path);
}

static sexp sexp_open_output_file (sexp ctx, sexp path) {
  FILE *out;
  if (! sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);
  out = fopen(sexp_string_data(path), "w");
  if (! out)
    return
      sexp_user_exception(ctx, SEXP_FALSE, "couldn't open output file", path);
  return sexp_make_input_port(ctx, out, path);
}

static sexp sexp_close_port (sexp ctx, sexp port) {
  if (! sexp_portp(port))
    return sexp_type_exception(ctx, "not a port", port);
  if (! sexp_port_openp(port))
    return sexp_user_exception(ctx, SEXP_FALSE, "port already closed", port);
  if (sexp_port_buf(port))
    free(sexp_port_buf(port));
  if (sexp_port_stream(port))
    fclose(sexp_port_stream(port));
  sexp_port_openp(port) = 0;
  return SEXP_VOID;
}

void sexp_warn_undefs (sexp ctx, sexp from, sexp to, sexp out) {
  sexp x;
  for (x=from; sexp_pairp(x) && x!=to; x=sexp_cdr(x))
    if (sexp_cdar(x) == SEXP_UNDEF) {
      sexp_write_string(ctx, "WARNING: reference to undefined variable: ", out);
      sexp_write(ctx, sexp_caar(x), out);
      sexp_write_char(ctx, '\n', out);
    }
}

sexp sexp_load (sexp ctx, sexp source, sexp env) {
  sexp tmp, out;
  sexp_gc_var(ctx, ctx2, s_ctx2);
  sexp_gc_var(ctx, x, s_x);
  sexp_gc_var(ctx, in, s_in);
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_preserve(ctx, ctx2, s_ctx2);
  sexp_gc_preserve(ctx, x, s_x);
  sexp_gc_preserve(ctx, in, s_in);
  sexp_gc_preserve(ctx, res, s_res);
  res = SEXP_VOID;
  in = sexp_open_input_file(ctx, source);
  out = env_global_ref(env, the_cur_err_symbol, SEXP_FALSE);
  ctx2 = sexp_make_context(ctx, NULL, env);
  sexp_context_parent(ctx2) = ctx;
  tmp = sexp_env_bindings(env);
  sexp_context_tailp(ctx2) = 0;
  if (sexp_exceptionp(in)) {
    sexp_print_exception(ctx, in, out);
    res = in;
  } else {
    sexp_port_sourcep(in) = 1;
    while ((x=sexp_read(ctx, in)) != (sexp) SEXP_EOF) {
      res = sexp_eval(ctx2, x);
      if (sexp_exceptionp(res))
        break;
    }
    if (x == SEXP_EOF)
      res = SEXP_VOID;
    sexp_close_port(ctx, in);
#if USE_WARN_UNDEFS
    if (sexp_oportp(out))
      sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp, out);
#endif
  }
  sexp_gc_release(ctx, ctx2, s_ctx2);
  return res;
}

#if USE_MATH

#if USE_BIGNUMS
#define maybe_convert_bignum(z) \
  else if (sexp_bignump(z)) d = sexp_bignum_to_double(z);
#else
#define maybe_convert_bignum(z)
#endif

#define define_math_op(name, cname)       \
  static sexp name (sexp ctx, sexp z) {   \
    double d;                             \
    if (sexp_flonump(z))                  \
      d = sexp_flonum_value(z);           \
    else if (sexp_integerp(z))            \
      d = (double)sexp_unbox_integer(z);  \
    maybe_convert_bignum(z)               \
    else                                  \
      return sexp_type_exception(ctx, "not a number", z); \
    return sexp_make_flonum(ctx, cname(d));               \
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

static sexp sexp_expt (sexp ctx, sexp x, sexp e) {
  double f, x1, e1;
  sexp res;
#if USE_BIGNUMS
  if (sexp_bignump(e)) {
    if ((x == sexp_make_integer(0)) || (x == sexp_make_integer(-1)))
      res = sexp_make_flonum(ctx, pow(0, 0));
    else if (x == sexp_make_integer(1))
      res = sexp_make_flonum(ctx, sexp_unbox_integer(x));
    else if (sexp_flonump(x))
      res = sexp_make_flonum(ctx, pow(sexp_flonum_value(x), sexp_bignum_to_double(e)));
    else
      res = sexp_make_flonum(ctx, pow(10.0, 1e100));
  } else if (sexp_bignump(x)) {
    res = sexp_bignum_expt(ctx, x, e);
  } else {
#endif
  if (sexp_integerp(x))
    x1 = (double)sexp_unbox_integer(x);
#if USE_FLONUMS
  else if (sexp_flonump(x))
    x1 = sexp_flonum_value(x);
#endif
  else
    return sexp_type_exception(ctx, "not a number", x);
  if (sexp_integerp(e))
    e1 = (double)sexp_unbox_integer(e);
#if USE_FLONUMS
  else if (sexp_flonump(e))
    e1 = sexp_flonum_value(e);
#endif
  else
    return sexp_type_exception(ctx, "not a number", e);
  f = pow(x1, e1);
#if USE_FLONUMS
  if ((f > SEXP_MAX_FIXNUM) || sexp_flonump(x) || sexp_flonump(e)) {
    if (sexp_flonump(x) || sexp_flonump(e))
      res = sexp_make_flonum(ctx, f);
    else
      res = sexp_bignum_expt(ctx, sexp_fixnum_to_bignum(ctx, x), e);
  } else
#endif
    res = sexp_make_integer((sexp_sint_t)round(f));
#if USE_BIGNUMS
  }
#endif
  return res;
}

static sexp sexp_string_cmp (sexp ctx, sexp str1, sexp str2, sexp ci) {
  sexp_sint_t len1, len2, len, diff;
  if (! sexp_stringp(str1))
    return sexp_type_exception(ctx, "not a string", str1);
  if (! sexp_stringp(str2))
    return sexp_type_exception(ctx, "not a string", str2);
  len1 = sexp_string_length(str1);
  len2 = sexp_string_length(str2);
  len = ((len1<len2) ? len1 : len2);
  if (ci==SEXP_FALSE)
    diff = strncmp(sexp_string_data(str1), sexp_string_data(str2), len);
  else
    diff = strncasecmp(sexp_string_data(str1), sexp_string_data(str2), len);
  if (! diff)
    diff = len1 - len2;
  return sexp_make_integer(diff);
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

static sexp sexp_copy_core (sexp ctx, sexp core) {
  sexp res = sexp_alloc_type(ctx, core, SEXP_CORE);
  memcpy(res, core, sexp_sizeof(core));
  return res;
}

static sexp sexp_copy_opcode (sexp ctx, sexp op) {
  sexp res = sexp_alloc_type(ctx, opcode, SEXP_OPCODE);
  memcpy(res, op, sexp_sizeof(opcode));
  return res;
}

static sexp sexp_make_null_env (sexp ctx, sexp version) {
  sexp_uint_t i;
  sexp e = sexp_alloc_type(ctx, env, SEXP_ENV);
  sexp_env_lambda(e) = NULL;
  sexp_env_parent(e) = NULL;
  sexp_env_bindings(e) = SEXP_NULL;
  for (i=0; i<(sizeof(core_forms)/sizeof(core_forms[0])); i++)
    env_define(ctx, e, sexp_intern(ctx, sexp_core_name(&core_forms[i])),
               sexp_copy_core(ctx, &core_forms[i]));
  return e;
}

static sexp sexp_make_standard_env (sexp ctx, sexp version) {
  sexp_uint_t i;
  sexp ctx2, cell, sym, perr_cell, err_cell;
  sexp_gc_var(ctx, e, s_e);
  sexp_gc_var(ctx, op, s_op);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_var(ctx, err_handler, s_err);
  sexp_gc_preserve(ctx, e, s_e);
  sexp_gc_preserve(ctx, op, s_op);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, err_handler, s_err);
  e = sexp_make_null_env(ctx, version);
  for (i=0; i<(sizeof(opcodes)/sizeof(opcodes[0])); i++) {
    op = sexp_copy_opcode(ctx, &opcodes[i]);
    if (sexp_opcode_opt_param_p(op) && sexp_opcode_default(op)) {
      sym = sexp_intern(ctx, (char*)sexp_opcode_default(op));
      cell = env_cell_create(ctx, e, sym, SEXP_VOID);
      sexp_opcode_default(op) = cell;
    }
    env_define(ctx, e, sexp_intern(ctx, sexp_opcode_name(op)), op);
  }
  /* add io port and interaction env parameters */
  env_define(ctx, e, the_cur_in_symbol,
             sexp_make_input_port(ctx, stdin, SEXP_FALSE));
  env_define(ctx, e, the_cur_out_symbol,
             sexp_make_output_port(ctx, stdout, SEXP_FALSE));
  env_define(ctx, e, the_cur_err_symbol,
             sexp_make_output_port(ctx, stderr, SEXP_FALSE));
  env_define(ctx, e, the_interaction_env_symbol, e);
  /* add default exception handler */
  err_cell = env_cell(e, the_cur_err_symbol);
  perr_cell = env_cell(e, sexp_intern(ctx, "print-exception"));
  ctx2 = sexp_make_context(ctx, sexp_context_stack(ctx), e);
  sexp_context_tailp(ctx2) = 0;
  if (err_cell && perr_cell && sexp_opcodep(sexp_cdr(perr_cell))) {
    emit(ctx2, OP_GLOBAL_KNOWN_REF);
    emit_word(ctx2, (sexp_uint_t)err_cell);
    emit(ctx2, OP_LOCAL_REF);
    emit_word(ctx2, 0);
    emit(ctx2, OP_FCALL2);
    emit_word(ctx2, (sexp_uint_t)sexp_opcode_data(sexp_cdr(perr_cell)));
  }
  emit_push(ctx2, SEXP_VOID);
  emit(ctx2, OP_DONE);
  tmp = sexp_make_vector(ctx2, 0, SEXP_VOID);
  err_handler = sexp_make_procedure(ctx2,
                                    sexp_make_integer(0),
                                    sexp_make_integer(0),
                                    finalize_bytecode(ctx2),
                                    tmp);
  env_define(ctx2, e, the_err_handler_symbol, err_handler);
  sexp_gc_release(ctx, e, s_e);
  return e;
}

/************************** eval interface ****************************/

sexp sexp_apply (sexp ctx, sexp proc, sexp args) {
  sexp ls, *stack = sexp_stack_data(sexp_context_stack(ctx));
  sexp_sint_t top = sexp_context_top(ctx), offset;
  offset = top + sexp_unbox_integer(sexp_length(ctx, args));
  for (ls=args; sexp_pairp(ls); ls=sexp_cdr(ls), top++)
    stack[--offset] = sexp_car(ls);
  stack[top] = sexp_make_integer(top);
  top++;
  sexp_context_top(ctx) = top + 3;
  stack[top++] = sexp_make_integer(0);
  stack[top++] = final_resumer;
  stack[top++] = sexp_make_integer(0);
  return sexp_vm(ctx, proc);
}

sexp sexp_compile (sexp ctx, sexp x) {
  sexp_gc_var(ctx, ast, s_ast);
  sexp_gc_var(ctx, ctx2, s_ctx2);
  sexp_gc_var(ctx, vec, s_vec);
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_preserve(ctx, ast, s_ast);
  sexp_gc_preserve(ctx, ctx2, s_ctx2);
  sexp_gc_preserve(ctx, vec, s_vec);
  sexp_gc_preserve(ctx, res, s_res);
  ast = analyze(ctx, x);
  if (sexp_exceptionp(ast)) {
    res = ast;
  } else {
    free_vars(ctx, ast, SEXP_NULL);    /* should return SEXP_NULL */
    ctx2 = sexp_make_context(ctx,sexp_context_stack(ctx),sexp_context_env(ctx));
    sexp_context_parent(ctx2) = ctx;
    generate(ctx2, ast);
    res = finalize_bytecode(ctx2);
    vec = sexp_make_vector(ctx, 0, SEXP_VOID);
    res = sexp_make_procedure(ctx, sexp_make_integer(0), sexp_make_integer(0),
                              res, vec);
  }
  sexp_gc_release(ctx, ast, s_ast);
  return res;
}

sexp sexp_eval (sexp ctx, sexp obj) {
  sexp res;
  sexp_gc_var(ctx, thunk, s_thunk);
  sexp_gc_preserve(ctx, thunk, s_thunk);
  thunk = sexp_compile(ctx, obj);
  if (sexp_exceptionp(thunk)) {
    sexp_print_exception(ctx, thunk,
                         env_global_ref(sexp_context_env(ctx),
                                        the_cur_err_symbol,
                                        SEXP_FALSE));
    res = thunk;
  } else {
    res = sexp_apply(ctx, thunk, SEXP_NULL);
  }
  sexp_gc_release(ctx, thunk, s_thunk);
  return res;
}

sexp sexp_eval_string (sexp ctx, char *str) {
  sexp res;
  sexp_gc_var(ctx, obj, s_obj);
  sexp_gc_preserve(ctx, obj, s_obj);
  obj = sexp_read_from_string(ctx, str);
  res = sexp_eval(ctx, obj);
  sexp_gc_release(ctx, obj, s_obj);
  return res;
}

void sexp_scheme_init () {
  sexp ctx;
  if (! scheme_initialized_p) {
    scheme_initialized_p = 1;
    sexp_init();
    ctx = sexp_make_context(NULL, NULL, NULL);
    the_compile_error_symbol = sexp_intern(ctx, "compile");
    the_err_handler_symbol = sexp_intern(ctx, "*current-exception-handler*");
    the_cur_in_symbol = sexp_intern(ctx, "*current-input-port*");
    the_cur_out_symbol = sexp_intern(ctx, "*current-output-port*");
    the_cur_err_symbol = sexp_intern(ctx, "*current-error-port*");
    the_interaction_env_symbol = sexp_intern(ctx, "*interaction-environment*");
#if USE_BOEHM
    GC_add_roots((char*)&continuation_resumer,
                 ((char*)&continuation_resumer)+sizeof(continuation_resumer)+1);
    GC_add_roots((char*)&final_resumer,
                 ((char*)&final_resumer)+sizeof(continuation_resumer)+1);
    GC_add_roots((char*)&opcodes, ((char*)&opcodes)+sizeof(opcodes)+1);
#endif
    emit(ctx, OP_RESUMECC);
    continuation_resumer = finalize_bytecode(ctx);
    ctx = sexp_make_child_context(ctx, NULL);
    emit(ctx, OP_DONE);
    final_resumer = sexp_make_procedure(ctx,
                                        sexp_make_integer(0),
                                        sexp_make_integer(0),
                                        finalize_bytecode(ctx),
                                        sexp_make_vector(ctx, 0, SEXP_VOID));
  }
}
