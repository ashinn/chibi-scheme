/*  eval.c -- evaluator library implementation                */
/*  Copyright (c) 2009-2011 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"

/************************************************************************/

static int scheme_initialized_p = 0;

static sexp analyze (sexp ctx, sexp x);
static void generate (sexp ctx, sexp x);

#if SEXP_USE_MODULES
sexp sexp_load_module_file_op (sexp ctx sexp_api_params(self, n), sexp file, sexp env);
sexp sexp_find_module_file_op (sexp ctx sexp_api_params(self, n), sexp file);
sexp sexp_current_environment (sexp ctx sexp_api_params(self, n));
#endif

sexp sexp_compile_error (sexp ctx, const char *message, sexp o) {
  sexp exn;
  sexp_gc_var3(sym, irritants, msg);
  sexp_gc_preserve3(ctx, sym, irritants, msg);
  irritants = sexp_list1(ctx, o);
  msg = sexp_c_string(ctx, message, -1);
  exn = sexp_make_exception(ctx, sym = sexp_intern(ctx, "compile", -1),
                            msg, irritants, SEXP_FALSE,
                            (sexp_pairp(o)?sexp_pair_source(o):SEXP_FALSE));
  sexp_gc_release3(ctx);
  return exn;
}

static void sexp_warn (sexp ctx, char *msg, sexp x) {
  sexp out = sexp_current_error_port(ctx);
  if (sexp_oportp(out)) {
    sexp_write_string(ctx, "WARNING: ", out);
    sexp_write_string(ctx, msg, out);
    sexp_write(ctx, x, out);
    sexp_write_char(ctx, '\n', out);
  }
}

void sexp_warn_undefs (sexp ctx, sexp from, sexp to) {
  sexp x;
  for (x=from; sexp_pairp(x) && x!=to; x=sexp_env_next_cell(x))
    if (sexp_cdr(x) == SEXP_UNDEF)
      sexp_warn(ctx, "reference to undefined variable: ", sexp_car(x));
}


/********************** environment utilities ***************************/

static sexp sexp_env_cell_loc (sexp env, sexp key, int localp, sexp *varenv) {
  sexp ls;

  do {
#if SEXP_USE_RENAME_BINDINGS
    for (ls=sexp_env_renames(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
      if (sexp_car(ls) == key) {
        if (varenv) *varenv = env;
        return sexp_cdr(ls);
      }
#endif
    for (ls=sexp_env_bindings(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
      if (sexp_car(ls) == key) {
        if (varenv) *varenv = env;
        return ls;
      }
    env = (localp ? NULL : sexp_env_parent(env));
  } while (env);

  return NULL;
}

sexp sexp_env_cell (sexp env, sexp key, int localp) {
  return sexp_env_cell_loc(env, key, localp, NULL);
}

static sexp sexp_env_undefine (sexp ctx, sexp env, sexp key) {
  sexp ls1=NULL, ls2;
  for (ls2=sexp_env_bindings(env); sexp_pairp(ls2);
       ls1=ls2, ls2=sexp_env_next_cell(ls2))
    if (sexp_car(ls2) == key) {
      if (ls1) sexp_env_next_cell(ls1) = sexp_env_next_cell(ls2);
      else sexp_env_bindings(env) = sexp_env_next_cell(ls2);
      return SEXP_TRUE;
    }
  return SEXP_FALSE;
}

static sexp sexp_env_cell_define (sexp ctx, sexp env, sexp key,
                                  sexp value, sexp *varenv) {
  sexp_gc_var2(cell, ls);
  while (sexp_env_lambda(env) || sexp_env_syntactic_p(env))
    env = sexp_env_parent(env);
  if (varenv) *varenv = env;
#if SEXP_USE_RENAME_BINDINGS
  for (ls=sexp_env_renames(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
    if (sexp_car(ls) == key) {
      sexp_car(ls) = SEXP_FALSE;
      break;
    }
#endif
  for (ls=sexp_env_bindings(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
    if (sexp_car(ls) == key)
      return ls;
  sexp_gc_preserve2(ctx, cell, ls);
  sexp_env_push(ctx, env, cell, key, value);
  sexp_gc_release2(ctx);
  return cell;
}

static sexp sexp_env_cell_create (sexp ctx, sexp env, sexp key,
                                  sexp value, sexp *varenv) {
  sexp cell = sexp_env_cell_loc(env, key, 0, varenv);
  if (!cell) cell = sexp_env_cell_define(ctx, env, key, value, varenv);
  return cell;
}

sexp sexp_env_ref (sexp env, sexp key, sexp dflt) {
  sexp cell = sexp_env_cell(env, key, 0);
  return (cell ? sexp_cdr(cell) : dflt);
}

sexp sexp_env_define (sexp ctx, sexp env, sexp key, sexp value) {
  sexp cell, tmp, res = SEXP_VOID;
  if (sexp_immutablep(env))
    return sexp_user_exception(ctx, NULL, "immutable binding", key);
  cell = sexp_env_cell(env, key, 1);
  if (!cell) {
    sexp_env_push(ctx, env, tmp, key, value);
  } else if (sexp_immutablep(cell)) {
    res = sexp_user_exception(ctx, NULL, "immutable binding", key);
  } else if (sexp_syntacticp(value) && !sexp_syntacticp(sexp_cdr(cell))) {
    sexp_env_undefine(ctx, env, key);
    sexp_env_push(ctx, env, tmp, key, value);
  } else {
    sexp_cdr(cell) = value;
  }
  return res;
}

#if SEXP_USE_RENAME_BINDINGS
sexp sexp_env_rename (sexp ctx, sexp env, sexp key, sexp value) {
  sexp tmp;
  sexp_env_push_rename(ctx, env, tmp, key, value);
  return SEXP_VOID;
}
#endif

sexp sexp_env_exports_op (sexp ctx sexp_api_params(self, n), sexp env) {
  sexp ls;
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_NULL;
#if SEXP_USE_RENAME_BINDINGS
  for (ls=sexp_env_renames(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
    sexp_push(ctx, res, sexp_cadr(ls));
#endif
  for (ls=sexp_env_bindings(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
    sexp_push(ctx, res, sexp_car(ls));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_extend_env (sexp ctx, sexp env, sexp vars, sexp value) {
  sexp_gc_var2(e, tmp);
  sexp_gc_preserve2(ctx, e, tmp);
  e = sexp_alloc_type(ctx, env, SEXP_ENV);
  sexp_env_parent(e) = env;
  sexp_env_bindings(e) = SEXP_NULL;
#if SEXP_USE_RENAME_BINDINGS
  sexp_env_renames(e) = SEXP_NULL;
#endif
  for ( ; sexp_pairp(vars); vars = sexp_cdr(vars))
    sexp_env_push(ctx, e, tmp, sexp_car(vars), value);
  sexp_gc_release2(ctx);
  return e;
}

sexp sexp_extend_synclo_env (sexp ctx, sexp env) {
  sexp e1, e2;
  sexp_gc_var1(e);
  sexp_gc_preserve1(ctx, e);
  e = env;
  if (sexp_pairp(sexp_context_fv(ctx))) {
    e = sexp_alloc_type(ctx, env, SEXP_ENV);
    for (e1=env, e2=NULL; e1; e1=sexp_env_parent(e1)) {
      e2 = e2 ? (sexp_env_parent(e2) = sexp_alloc_type(ctx, env, SEXP_ENV)) : e;
      sexp_env_bindings(e2) = sexp_env_bindings(e1);
      sexp_env_syntactic_p(e2) = 1;
#if SEXP_USE_RENAME_BINDINGS
      sexp_env_renames(e2) = sexp_env_renames(e1);
#endif
    }
    sexp_env_parent(e2) = sexp_context_env(ctx);
  }
  sexp_gc_release1(ctx);
  return e;
}

static sexp sexp_reverse_flatten_dot (sexp ctx, sexp ls) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  for (res=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_push(ctx, res, sexp_car(ls));
  if (!sexp_nullp(ls))
    res = sexp_cons(ctx, ls, res);
  sexp_gc_release1(ctx);
  return res;
}

static sexp sexp_flatten_dot (sexp ctx, sexp ls) {
  return sexp_nreverse(ctx, sexp_reverse_flatten_dot(ctx, ls));
}

int sexp_param_index (sexp lambda, sexp name) {
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

static void shrink_bcode (sexp ctx, sexp_uint_t i) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(ctx)) != i) {
    tmp = sexp_alloc_bytecode(ctx, i);
    sexp_bytecode_name(tmp) = sexp_bytecode_name(sexp_context_bc(ctx));
    sexp_bytecode_length(tmp) = i;
    sexp_bytecode_literals(tmp)
      = sexp_bytecode_literals(sexp_context_bc(ctx));
    memcpy(sexp_bytecode_data(tmp),
           sexp_bytecode_data(sexp_context_bc(ctx)),
           i);
    sexp_context_bc(ctx) = tmp;
  }
}

static void expand_bcode (sexp ctx, sexp_uint_t size) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(ctx))
      < (sexp_context_pos(ctx))+size) {
    tmp=sexp_alloc_bytecode(ctx, sexp_bytecode_length(sexp_context_bc(ctx))*2);
    sexp_bytecode_name(tmp) = sexp_bytecode_name(sexp_context_bc(ctx));
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

static void emit_enter (sexp ctx);
static void emit_return (sexp ctx);
static void bless_bytecode (sexp ctx, sexp bc);

static sexp finalize_bytecode (sexp ctx) {
  sexp bc;
  emit_return(ctx);
  shrink_bcode(ctx, sexp_context_pos(ctx));
  bc = sexp_context_bc(ctx);
  if (sexp_pairp(sexp_bytecode_literals(bc))) { /* compress literals */
    if (sexp_nullp(sexp_cdr(sexp_bytecode_literals(bc))))
      sexp_bytecode_literals(bc) = sexp_car(sexp_bytecode_literals(bc));
    else if (sexp_nullp(sexp_cddr(sexp_bytecode_literals(bc))))
      sexp_cdr(sexp_bytecode_literals(bc)) = sexp_cadr(sexp_bytecode_literals(bc));
    else
      sexp_bytecode_literals(bc) = sexp_list_to_vector(ctx, sexp_bytecode_literals(bc));
  }
  bless_bytecode(ctx, bc);
  return bc;
}

static void emit (sexp ctx, unsigned char c)  {
  expand_bcode(ctx, 1);
  sexp_bytecode_data(sexp_context_bc(ctx))[sexp_context_pos(ctx)++] = c;
}

sexp sexp_make_procedure_op (sexp ctx sexp_api_params(self, n), sexp flags,
                             sexp num_args, sexp bc, sexp vars) {
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

static sexp sexp_make_synclo_op (sexp ctx sexp_api_params(self, n), sexp env, sexp fv, sexp expr) {
  sexp res;
  if (! (sexp_symbolp(expr) || sexp_pairp(expr)))
    return expr;
  res = sexp_alloc_type(ctx, synclo, SEXP_SYNCLO);
  sexp_synclo_env(res) = env;
  sexp_synclo_free_vars(res) = fv;
  sexp_synclo_expr(res) = expr;
  return res;
}

/* internal AST */

static sexp sexp_make_lambda (sexp ctx, sexp params) {
  sexp res = sexp_alloc_type(ctx, lambda, SEXP_LAMBDA);
  sexp_lambda_name(res) = SEXP_FALSE;
  sexp_lambda_params(res) = params;
  sexp_lambda_fv(res) = SEXP_NULL;
  sexp_lambda_sv(res) = SEXP_NULL;
  sexp_lambda_locals(res) = SEXP_NULL;
  sexp_lambda_defs(res) = SEXP_NULL;
  sexp_lambda_return_type(res) = SEXP_FALSE;
  sexp_lambda_param_types(res) = SEXP_NULL;
  return res;
}

static sexp sexp_make_set (sexp ctx, sexp var, sexp value) {
  sexp res = sexp_alloc_type(ctx, set, SEXP_SET);
  sexp_set_var(res) = var;
  sexp_set_value(res) = value;
  return res;
}

static sexp sexp_make_ref (sexp ctx, sexp name, sexp cell) {
  sexp res = sexp_alloc_type(ctx, ref, SEXP_REF);
  sexp_ref_name(res) = name;
  sexp_ref_cell(res) = cell;
  return res;
}

static sexp sexp_make_cnd (sexp ctx, sexp test, sexp pass, sexp fail) {
  sexp res = sexp_alloc_type(ctx, cnd, SEXP_CND);
  sexp_cnd_test(res) = test;
  sexp_cnd_pass(res) = pass;
  sexp_cnd_fail(res) = fail;
  return res;
}

static sexp sexp_make_lit (sexp ctx, sexp value) {
  sexp res = sexp_alloc_type(ctx, lit, SEXP_LIT);
  sexp_lit_value(res) = value;
  return res;
}

/****************************** contexts ******************************/

#define SEXP_STACK_SIZE (sexp_sizeof(stack)+sizeof(sexp)*SEXP_INIT_STACK_SIZE)

static void sexp_add_path (sexp ctx, const char *str) {
  const char *colon;
  if (str && *str) {
    colon = strchr(str, ':');
    if (colon)
      sexp_add_path(ctx, colon+1);
    else
      colon = str + strlen(str);
    sexp_push(ctx, sexp_global(ctx, SEXP_G_MODULE_PATH), SEXP_VOID);
    sexp_car(sexp_global(ctx, SEXP_G_MODULE_PATH))
      = sexp_c_string(ctx, str, colon-str);
  }
}

void sexp_init_eval_context_globals (sexp ctx) {
  sexp_gc_var3(tmp, vec, ctx2);
  ctx = sexp_make_child_context(ctx, NULL);
  sexp_gc_preserve3(ctx, tmp, vec, ctx2);
#if ! SEXP_USE_NATIVE_X86
  emit(ctx, SEXP_OP_RESUMECC);
  sexp_global(ctx, SEXP_G_RESUMECC_BYTECODE) = finalize_bytecode(ctx);
  ctx2 = sexp_make_child_context(ctx, NULL);
  emit(ctx2, SEXP_OP_DONE);
  tmp = finalize_bytecode(ctx2);
  vec = sexp_make_vector(ctx, 0, SEXP_VOID);
  sexp_global(ctx, SEXP_G_FINAL_RESUMER)
    = sexp_make_procedure(ctx, SEXP_ZERO, SEXP_ZERO, tmp, vec);
  sexp_bytecode_name(sexp_procedure_code(sexp_global(ctx, SEXP_G_FINAL_RESUMER)))
    = sexp_intern(ctx, "final-resumer", -1);
#endif
  sexp_global(ctx, SEXP_G_MODULE_PATH) = SEXP_NULL;
  sexp_add_path(ctx, sexp_default_module_dir);
  sexp_add_path(ctx, getenv(SEXP_MODULE_PATH_VAR));
  tmp = sexp_c_string(ctx, "./lib", 5);
  sexp_push(ctx, sexp_global(ctx, SEXP_G_MODULE_PATH), tmp);
  tmp = sexp_c_string(ctx, ".", 1);
  sexp_push(ctx, sexp_global(ctx, SEXP_G_MODULE_PATH), tmp);
#if SEXP_USE_GREEN_THREADS
  sexp_global(ctx, SEXP_G_IO_BLOCK_ERROR)
    = sexp_user_exception(ctx, SEXP_FALSE, "I/O would block", SEXP_NULL);
  sexp_global(ctx, SEXP_G_THREADS_FRONT) = SEXP_NULL;
  sexp_global(ctx, SEXP_G_THREADS_BACK) = SEXP_NULL;
  sexp_global(ctx, SEXP_G_THREADS_SIGNALS) = SEXP_ZERO;
  sexp_global(ctx, SEXP_G_THREADS_SIGNAL_RUNNER) = SEXP_FALSE;
#endif
  sexp_gc_release3(ctx);
}

sexp sexp_make_eval_context (sexp ctx, sexp stack, sexp env, sexp_uint_t size, sexp_uint_t max_size) {
  sexp_gc_var1(res);
  res = sexp_make_context(ctx, size, max_size);
  if (!res || sexp_exceptionp(res))
    return res;
  if (ctx) sexp_gc_preserve1(ctx, res);
  sexp_context_env(res) = (env ? env : sexp_make_primitive_env(res, SEXP_SEVEN));
  sexp_context_bc(res) = sexp_alloc_bytecode(res, SEXP_INIT_BCODE_SIZE);
  sexp_bytecode_name(sexp_context_bc(res)) = SEXP_FALSE;
  sexp_bytecode_length(sexp_context_bc(res)) = SEXP_INIT_BCODE_SIZE;
  sexp_bytecode_literals(sexp_context_bc(res)) = SEXP_NULL;
  if ((! stack) || (stack == SEXP_FALSE)) {
    stack = sexp_alloc_tagged(res, SEXP_STACK_SIZE, SEXP_STACK);
    sexp_stack_length(stack) = SEXP_INIT_STACK_SIZE;
    sexp_stack_top(stack) = 0;
  }
  sexp_context_stack(res) = stack;
  if (! ctx) sexp_init_eval_context_globals(res);
  if (ctx) {
    sexp_context_params(res) = sexp_context_params(ctx);
    sexp_context_tracep(res) = sexp_context_tracep(ctx);
    sexp_gc_release1(ctx);
  }
  return res;
}

sexp sexp_make_child_context (sexp ctx, sexp lambda) {
  sexp res = sexp_make_eval_context(ctx,
                                    sexp_context_stack(ctx),
                                    sexp_context_env(ctx),
                                    0,
                                    sexp_context_max_size(ctx));
  if (! sexp_exceptionp(res)) {
    sexp_context_lambda(res) = lambda;
    sexp_context_top(res) = sexp_context_top(ctx);
    sexp_context_fv(res) = sexp_context_fv(ctx);
    sexp_context_tracep(res) = sexp_context_tracep(ctx);
  }
  return res;
}

/**************************** identifiers *****************************/

sexp sexp_identifierp_op (sexp ctx sexp_api_params(self, n), sexp x) {
  return sexp_make_boolean(sexp_idp(x));
}

sexp sexp_syntactic_closure_expr_op (sexp ctx sexp_api_params(self, n), sexp x) {
  return (sexp_synclop(x) ? sexp_synclo_expr(x) : x);
}

sexp sexp_strip_synclos (sexp ctx sexp_api_params(self, n), sexp x) {
  sexp res;
  sexp_gc_var2(kar, kdr);
  sexp_gc_preserve2(ctx, kar, kdr);
 loop:
  if (sexp_synclop(x)) {
    x = sexp_synclo_expr(x);
    goto loop;
  } else if (sexp_pairp(x)) {
    kar = sexp_strip_synclos(ctx sexp_api_pass(self, n), sexp_car(x));
    kdr = sexp_strip_synclos(ctx sexp_api_pass(self, n), sexp_cdr(x));
    res = sexp_cons(ctx, kar, kdr);
    sexp_immutablep(res) = 1;
  } else {
    res = x;
  }
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_identifier_eq_op (sexp ctx sexp_api_params(self, n), sexp e1, sexp id1, sexp e2, sexp id2) {
  sexp cell1, cell2;
  cell1 = sexp_env_cell(e1, id1, 0);
  if (!cell1 && sexp_synclop(id1)) {
    e1 = sexp_synclo_env(id1);
    id1 = sexp_synclo_expr(id1);
    cell1 = sexp_env_cell(e1, id1, 0);
  }
  cell2 = sexp_env_cell(e2, id2, 0);
  if (!cell2 && sexp_synclop(id2)) {
    e2 = sexp_synclo_env(id2);
    id2 = sexp_synclo_expr(id2);
    cell2 = sexp_env_cell(e2, id2, 0);
  }
  if (cell1 && (cell1 == cell2))
    return SEXP_TRUE;
  else if (!cell1 && !cell2 && (id1 == id2))
    return SEXP_TRUE;
#if ! SEXP_USE_STRICT_TOPLEVEL_BINDINGS
  else if (cell1 && !sexp_lambdap(sexp_cdr(cell1))
           && cell2 && !sexp_lambdap(sexp_cdr(cell2))
           && (id1 == id2))
    return SEXP_TRUE;
#endif
  return SEXP_FALSE;
}

/************************* the compiler ***************************/

static sexp analyze_app (sexp ctx, sexp x) {
  sexp p;
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
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
  if (sexp_pairp(res)) {        /* fill in lambda names */
    res = sexp_nreverse(ctx, res);
    if (sexp_lambdap(sexp_car(res))) {
      p=sexp_lambda_params(sexp_car(res));
      for (tmp=sexp_cdr(res); sexp_pairp(tmp)&&sexp_pairp(p); tmp=sexp_cdr(tmp), p=sexp_cdr(p))
        if (sexp_lambdap(sexp_car(tmp)))
          sexp_lambda_name(sexp_car(tmp)) = sexp_car(p);
    }
  }
  sexp_gc_release2(ctx);
  return res;
}

static sexp analyze_seq (sexp ctx, sexp ls) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
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
  sexp_gc_release2(ctx);
  return res;
}

static sexp analyze_var_ref (sexp ctx, sexp x, sexp *varenv) {
  sexp env = sexp_context_env(ctx), res;
  sexp_gc_var1(cell);
  sexp_gc_preserve1(ctx, cell);
  cell = sexp_env_cell_loc(env, x, 0, varenv);
  if (! cell) {
    if (sexp_synclop(x)) {
      if (sexp_not(sexp_memq(ctx, sexp_synclo_expr(x), sexp_context_fv(ctx)))
          && sexp_not(sexp_memq(ctx, sexp_synclo_expr(x), sexp_synclo_free_vars(x))))
        env = sexp_synclo_env(x);
      x = sexp_synclo_expr(x);
    }
    cell = sexp_env_cell_create(ctx, env, x, SEXP_UNDEF, varenv);
  }
  if (sexp_macrop(sexp_cdr(cell)) || sexp_corep(sexp_cdr(cell)))
    res = sexp_compile_error(ctx, "invalid use of syntax as value", x);
  else 
    res = sexp_make_ref(ctx, x, cell);
  sexp_gc_release1(ctx);
  return res;
}

static sexp analyze_set (sexp ctx, sexp x) {
  sexp res, varenv;
  sexp_gc_var2(ref, value);
  sexp_gc_preserve2(ctx, ref, value);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x))
         && sexp_nullp(sexp_cdddr(x)) && sexp_idp(sexp_cadr(x)))) {
    res = sexp_compile_error(ctx, "bad set! syntax", x);
  } else {
    ref = analyze_var_ref(ctx, sexp_cadr(x), &varenv);
    if (sexp_lambdap(sexp_ref_loc(ref)))
      sexp_insert(ctx, sexp_lambda_sv(sexp_ref_loc(ref)), sexp_ref_name(ref));
    value = analyze(ctx, sexp_caddr(x));
    if (sexp_exceptionp(ref))
      res = ref;
    else if (sexp_exceptionp(value))
      res = value;
    else if (sexp_immutablep(sexp_ref_cell(ref))
             || (varenv && sexp_immutablep(varenv)))
      res = sexp_compile_error(ctx, "immutable binding", sexp_cadr(x));
    else
      res = sexp_make_set(ctx, ref, value);
  }
  sexp_gc_release2(ctx);
  return res;
}

#define sexp_return(res, val) do {res=val; goto cleanup;} while (0)

static sexp analyze_lambda (sexp ctx, sexp x) {
  sexp name, ls, ctx3;
  sexp_gc_var6(res, body, tmp, value, defs, ctx2);
  sexp_gc_preserve6(ctx, res, body, tmp, value, defs, ctx2);
  /* verify syntax */
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x))))
    sexp_return(res, sexp_compile_error(ctx, "bad lambda syntax", x));
  for (ls=sexp_cadr(x); sexp_pairp(ls); ls=sexp_cdr(ls))
    if (! sexp_idp(sexp_car(ls)))
      sexp_return(res, sexp_compile_error(ctx, "non-symbol parameter", x));
    else if (sexp_truep(sexp_memq(ctx, sexp_car(ls), sexp_cdr(ls))))
      sexp_return(res, sexp_compile_error(ctx, "duplicate parameter", x));
  /* build lambda and analyze body */
  res = sexp_make_lambda(ctx, tmp=sexp_copy_list(ctx, sexp_cadr(x)));
  sexp_lambda_source(res) = sexp_pair_source(x);
  if (! (sexp_lambda_source(res) && sexp_pairp(sexp_lambda_source(res))))
    sexp_lambda_source(res) = sexp_pair_source(sexp_cdr(x));
  if (! (sexp_lambda_source(res) && sexp_pairp(sexp_lambda_source(res))))
    sexp_lambda_source(res) = sexp_pair_source(sexp_cddr(x));
  ctx2 = sexp_make_child_context(ctx, res);
  tmp = sexp_flatten_dot(ctx2, sexp_lambda_params(res));
  sexp_context_env(ctx2) = sexp_extend_env(ctx2, sexp_context_env(ctx2), tmp, res);
  sexp_env_lambda(sexp_context_env(ctx2)) = res;
  body = analyze_seq(ctx2, sexp_cddr(x));
  if (sexp_exceptionp(body)) sexp_return(res, body);
  /* delayed analyze internal defines */
  defs = SEXP_NULL;
  for (ls=sexp_lambda_defs(res); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    tmp = sexp_car(ls);
    ctx3 = sexp_cdr(tmp);
    if (sexp_pairp(sexp_caar(tmp))) {
      name = sexp_caaar(tmp);
      tmp = sexp_cons(ctx3, sexp_cdaar(tmp), sexp_cdar(tmp));
      tmp = sexp_cons(ctx3, SEXP_VOID, tmp);
      sexp_pair_source(tmp) = sexp_pair_source(sexp_caar(ls));
      value = analyze_lambda(ctx3, tmp);
    } else {
      name = sexp_caar(tmp);
      value = analyze(ctx3, sexp_cadar(tmp));
    }
    if (sexp_exceptionp(value)) sexp_return(res, value);
    if (sexp_lambdap(value)) sexp_lambda_name(value) = name;
    sexp_push(ctx3, defs,
              sexp_make_set(ctx3, analyze_var_ref(ctx3, name, NULL), value));
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
  sexp_gc_release6(ctx);
  return res;
}

static sexp analyze_if (sexp ctx, sexp x) {
  sexp res, fail_expr;
  sexp_gc_var3(test, pass, fail);
  sexp_gc_preserve3(ctx, test, pass, fail);
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
  sexp_gc_release3(ctx);
  return res;
}

static sexp analyze_define (sexp ctx, sexp x) {
  sexp name, res, varenv;
  sexp_gc_var4(ref, value, tmp, env);
  sexp_gc_preserve4(ctx, ref, value, tmp, env);
  env = sexp_context_env(ctx);
  while (sexp_env_syntactic_p(env) && sexp_env_parent(env))
    env = sexp_env_parent(env);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x)))) {
    res = sexp_compile_error(ctx, "bad define syntax", x);
  } else {
    name = (sexp_pairp(sexp_cadr(x)) ? sexp_caadr(x) : sexp_cadr(x));
    if (! sexp_idp(name)) {
      res = sexp_compile_error(ctx, "can't define a non-symbol", x);
    } else if (sexp_env_lambda(env) && sexp_lambdap(sexp_env_lambda(env))) {
      sexp_env_push(ctx, env, tmp, name, sexp_context_lambda(ctx));
      sexp_push(ctx, sexp_lambda_sv(sexp_env_lambda(env)), name);
      sexp_push(ctx, sexp_lambda_locals(sexp_env_lambda(env)), name);
      tmp = sexp_cons(ctx, sexp_cdr(x), ctx);
      sexp_pair_source(sexp_cdr(x)) = sexp_pair_source(x);
      sexp_push(ctx, sexp_lambda_defs(sexp_env_lambda(env)), tmp);
      res = SEXP_VOID;
    } else {
      if (sexp_synclop(name)) name = sexp_synclo_expr(name);
      sexp_env_cell_define(ctx, env, name, SEXP_VOID, &varenv);
      if (sexp_pairp(sexp_cadr(x))) {
        tmp = sexp_cons(ctx, sexp_cdadr(x), sexp_cddr(x));
        tmp = sexp_cons(ctx, SEXP_VOID, tmp);
        sexp_pair_source(tmp) = sexp_pair_source(x);
        value = analyze_lambda(ctx, tmp);
      } else
        value = analyze(ctx, sexp_caddr(x));
      tmp = sexp_env_cell_loc(env, name, 0, &varenv);
      ref = sexp_make_ref(ctx, name, tmp);
      if (sexp_exceptionp(ref)) {
        res = ref;
      } else if (sexp_exceptionp(value)) {
        res = value;
      } else if (varenv && sexp_immutablep(varenv)) {
        res = sexp_compile_error(ctx, "immutable binding", name);
      } else {
        if (sexp_lambdap(value)) sexp_lambda_name(value) = name;
        res = sexp_make_set(ctx, ref, value);
      }
    }
  }
  sexp_gc_release4(ctx);
  return res;
}

static sexp analyze_bind_syntax (sexp ls, sexp eval_ctx, sexp bind_ctx) {
  sexp res = SEXP_VOID, name;
  sexp_gc_var2(proc, mac);
  sexp_gc_preserve2(eval_ctx, proc, mac);
  for ( ; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    if (! (sexp_pairp(sexp_car(ls)) && sexp_pairp(sexp_cdar(ls))
           && sexp_nullp(sexp_cddar(ls)))) {
      res = sexp_compile_error(eval_ctx, "bad syntax binding", sexp_car(ls));
    } else {
      proc = sexp_eval(eval_ctx, sexp_cadar(ls), NULL);
      if (sexp_procedurep(proc)) {
        name = sexp_caar(ls);
        if (sexp_synclop(name) && sexp_env_global_p(sexp_context_env(bind_ctx)))
          name = sexp_synclo_expr(name);
        mac = sexp_make_macro(eval_ctx, proc, sexp_context_env(bind_ctx));
        sexp_macro_source(mac) = sexp_pair_source(sexp_cadar(ls));
        sexp_env_define(eval_ctx, sexp_context_env(bind_ctx), name, mac);
      } else {
        res = (sexp_exceptionp(proc) ? proc
               : sexp_compile_error(eval_ctx, "non-procedure macro:", proc));
        break;
      }
    }
  }
  sexp_gc_release2(eval_ctx);
  return res;
}

static sexp analyze_define_syntax (sexp ctx, sexp x) {
  sexp res;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  tmp = sexp_list1(ctx, sexp_cdr(x));
  res = analyze_bind_syntax(tmp, ctx, ctx);
  sexp_gc_release1(ctx);
  return res;
}

static sexp analyze_let_syntax_aux (sexp ctx, sexp x, int recp) {
  sexp res;
  sexp_gc_var3(env, ctx2, tmp);
  sexp_gc_preserve3(ctx, env, ctx2, tmp);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x)))) {
    res = sexp_compile_error(ctx, "bad let(rec)-syntax", x);
  } else {
    env = sexp_alloc_type(ctx, env, SEXP_ENV);
    sexp_env_syntactic_p(env) = 1;
    sexp_env_parent(env) = sexp_context_env(ctx);
    sexp_env_bindings(env) = SEXP_NULL;
#if SEXP_USE_RENAME_BINDINGS
    sexp_env_renames(env) = SEXP_NULL;
#endif
    ctx2 = sexp_make_child_context(ctx, sexp_context_lambda(ctx));
    sexp_context_env(ctx2) = env;
    tmp = analyze_bind_syntax(sexp_cadr(x), (recp ? ctx2 : ctx), ctx2);
    res = (sexp_exceptionp(tmp) ? tmp : analyze_seq(ctx2, sexp_cddr(x)));
  }
  sexp_gc_release3(ctx);
  return res;
}

static sexp analyze_let_syntax (sexp ctx, sexp x) {
  return analyze_let_syntax_aux(ctx, x, 0);
}

static sexp analyze_letrec_syntax (sexp ctx, sexp x) {
  return analyze_let_syntax_aux(ctx, x, 1);
}

static sexp analyze (sexp ctx, sexp object) {
  sexp op;
  sexp_gc_var4(res, tmp, x, cell);
  sexp_gc_preserve4(ctx, res, tmp, x, cell);
  x = object;
 loop:
  if (sexp_pairp(x)) {
    if (sexp_not(sexp_listp(ctx, x))) {
      res = sexp_compile_error(ctx, "dotted list in source", x);
    } else if (sexp_idp(sexp_car(x))) {
      cell = sexp_env_cell(sexp_context_env(ctx), sexp_car(x), 0);
      if (! cell && sexp_synclop(sexp_car(x)))
        cell = sexp_env_cell(sexp_synclo_env(sexp_car(x)),
                             sexp_synclo_expr(sexp_car(x)),
                             0);
      if (! cell) {
        res = analyze_app(ctx, x);
        if (sexp_exceptionp(res))
          sexp_warn(ctx, "exception inside undefined operator: ", sexp_car(x));
      } else {
        op = sexp_cdr(cell);
        if (sexp_corep(op)) {
          switch (sexp_core_code(op)) {
          case SEXP_CORE_DEFINE:
            res = analyze_define(ctx, x); break;
          case SEXP_CORE_SET:
            res = analyze_set(ctx, x); break;
          case SEXP_CORE_LAMBDA:
            res = analyze_lambda(ctx, x); break;
          case SEXP_CORE_IF:
            res = analyze_if(ctx, x); break;
          case SEXP_CORE_BEGIN:
            res = analyze_seq(ctx, sexp_cdr(x)); break;
          case SEXP_CORE_QUOTE:
          case SEXP_CORE_SYNTAX_QUOTE:
            if (! (sexp_pairp(sexp_cdr(x)) && sexp_nullp(sexp_cddr(x))))
              res = sexp_compile_error(ctx, "bad quote form", x);
            else
              res = sexp_make_lit(ctx,
                                  (sexp_core_code(op) == SEXP_CORE_QUOTE) ?
                                  sexp_strip_synclos(ctx  sexp_api_pass(NULL, 1), sexp_cadr(x)) :
                                  sexp_cadr(x));
            break;
          case SEXP_CORE_DEFINE_SYNTAX:
            res = analyze_define_syntax(ctx, x); break;
          case SEXP_CORE_LET_SYNTAX:
            res = analyze_let_syntax(ctx, x); break;
          case SEXP_CORE_LETREC_SYNTAX:
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
          if (sexp_exceptionp(x) && sexp_not(sexp_exception_source(x)))
            sexp_exception_source(x) = sexp_pair_source(sexp_car(tmp));
          goto loop;
        } else if (sexp_opcodep(op)) {
          res = sexp_length(ctx, sexp_cdr(x));
          if (sexp_unbox_fixnum(res) < sexp_opcode_num_args(op)) {
            res = sexp_compile_error(ctx, "not enough args for opcode", x);
          } else if ((sexp_unbox_fixnum(res) > sexp_opcode_num_args(op))
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
    } else {
      res = analyze_app(ctx, x);
      if (!sexp_exceptionp(res)
          && !(sexp_pairp(sexp_car(x))
               || (sexp_synclop(sexp_car(x))
                   && sexp_pairp(sexp_synclo_expr(sexp_car(x))))))
        sexp_warn(ctx, "invalid operator in application: ", x);
    }
  } else if (sexp_idp(x)) {
    res = analyze_var_ref(ctx, x, NULL);
  } else if (sexp_synclop(x)) {
    tmp = sexp_make_child_context(ctx, sexp_context_lambda(ctx));
    sexp_context_fv(tmp) = sexp_append2(tmp,
                                        sexp_synclo_free_vars(x),
                                        sexp_context_fv(tmp));
    sexp_context_env(tmp) = sexp_extend_synclo_env(tmp, sexp_synclo_env(x));
    x = sexp_synclo_expr(x);
    res = analyze(tmp, x);
  } else if (sexp_nullp(x)) {
    res = sexp_compile_error(ctx, "empty application in source", x);
  } else {
    res = x;
  }
  if (sexp_exceptionp(res) && sexp_not(sexp_exception_source(res))
      && sexp_pairp(x))
    sexp_exception_source(res) = sexp_pair_source(x);
  sexp_gc_release4(ctx);
  return res;
}

sexp sexp_analyze (sexp ctx, sexp x) {return analyze(ctx, x);}

/********************** free varable analysis *************************/

static sexp insert_free_var (sexp ctx, sexp x, sexp fv) {
  sexp name=sexp_ref_name(x), loc=sexp_ref_loc(x), ls;
  for (ls=fv; sexp_pairp(ls); ls=sexp_cdr(ls))
    if ((name == sexp_ref_name(sexp_car(ls)))
        && (loc == sexp_ref_loc(sexp_car(ls))))
      return fv;
  return sexp_cons(ctx, x, fv);
}

static sexp union_free_vars (sexp ctx, sexp fv1, sexp fv2) {
  sexp_gc_var1(res);
  if (sexp_nullp(fv2))
    return fv1;
  sexp_gc_preserve1(ctx, res);
  for (res=fv2; sexp_pairp(fv1); fv1=sexp_cdr(fv1))
    res = insert_free_var(ctx, sexp_car(fv1), res);
  sexp_gc_release1(ctx);
  return res;
}

static sexp diff_free_vars (sexp ctx, sexp lambda, sexp fv, sexp params) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_NULL;
  for ( ; sexp_pairp(fv); fv=sexp_cdr(fv))
    if ((sexp_ref_loc(sexp_car(fv)) != lambda)
        || (sexp_memq(NULL, sexp_ref_name(sexp_car(fv)), params)
            == SEXP_FALSE))
      sexp_push(ctx, res, sexp_car(fv));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_free_vars (sexp ctx, sexp x, sexp fv) {
  sexp_gc_var2(fv1, fv2);
  sexp_gc_preserve2(ctx, fv1, fv2);
  fv1 = fv;
  if (sexp_lambdap(x)) {
    fv1 = sexp_free_vars(ctx, sexp_lambda_body(x), SEXP_NULL);
    fv2 = sexp_flatten_dot(ctx, sexp_lambda_params(x));
    fv2 = sexp_append2(ctx, sexp_lambda_locals(x), fv2);
    fv2 = diff_free_vars(ctx, x, fv1, fv2);
    sexp_lambda_fv(x) = fv2;
    fv1 = union_free_vars(ctx, fv2, fv);
  } else if (sexp_pairp(x)) {
    for ( ; sexp_pairp(x); x=sexp_cdr(x))
      fv1 = sexp_free_vars(ctx, sexp_car(x), fv1);
  } else if (sexp_cndp(x)) {
    fv1 = sexp_free_vars(ctx, sexp_cnd_test(x), fv);
    fv1 = sexp_free_vars(ctx, sexp_cnd_pass(x), fv1);
    fv1 = sexp_free_vars(ctx, sexp_cnd_fail(x), fv1);
  } else if (sexp_seqp(x)) {
    for (x=sexp_seq_ls(x); sexp_pairp(x); x=sexp_cdr(x))
      fv1 = sexp_free_vars(ctx, sexp_car(x), fv1);
  } else if (sexp_setp(x)) {
    fv1 = sexp_free_vars(ctx, sexp_set_value(x), fv);
    fv1 = sexp_free_vars(ctx, sexp_set_var(x), fv1);
  } else if (sexp_refp(x) && sexp_lambdap(sexp_ref_loc(x))) {
    fv1 = insert_free_var(ctx, x, fv);
  } else if (sexp_synclop(x)) {
    fv1 = sexp_free_vars(ctx, sexp_synclo_expr(x), fv);
  }
  sexp_gc_release2(ctx);
  return fv1;
}

/************************ library procedures **************************/

static sexp sexp_exception_type_op (sexp ctx sexp_api_params(self, n), sexp exn) {
  sexp_assert_type(ctx, sexp_exceptionp, SEXP_EXCEPTION, exn);
  return sexp_exception_kind(exn);
}

sexp sexp_open_input_file_op (sexp ctx sexp_api_params(self, n), sexp path) {
  FILE *in;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, path);
  in = fopen(sexp_string_data(path), "r");
  if (! in)
    return sexp_user_exception(ctx, self, "couldn't open input file", path);
#if SEXP_USE_GREEN_THREADS
  fcntl(fileno(in), F_SETFL, O_NONBLOCK);
#endif
  return sexp_make_input_port(ctx, in, path);
}

sexp sexp_open_output_file_op (sexp ctx sexp_api_params(self, n), sexp path) {
  FILE *out;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, path);
  out = fopen(sexp_string_data(path), "w");
  if (! out)
    return sexp_user_exception(ctx, self, "couldn't open output file", path);
  return sexp_make_output_port(ctx, out, path);
}

sexp sexp_close_port_op (sexp ctx sexp_api_params(self, n), sexp port) {
  sexp_assert_type(ctx, sexp_portp, SEXP_OPORT, port);
  if (! sexp_port_openp(port))
    return sexp_user_exception(ctx, self, "port already closed", port);
  return sexp_finalize_port(ctx sexp_api_pass(self, n), port);
}

#if SEXP_USE_DL
#ifdef __MINGW32__
#include <windows.h>
static sexp sexp_load_dl (sexp ctx, sexp file, sexp env) {
  sexp_proc2 init;
  HINSTANCE handle = LoadLibraryA(sexp_string_data(file));
  if(!handle)
    return sexp_compile_error(ctx, "couldn't load dynamic library", file);
  init = (sexp_proc2) GetProcAddress(handle, "sexp_init_library");
  if(!init) {
    FreeLibrary(handle);
    return sexp_compile_error(ctx, "dynamic library has no sexp_init_library", file);
  }
  return init(ctx sexp_api_pass(NULL, 1), env);
}
#else
static sexp sexp_load_dl (sexp ctx, sexp file, sexp env) {
  sexp_proc2 init;
  void *handle = dlopen(sexp_string_data(file), RTLD_LAZY);
  if (! handle)
    return sexp_compile_error(ctx, "couldn't load dynamic library", file);
  init = dlsym(handle, "sexp_init_library");
  if (! init) {
    dlclose(handle);
    return sexp_compile_error(ctx, "dynamic library has no sexp_init_library", file);
  }
  return init(ctx sexp_api_pass(NULL, 1), env);
}
#endif
#endif

sexp sexp_load_op (sexp ctx sexp_api_params(self, n), sexp source, sexp env) {
#if SEXP_USE_DL
  char *suffix;
#endif
  sexp tmp, out=SEXP_FALSE;
  sexp_gc_var4(ctx2, x, in, res);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, source);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
#if SEXP_USE_DL
  suffix = sexp_string_data(source)
    + sexp_string_length(source) - strlen(sexp_so_extension);
  if (strcmp(suffix, sexp_so_extension) == 0) {
    res = sexp_load_dl(ctx, source, env);
  } else {
#endif
  sexp_gc_preserve4(ctx, ctx2, x, in, res);
  res = SEXP_VOID;
  in = sexp_open_input_file(ctx, source);
  out = sexp_current_error_port(ctx);
  ctx2 = sexp_make_eval_context(ctx, NULL, env, 0, 0);
  sexp_context_parent(ctx2) = ctx;
  tmp = sexp_env_bindings(env);
  sexp_context_tailp(ctx2) = 0;
  if (sexp_exceptionp(in)) {
    if (sexp_not(out)) out = sexp_current_error_port(ctx);
    if (sexp_oportp(out))
      sexp_print_exception(ctx, in, out);
    res = in;
  } else {
    sexp_port_sourcep(in) = 1;
    while ((x=sexp_read(ctx, in)) != (sexp) SEXP_EOF) {
      res = sexp_eval(ctx2, x, env);
      if (sexp_exceptionp(res))
        break;
    }
    if (x == SEXP_EOF)
      res = SEXP_VOID;
    sexp_close_port(ctx, in);
  }
#if SEXP_USE_WARN_UNDEFS
  if (! sexp_exceptionp(res))
    sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp);
#endif
  sexp_gc_release4(ctx);
#if SEXP_USE_DL
  }
#endif
  return res;
}

#if SEXP_USE_MATH

#if SEXP_USE_BIGNUMS
#define maybe_convert_bignum(z) \
  else if (sexp_bignump(z)) d = sexp_bignum_to_double(z);
#else
#define maybe_convert_bignum(z)
#endif

#define define_math_op(name, cname)                                     \
  static sexp name (sexp ctx sexp_api_params(self, n), sexp z) {        \
    double d;                                                           \
    if (sexp_flonump(z))                                                \
      d = sexp_flonum_value(z);                                         \
    else if (sexp_fixnump(z))                                           \
      d = (double)sexp_unbox_fixnum(z);                                 \
    maybe_convert_bignum(z)                                             \
    else                                                                \
      return sexp_type_exception(ctx, self, SEXP_NUMBER, z);            \
    return sexp_make_flonum(ctx, cname(d));                             \
  }

define_math_op(sexp_exp, exp)
define_math_op(sexp_log, log)
define_math_op(sexp_sin, sin)
define_math_op(sexp_cos, cos)
define_math_op(sexp_tan, tan)
define_math_op(sexp_asin, asin)
define_math_op(sexp_acos, acos)
define_math_op(sexp_atan, atan)
define_math_op(sexp_round, round)
define_math_op(sexp_trunc, trunc)
define_math_op(sexp_floor, floor)
define_math_op(sexp_ceiling, ceil)

static sexp sexp_sqrt (sexp ctx sexp_api_params(self, n), sexp z) {
  double d, r;
  if (sexp_flonump(z))
    d = sexp_flonum_value(z);
  else if (sexp_fixnump(z))
    d = (double)sexp_unbox_fixnum(z);
  maybe_convert_bignum(z)       /* XXXX add bignum sqrt */
  else
    return sexp_type_exception(ctx, self, SEXP_NUMBER, z);
  r = sqrt(d);
  if (sexp_fixnump(z) && (((sexp_uint_t)r*(sexp_uint_t)r)==sexp_unbox_fixnum(z)))
    return sexp_make_fixnum(round(r));
  else
    return sexp_make_flonum(ctx, r);
}

#endif

static sexp sexp_expt_op (sexp ctx sexp_api_params(self, n), sexp x, sexp e) {
  long double f, x1, e1;
  sexp res;
#if SEXP_USE_BIGNUMS
  if (sexp_bignump(e)) {        /* bignum exponent needs special handling */
    if ((x == SEXP_ZERO) || (x == SEXP_NEG_ONE))
      res = sexp_make_flonum(ctx, pow(0, 0));          /* +nan.0 */
    else if (x == SEXP_ONE)
      res = SEXP_ONE;                                  /* 1.0    */
    else if (sexp_flonump(x))
      res = sexp_make_flonum(ctx, pow(sexp_flonum_value(x), sexp_bignum_to_double(e)));
    else
      res = sexp_make_flonum(ctx, pow(10.0, 1e100));   /* +inf.0 */
  } else if (sexp_bignump(x)) {
    res = sexp_bignum_expt(ctx, x, e);
  } else {
#endif
  if (sexp_fixnump(x))
    x1 = sexp_unbox_fixnum(x);
#if SEXP_USE_FLONUMS
  else if (sexp_flonump(x))
    x1 = sexp_flonum_value(x);
#endif
  else
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, x);
  if (sexp_fixnump(e))
    e1 = sexp_unbox_fixnum(e);
#if SEXP_USE_FLONUMS
  else if (sexp_flonump(e))
    e1 = sexp_flonum_value(e);
#endif
  else
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, e);
  f = pow(x1, e1);
  if ((f > SEXP_MAX_FIXNUM) || (f < SEXP_MIN_FIXNUM)
#if SEXP_USE_FLONUMS
      || (! sexp_fixnump(x)) || (! sexp_fixnump(e))
#endif
      ) {
#if SEXP_USE_BIGNUMS
    if (sexp_fixnump(x) && sexp_fixnump(e))
      res = sexp_bignum_expt(ctx, sexp_fixnum_to_bignum(ctx, x), e);
    else
#endif
#if SEXP_USE_FLONUMS
      res = sexp_make_flonum(ctx, f);
#else
      res = sexp_make_fixnum((sexp_sint_t)round(f));
#endif
  } else
    res = sexp_make_fixnum((sexp_sint_t)round(f));
#if SEXP_USE_BIGNUMS
  }
#endif
  return res;
}

static sexp sexp_string_cmp_op (sexp ctx sexp_api_params(self, n), sexp str1, sexp str2, sexp ci) {
  sexp_sint_t len1, len2, len, diff;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str1);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str2);
  len1 = sexp_string_length(str1);
  len2 = sexp_string_length(str2);
  len = ((len1<len2) ? len1 : len2);
  if (ci==SEXP_FALSE)
    diff = strncmp(sexp_string_data(str1), sexp_string_data(str2), len);
  else
    diff = strncasecmp(sexp_string_data(str1), sexp_string_data(str2), len);
  if (! diff)
    diff = len1 - len2;
  return sexp_make_fixnum(diff);
}

#if SEXP_USE_UTF8_STRINGS

static int sexp_utf8_initial_byte_count (int c) {
  if (c < 0xC0) return 1;
  if (c < 0xE0) return 2;
  return ((c>>4)&1)+3;
}

static int sexp_utf8_char_byte_count (int c) {
  if (c < 0x80) return 1;
  if (c < 0x800) return 2;
  if (c < 0x10000) return 3;
  return 4;
}

static int sexp_string_utf8_length (unsigned char *p, int len) {
  unsigned char *q = p+len;
  int i;
  for (i=0; p<q; i++)
    p += sexp_utf8_initial_byte_count(*p);
  return i;
}

static char* sexp_string_utf8_prev (unsigned char *p) {
  while ((*--p)>>6 == 2)
    ;
  return (char*)p;
}

sexp sexp_string_utf8_ref (sexp ctx, sexp str, sexp i) {
  unsigned char *p=(unsigned char*)sexp_string_data(str) + sexp_unbox_fixnum(i);
  if (*p < 0x80)
    return sexp_make_character(*p);
  else if ((*p < 0xC0) || (*p > 0xF7))
    return sexp_user_exception(ctx, NULL, "string-ref: invalid utf8 byte", i);
  else if (*p < 0xE0)
    return sexp_make_character(((p[0]&0x3F)<<6) + (p[1]&0x3F));
  else if (*p < 0xF0)
    return sexp_make_character(((p[0]&0x1F)<<12) + ((p[1]&0x3F)<<6) + (p[2]&0x3F));
  else
    return sexp_make_character(((p[0]&0x0F)<<16) + ((p[1]&0x3F)<<6) + ((p[2]&0x3F)<<6) + (p[2]&0x3F));
}

sexp sexp_string_utf8_index_ref (sexp ctx sexp_api_params(self, n), sexp str, sexp i) {
  sexp off;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, i);
  off = sexp_string_index_to_offset(ctx sexp_api_pass(self, n), str, i);
  if (sexp_exceptionp(off)) return off;
  return sexp_string_utf8_ref(ctx, str, off);
}

void sexp_write_utf8_char (sexp ctx, int c, sexp out) {
  unsigned char buf[8];
  int len = sexp_utf8_char_byte_count(c);
  sexp_utf8_encode_char(buf, len, c);
  buf[len] = 0;
  sexp_write_string(ctx, (char*)buf, out);
}

sexp sexp_read_utf8_char (sexp ctx, sexp port, int i) {
  if (i >= 0x80) {
    if ((i < 0xC0) || (i > 0xF7)) {
      return sexp_user_exception(ctx, NULL, "read-char: invalid utf8 byte", sexp_make_fixnum(i));
    } else if (i < 0xE0) {
      i = ((i&0x3F)<<6) + (sexp_read_char(ctx, port)&0x3F);
    } else if (i < 0xF0) {
      i = ((i&0x1F)<<12) + ((sexp_read_char(ctx, port)&0x3F)<<6);
      i += sexp_read_char(ctx, port)&0x3F;
    } else {
      i = ((i&0x0F)<<16) + ((sexp_read_char(ctx, port)&0x3F)<<6);
      i += (sexp_read_char(ctx, port)&0x3F)<<6;
      i += sexp_read_char(ctx, port)&0x3F;
    }
  }
  return sexp_make_character(i);
}

#if SEXP_USE_MUTABLE_STRINGS

void sexp_string_utf8_set (sexp ctx, sexp str, sexp index, sexp ch) {
  sexp b;
  unsigned char *p, *q;
  int i = sexp_unbox_fixnum(index), c = sexp_unbox_character(ch),
    old_len, new_len, len;
  p = (unsigned char*)sexp_string_data(str) + i;
  old_len = sexp_utf8_initial_byte_count(*p);
  new_len = sexp_utf8_char_byte_count(c);
  if (old_len != new_len) { /* resize bytes if needed */
    len = sexp_string_length(str)+(new_len-old_len);
    b = sexp_make_bytes(ctx, sexp_make_fixnum(len), SEXP_VOID);
    if (! sexp_exceptionp(b)) {
      q = (unsigned char*)sexp_bytes_data(b);
      memcpy(q, sexp_string_data(str), i);
      memcpy(q+i+new_len, p+old_len, len-i-new_len+1);
      sexp_string_bytes(str) = b;
      p = q + i;
    }
    sexp_string_length(str) += new_len - old_len;
  }
  sexp_utf8_encode_char(p, new_len, c);
}

sexp sexp_string_utf8_index_set (sexp ctx sexp_api_params(self, n), sexp str, sexp i, sexp ch) {
  sexp off;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, i);
  sexp_assert_type(ctx, sexp_charp, SEXP_CHAR, ch);
  off = sexp_string_index_to_offset(ctx sexp_api_pass(self, n), str, i);
  if (sexp_exceptionp(off)) return off;
  sexp_string_utf8_set(ctx, str, off, ch);
  return SEXP_VOID;
}

#endif
#endif

#if SEXP_USE_AUTO_FORCE
sexp sexp_make_promise (sexp ctx sexp_api_params(self, n), sexp thunk) {
  sexp_assert_type(ctx, sexp_applicablep, SEXP_PROCEDURE, thunk);
  sexp res = sexp_alloc_type(ctx, promise, SEXP_PROMISE);
  sexp_promise_donep(res) = 0;
  sexp_promise_thunk(res) = thunk;
  sexp_promise_value(res) = SEXP_VOID;
  return res;
}
#endif

#ifdef PLAN9
#include "opt/plan9.c"
#endif

/************************** optimizations *****************************/

#if SEXP_USE_SIMPLIFY
#include "opt/simplify.c"
#endif

/***************************** opcodes ********************************/

#if SEXP_USE_TYPE_DEFS

sexp sexp_type_slot_offset_op (sexp ctx  sexp_api_params(self, n), sexp type, sexp slot) {
  sexp cpl, slots, *v;
  int i, offset=0, len;
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, type);
  cpl = sexp_type_cpl(type);
  if (sexp_vectorp(cpl)) {
    v = sexp_vector_data(cpl);
    len = sexp_vector_length(cpl);
  } else {
    v = &sexp_type_slots(type);
    len = 1;
  }
  len = sexp_vectorp(cpl) ? sexp_vector_length(cpl) : 1;
  for (i=0; i<len; i++)
    for (slots=sexp_type_slots(v[i]); sexp_pairp(slots); slots=sexp_cdr(slots), offset++)
      if (sexp_car(slots) == slot)
        return sexp_make_fixnum(offset);
  return SEXP_FALSE;
}

sexp sexp_make_type_predicate_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type) {
  if (sexp_typep(type)) type = sexp_make_fixnum(sexp_type_tag(type));
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, type);
  return sexp_make_opcode(ctx, self, name, sexp_make_fixnum(SEXP_OPC_TYPE_PREDICATE),
                          sexp_make_fixnum(SEXP_OP_TYPEP), SEXP_ONE, SEXP_ZERO,
                          SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, type, NULL, NULL);
}

sexp sexp_make_constructor_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type) {
  sexp_uint_t type_size;
  if (sexp_typep(type)) type = sexp_make_fixnum(sexp_type_tag(type));
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, type);
  type_size = sexp_type_size_base(sexp_type_by_index(ctx, sexp_unbox_fixnum(type)));
  return sexp_make_opcode(ctx, self, name, sexp_make_fixnum(SEXP_OPC_CONSTRUCTOR),
                          sexp_make_fixnum(SEXP_OP_MAKE), SEXP_ZERO, SEXP_ZERO,
                          SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, type,
                          sexp_make_fixnum(type_size), NULL);
}

sexp sexp_make_getter_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type, sexp index) {
  if (sexp_typep(type)) type = sexp_make_fixnum(sexp_type_tag(type));
  if ((! sexp_fixnump(type))  || (sexp_unbox_fixnum(type) < 0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, type);
  if ((! sexp_fixnump(index)) || (sexp_unbox_fixnum(index) < 0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, index);
  return
    sexp_make_opcode(ctx, self, name, sexp_make_fixnum(SEXP_OPC_GETTER),
                     sexp_make_fixnum(SEXP_OP_SLOT_REF), SEXP_ONE, SEXP_ZERO,
                     type, SEXP_ZERO, SEXP_ZERO, type, index, NULL);
}

sexp sexp_make_setter_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type, sexp index) {
  if (sexp_typep(type)) type = sexp_make_fixnum(sexp_type_tag(type));
  if ((! sexp_fixnump(type))  || (sexp_unbox_fixnum(type) < 0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, type);
  if ((! sexp_fixnump(index)) || (sexp_unbox_fixnum(index) < 0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, index);
  return
    sexp_make_opcode(ctx, self, name, sexp_make_fixnum(SEXP_OPC_SETTER),
                     sexp_make_fixnum(SEXP_OP_SLOT_SET), SEXP_TWO, SEXP_ZERO,
                     type, SEXP_ZERO, SEXP_ZERO, type, index, NULL);
}

#endif

#include "opcodes.c"

static sexp sexp_copy_core (sexp ctx, struct sexp_core_form_struct *core) {
  sexp res = sexp_alloc_type(ctx, core, SEXP_CORE);
  memcpy(&(res->value), core, sizeof(core[0]));
  return res;
}

static sexp sexp_copy_opcode (sexp ctx, struct sexp_opcode_struct *op) {
  sexp res = sexp_alloc_type(ctx, opcode, SEXP_OPCODE);
  memcpy(&(res->value), op, sizeof(op[0]));
  return res;
}

sexp sexp_make_opcode (sexp ctx, sexp self, sexp name, sexp op_class, sexp code,
                       sexp num_args, sexp flags, sexp arg1t, sexp arg2t,
                       sexp invp, sexp data, sexp data2, sexp_proc1 func) {
  sexp res;
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, num_args);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, flags);
  if ((! sexp_fixnump(op_class)) || (sexp_unbox_fixnum(op_class) <= 0)
      || (sexp_unbox_fixnum(op_class) >= SEXP_OPC_NUM_OP_CLASSES))
    res = sexp_xtype_exception(ctx, self, "make-opcode: bad opcode class", op_class);
  else if ((! sexp_fixnump(code)) || (sexp_unbox_fixnum(code) <= 0)
      || (sexp_unbox_fixnum(code) >= SEXP_OP_NUM_OPCODES))
    res = sexp_xtype_exception(ctx, self, "make-opcode: bad opcode", code);
  else {
    res = sexp_alloc_type(ctx, opcode, SEXP_OPCODE);
    sexp_opcode_class(res) = sexp_unbox_fixnum(op_class);
    sexp_opcode_code(res) = sexp_unbox_fixnum(code);
    sexp_opcode_num_args(res) = sexp_unbox_fixnum(num_args);
    sexp_opcode_flags(res) = sexp_unbox_fixnum(flags);
    sexp_opcode_arg1_type(res) = arg1t;
    sexp_opcode_arg2_type(res) = arg2t;
    sexp_opcode_inverse(res) = sexp_unbox_fixnum(invp);
    sexp_opcode_data(res) = data;
    sexp_opcode_data2(res) = data2;
    sexp_opcode_func(res) = func;
    sexp_opcode_name(res) = sexp_stringp(name) ? strdup(sexp_string_data(name)) : "";
  }
  return res;
}

sexp sexp_make_foreign (sexp ctx, const char *name, int num_args,
                        int flags, sexp_proc1 f, sexp data) {
  sexp res;
#if ! SEXP_USE_EXTENDED_FCALL
  if (num_args > 4)
    return sexp_user_exception(ctx, NULL, "make-foreign: exceeded foreign arg limit",
                               sexp_make_fixnum(num_args));
#endif
  res = sexp_alloc_type(ctx, opcode, SEXP_OPCODE);
  sexp_opcode_class(res) = SEXP_OPC_FOREIGN;
#if SEXP_USE_EXTENDED_FCALL
  if (num_args > 4)
    sexp_opcode_code(res) = SEXP_OP_FCALLN;
  else
#endif
    sexp_opcode_code(res) = SEXP_OP_FCALL1+num_args-1;
  if (flags & 1) num_args--;
  sexp_opcode_num_args(res) = num_args;
  sexp_opcode_flags(res) = flags;
  sexp_opcode_name(res) = name;
  sexp_opcode_data(res) = data;
  sexp_opcode_func(res) = f;
  return res;
}

sexp sexp_define_foreign_aux (sexp ctx, sexp env, const char *name, int num_args,
                              int flags, sexp_proc1 f, sexp data) {
  sexp_gc_var2(op, res);
  sexp_gc_preserve2(ctx, op, res);
  op = sexp_make_foreign(ctx, name, num_args, flags, f, data);
  if (sexp_exceptionp(op))
    res = op;
  else
    sexp_env_define(ctx, env, res = sexp_intern(ctx, name, -1), op);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_define_foreign_param (sexp ctx, sexp env, const char *name,
                                int num_args, sexp_proc1 f, const char *param) {
  sexp res = SEXP_FALSE;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  tmp = sexp_intern(ctx, param, -1);
  tmp = sexp_env_ref(env, tmp, SEXP_FALSE);
  if (sexp_opcodep(tmp))
    res = sexp_define_foreign_aux(ctx, env, name, num_args, 3, f, tmp);
  sexp_gc_release1(ctx);
  return res;
}

#if SEXP_USE_STATIC_LIBS
#include "clibs.c"
#endif

/*********************** standard environment *************************/

static struct sexp_core_form_struct core_forms[] = {
  {SEXP_CORE_DEFINE, "define"},
  {SEXP_CORE_SET, "set!"},
  {SEXP_CORE_LAMBDA, "lambda"},
  {SEXP_CORE_IF, "if"},
  {SEXP_CORE_BEGIN, "begin"},
  {SEXP_CORE_QUOTE, "quote"},
  {SEXP_CORE_SYNTAX_QUOTE, "syntax-quote"},
  {SEXP_CORE_DEFINE_SYNTAX, "define-syntax"},
  {SEXP_CORE_LET_SYNTAX, "let-syntax"},
  {SEXP_CORE_LETREC_SYNTAX, "letrec-syntax"},
};

sexp sexp_make_env_op (sexp ctx sexp_api_params(self, n)) {
  sexp e = sexp_alloc_type(ctx, env, SEXP_ENV);
  sexp_env_lambda(e) = NULL;
  sexp_env_parent(e) = NULL;
  sexp_env_bindings(e) = SEXP_NULL;
#if SEXP_USE_RENAME_BINDINGS
  sexp_env_renames(e) = SEXP_NULL;
#endif
  return e;
}

sexp sexp_make_null_env_op (sexp ctx sexp_api_params(self, n), sexp version) {
  sexp_uint_t i;
  sexp_gc_var2(e, core);
  sexp_gc_preserve2(ctx, e, core);
  e = sexp_make_env(ctx);
  for (i=0; i<(sizeof(core_forms)/sizeof(core_forms[0])); i++) {
    core = sexp_copy_core(ctx, &core_forms[i]);
    sexp_env_define(ctx, e, sexp_intern(ctx, sexp_core_name(core), -1), core);
  }
  sexp_gc_release2(ctx);
  return e;
}

sexp sexp_make_primitive_env (sexp ctx, sexp version) {
  int i;
  sexp_gc_var4(e, op, sym, name);
  sexp_gc_preserve4(ctx, e, op, sym, name);
  e = sexp_make_null_env(ctx, version);
  for (i=0; i<(sizeof(opcodes)/sizeof(opcodes[0])); i++) {
    op = sexp_copy_opcode(ctx, &opcodes[i]);
    name = sexp_intern(ctx, sexp_opcode_name(op), -1);
    if (sexp_opcode_opt_param_p(op) && sexp_opcode_data(op)) {
      sym = sexp_intern(ctx, (char*)sexp_opcode_data(op), -1);
      sexp_opcode_data(op) = sexp_env_ref(e, sym, SEXP_FALSE);
    } else if (sexp_opcode_class(op) == SEXP_OPC_PARAMETER) {
      sexp_opcode_data(op) = sexp_cons(ctx, name, SEXP_FALSE);
    }
    sexp_env_define(ctx, e, name, op);
  }
  sexp_gc_release4(ctx);
  return e;
}

sexp sexp_find_module_file (sexp ctx, const char *file) {
  sexp res=SEXP_FALSE, ls;
  char *dir, *path;
  sexp_uint_t slash, dirlen, filelen, len;
#ifdef PLAN9
#define file_exists_p(path, buf) (stat(path, buf, 128) >= 0)
  unsigned char buf[128];
#else
#define file_exists_p(path, buf) (! stat(path, buf))
  struct stat buf_str;
  struct stat *buf = &buf_str;
#endif

  filelen = strlen(file);

  ls = sexp_global(ctx, SEXP_G_MODULE_PATH);
  for ( ; sexp_pairp(ls) && sexp_not(res); ls=sexp_cdr(ls)) {
    dir = sexp_string_data(sexp_car(ls));
    dirlen = sexp_string_length(sexp_car(ls));
    slash = dir[dirlen-1] == '/';
    len = dirlen+filelen+2-slash;
    path = (char*) malloc(len);
    if (! path) return sexp_global(ctx, SEXP_G_OOM_ERROR);
    memcpy(path, dir, dirlen);
    if (! slash) path[dirlen] = '/';
    memcpy(path+len-filelen-1, file, filelen);
    path[len-1] = '\0';
    if (file_exists_p(path, buf))
      res = sexp_c_string(ctx, path, len-1);
    free(path);
  }

  return res;
}

#define sexp_file_not_found "couldn't find file in module path"

sexp sexp_load_module_file (sexp ctx, const char *file, sexp env) {
  sexp res;
  sexp_gc_var1(path);
  sexp_gc_preserve1(ctx, path);
  path = sexp_find_module_file(ctx, file);
  if (sexp_stringp(path)) {
    res = sexp_load(ctx, path, env);
  } else {
    path = sexp_c_string(ctx, file, -1);
    res = sexp_user_exception(ctx, SEXP_FALSE, sexp_file_not_found, path);
  }
  sexp_gc_release1(ctx);
  return res;
}

#if SEXP_USE_MODULES
sexp sexp_find_module_file_op (sexp ctx sexp_api_params(self, n), sexp file) {
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, file);
  return sexp_find_module_file(ctx, sexp_string_data(file));
}
sexp sexp_load_module_file_op (sexp ctx sexp_api_params(self, n), sexp file, sexp env) {
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, file);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  return sexp_load_module_file(ctx, sexp_string_data(file), env);
}
sexp sexp_current_environment (sexp ctx sexp_api_params(self, n)) {
  return sexp_context_env(ctx);
}
#endif

sexp sexp_add_module_directory_op (sexp ctx sexp_api_params(self, n), sexp dir, sexp appendp) {
  sexp ls;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, dir);
  if (sexp_truep(appendp)) {
    if (sexp_pairp(ls=sexp_global(ctx, SEXP_G_MODULE_PATH))) {
      for ( ; sexp_pairp(sexp_cdr(ls)); ls=sexp_cdr(ls))
        ;
      sexp_cdr(ls) = sexp_list1(ctx, dir);
    } else {
      sexp_global(ctx, SEXP_G_MODULE_PATH) = sexp_list1(ctx, dir);
    }
  } else {
    sexp_push(ctx, sexp_global(ctx, SEXP_G_MODULE_PATH), dir);
  }
  return SEXP_VOID;
}

sexp sexp_parameter_ref (sexp ctx, sexp param) {
#if SEXP_USE_GREEN_THREADS
  sexp ls;
  for (ls=sexp_context_params(ctx); sexp_pairp(ls); ls=sexp_cdr(ls))
    if (sexp_caar(ls) == param)
      return sexp_cdar(ls);
#endif
  return sexp_opcodep(param) && sexp_opcode_data(param) && sexp_pairp(sexp_opcode_data(param))
    ? sexp_cdr(sexp_opcode_data(param)) : SEXP_FALSE;
}

void sexp_set_parameter (sexp ctx, sexp env, sexp name, sexp value) {
  sexp param = sexp_env_ref(env, name, SEXP_FALSE);
  if (sexp_opcodep(param)) {
    if (! sexp_pairp(sexp_opcode_data(param)))
      sexp_opcode_data(param) = sexp_cons(ctx, name, value);
    else
      sexp_cdr(sexp_opcode_data(param)) = value;
  }
}

sexp sexp_load_standard_ports (sexp ctx, sexp env, FILE* in, FILE* out,
                               FILE* err, int no_close) {
  sexp_gc_var1(p);
  sexp_gc_preserve1(ctx, p);
  if (!env) env = sexp_context_env(ctx);
  if (in) {
    p = sexp_make_input_port(ctx, in, SEXP_FALSE);
    sexp_port_no_closep(p) = no_close;
    sexp_set_parameter(ctx, env, sexp_global(ctx, SEXP_G_CUR_IN_SYMBOL), p);
  }
  if (out) {
    p = sexp_make_output_port(ctx, out, SEXP_FALSE);
    sexp_port_no_closep(p) = no_close;
    sexp_set_parameter(ctx, env, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL), p);
  }
  if (err) {
    p = sexp_make_output_port(ctx, err, SEXP_FALSE);
    sexp_port_no_closep(p) = no_close;
    sexp_set_parameter(ctx, env, sexp_global(ctx, SEXP_G_CUR_ERR_SYMBOL), p);
  }
  sexp_gc_release1(ctx);
  return SEXP_VOID;
}

sexp sexp_load_standard_env (sexp ctx, sexp e, sexp version) {
  sexp_gc_var3(op, tmp, sym);
  sexp_gc_preserve3(ctx, op, tmp, sym);
  if (!e) e = sexp_context_env(ctx);
#if SEXP_USE_DL
  sexp_env_define(ctx, e, sym=sexp_intern(ctx, "*shared-object-extension*", -1),
                  tmp=sexp_c_string(ctx, sexp_so_extension, -1));
#endif
  tmp = sexp_list1(ctx, sym=sexp_intern(ctx, sexp_platform, -1));
#if SEXP_BSD
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "bsd", -1));
#endif
#if SEXP_USE_DL
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "dynamic-loading", -1));
#endif
#if SEXP_USE_MODULES
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "modules", -1));
#endif
#if SEXP_USE_BOEHM
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "boehm-gc", -1));
#endif
#if SEXP_USE_UTF8_STRINGS
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "utf-8", -1));
#endif
#if SEXP_USE_GREEN_THREADS
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "threads", -1));
#endif
#if SEXP_USE_AUTO_FORCE
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "auto-force", -1));
#endif
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "chibi", -1));
  sexp_env_define(ctx, e, sym=sexp_intern(ctx, "*features*", -1), tmp);
  sexp_global(ctx, SEXP_G_OPTIMIZATIONS) = SEXP_NULL;
#if SEXP_USE_SIMPLIFY
  op = sexp_make_foreign(ctx, "simplify", 1, 0,
                         (sexp_proc1)sexp_simplify, SEXP_VOID);
  tmp = sexp_cons(ctx, sexp_make_fixnum(500), op);
  sexp_push(ctx, sexp_global(ctx, SEXP_G_OPTIMIZATIONS), tmp);
#endif
  sexp_global(ctx, SEXP_G_ERR_HANDLER)
    = sexp_env_ref(e, sym=sexp_intern(ctx, "current-exception-handler", -1), SEXP_FALSE);
  /* load init.scm */
  tmp = sexp_load_module_file(ctx, sexp_init_file, e);
  sexp_set_parameter(ctx, e, sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL), e);
  /* load and bind config env */
#if SEXP_USE_MODULES
  if (! sexp_exceptionp(tmp)) {
    sym = sexp_intern(ctx, "*config-env*", -1);
    if (! sexp_envp(tmp=sexp_global(ctx, SEXP_G_CONFIG_ENV))) {
      tmp = sexp_make_env(ctx);
      if (! sexp_exceptionp(tmp)) {
        sexp_global(ctx, SEXP_G_CONFIG_ENV) = tmp;
        sexp_env_parent(tmp) = e;
        op = sexp_load_module_file(ctx, sexp_config_file, tmp);
        if (sexp_exceptionp(op))
          sexp_print_exception(ctx, op, sexp_current_error_port(ctx));
        sexp_env_define(ctx, tmp, sym, tmp);
      }
    }
    sexp_env_define(ctx, e, sym, tmp);
  }
#endif
#if SEXP_USE_STATIC_LIBS
  sexp_init_all_libraries(ctx, e);
#endif
  sexp_gc_release3(ctx);
  return sexp_exceptionp(tmp) ? tmp : e;
}

sexp sexp_make_standard_env_op (sexp ctx sexp_api_params(self, n), sexp version) {
  sexp_gc_var1(env);
  sexp_gc_preserve1(ctx, env);
  env = sexp_make_primitive_env(ctx, version);
  if (! sexp_exceptionp(env)) sexp_load_standard_env(ctx, env, version);
  sexp_gc_release1(ctx);
  return env;
}

#if SEXP_USE_RENAME_BINDINGS
#define sexp_same_bindingp(x, y) ((x) == (y))
#else
#define sexp_same_bindingp(x, y) (sexp_env_value(x) == sexp_env_value(y))
#endif

sexp sexp_env_import_op (sexp ctx sexp_api_params(self, n), sexp to, sexp from, sexp ls, sexp immutp) {
  sexp oldname, newname;
  sexp_gc_var2(value, oldcell);
  sexp_gc_preserve2(ctx, value, oldcell);
  if (! sexp_envp(to)) to = sexp_context_env(ctx);
  if (! sexp_envp(from)) from = sexp_context_env(ctx);
  value = sexp_make_env(ctx);
  sexp_env_parent(value) = sexp_env_parent(to);
  sexp_env_parent(to) = value;
  sexp_immutablep(value) = sexp_truep(immutp);
  if (sexp_not(ls)) {
    sexp_env_bindings(value) = sexp_env_bindings(from);
#if SEXP_USE_RENAME_BINDINGS
    sexp_env_renames(value) = sexp_env_renames(from);
#endif
  } else {
    for ( ; sexp_pairp(ls); ls=sexp_cdr(ls)) {
      if (sexp_pairp(sexp_car(ls))) {
        newname = sexp_caar(ls); oldname = sexp_cdar(ls);
      } else {
        newname = oldname = sexp_car(ls);
      }
      oldcell = sexp_env_cell(to, newname, 0);
      value = sexp_env_cell(from, oldname, 0);
      if (value) {
#if SEXP_USE_RENAME_BINDINGS
        sexp_env_rename(ctx, to, newname, value);
#else
        sexp_env_define(ctx, to, newname, sexp_cdr(value));
#endif
#if SEXP_USE_WARN_UNDEFS
        if (oldcell && !sexp_same_bindingp(oldcell, value))
          sexp_warn(ctx, "importing already defined binding: ", newname);
      } else {
        sexp_warn(ctx, "importing undefined variable: ", oldname);
#endif
      }
    }
  }
  sexp_gc_release2(ctx);
  return SEXP_VOID;
}

/************************* backend ***************************/

#if SEXP_USE_NATIVE_X86
#include "opt/x86.c"
#else
#include "vm.c"
#endif

/************************** eval interface ****************************/

sexp sexp_compile_op (sexp ctx sexp_api_params(self, n), sexp obj, sexp env) {
  sexp_gc_var3(ast, vec, res);
  sexp ctx2;
  if (! env) env = sexp_context_env(ctx);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  sexp_gc_preserve3(ctx, ast, vec, res);
  ctx2 = sexp_make_eval_context(ctx, NULL, env, 0, 0);
  sexp_context_child(ctx) = ctx2;
  ast = sexp_analyze(ctx2, obj);
  if (sexp_exceptionp(ast)) {
    res = ast;
  } else {
    res = sexp_global(ctx2, SEXP_G_OPTIMIZATIONS);
    for ( ; sexp_pairp(res); res=sexp_cdr(res))
      ast = sexp_apply1(ctx2, sexp_cdar(res), ast);
    sexp_free_vars(ctx2, ast, SEXP_NULL);    /* should return SEXP_NULL */
    emit_enter(ctx2);
    generate(ctx2, ast);
    res = finalize_bytecode(ctx2);
    vec = sexp_make_vector(ctx2, 0, SEXP_VOID);
    res = sexp_make_procedure(ctx2, SEXP_ZERO, SEXP_ZERO, res, vec);
  }
  sexp_context_child(ctx) = SEXP_FALSE;
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_eval_op (sexp ctx sexp_api_params(self, n), sexp obj, sexp env) {
  sexp_sint_t top;
  sexp ctx2;
  sexp_gc_var2(res, params);
  if (! env) env = sexp_context_env(ctx);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  sexp_gc_preserve2(ctx, res, params);
  top = sexp_context_top(ctx);
  params = sexp_context_params(ctx);
  sexp_context_params(ctx) = SEXP_NULL;
  ctx2 = sexp_make_eval_context(ctx, NULL, env, 0, 0);
  sexp_context_child(ctx) = ctx2;
  res = sexp_compile_op(ctx2, self, n, obj, env);
  if (! sexp_exceptionp(res))
    res = sexp_apply(ctx2, res, SEXP_NULL);
  sexp_context_child(ctx) = SEXP_FALSE;
  sexp_context_params(ctx) = params;
  sexp_context_top(ctx) = top;
  sexp_context_last_fp(ctx) = sexp_context_last_fp(ctx2);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_eval_string (sexp ctx, const char *str, sexp_sint_t len, sexp env) {
  sexp res;
  sexp_gc_var1(obj);
  sexp_gc_preserve1(ctx, obj);
  obj = sexp_read_from_string(ctx, str, len);
  res = sexp_eval(ctx, obj, env);
  sexp_gc_release1(ctx);
  return res;
}

void sexp_scheme_init (void) {
  if (! scheme_initialized_p) {
    scheme_initialized_p = 1;
    sexp_init();
  }
}
