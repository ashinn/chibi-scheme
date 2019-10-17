/*  eval.c -- evaluator library implementation                */
/*  Copyright (c) 2009-2015 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"

#if SEXP_USE_DEBUG_VM || SEXP_USE_PROFILE_VM || SEXP_USE_STATIC_LIBS
#include "opt/opcode_names.h"
#endif

/************************************************************************/

static int scheme_initialized_p = 0;

static sexp analyze (sexp ctx, sexp x, int depth, int defok);

#if SEXP_USE_MODULES
sexp sexp_load_module_file_op (sexp ctx, sexp self, sexp_sint_t n, sexp file, sexp env);
sexp sexp_find_module_file_op (sexp ctx, sexp self, sexp_sint_t n, sexp file);
sexp sexp_current_environment (sexp ctx, sexp self, sexp_sint_t n);
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

void sexp_warn (sexp ctx, const char *msg, sexp x) {
  sexp_gc_var1(out);
  int strictp = sexp_truep(sexp_global(ctx, SEXP_G_STRICT_P));
  sexp_gc_preserve1(ctx, out);
  out = sexp_current_error_port(ctx);
  if (sexp_not(out)) {          /* generate a throw-away port */
    out = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
    sexp_port_no_closep(out) = 1;
  }
  if (sexp_oportp(out)) {
    sexp_write_string(ctx, strictp ? "ERROR: " : "WARNING: ", out);
    sexp_write_string(ctx, msg, out);
    sexp_write(ctx, x, out);
    sexp_write_char(ctx, '\n', out);
    if (strictp) sexp_stack_trace(ctx, out);
  }
  sexp_gc_release1(ctx);
  if (strictp) exit(1);
}

sexp sexp_warn_undefs_op (sexp ctx, sexp self, sexp_sint_t n, sexp from, sexp to, sexp res) {
  sexp x, ignore = (res && sexp_exceptionp(res)) ? sexp_exception_irritants(res) : SEXP_NULL;
  if (sexp_envp(from)) from = sexp_env_bindings(from);
  for (x=from; sexp_pairp(x) && x!=to; x=sexp_env_next_cell(x))
    if (sexp_cdr(x) == SEXP_UNDEF && sexp_car(x) != ignore
        && !sexp_synclop(sexp_car(x))
        && sexp_not(sexp_memq(ctx, sexp_car(x), ignore)))
      sexp_warn(ctx, "reference to undefined variable: ", sexp_car(x));
  return SEXP_VOID;
}

sexp sexp_maybe_wrap_error (sexp ctx, sexp obj) {
  sexp_gc_var2(tmp, res);
  if (sexp_exceptionp(obj)) {
    sexp_gc_preserve2(ctx, tmp, res);
    tmp = obj;
    tmp = sexp_list1(ctx, tmp);
    res = sexp_make_trampoline(ctx, SEXP_FALSE, tmp);
    sexp_gc_release2(ctx);
    return res;
  }
  return obj;
}

/********************** environment utilities ***************************/

static sexp sexp_env_cell_loc1 (sexp env, sexp key, int localp, sexp *varenv) {
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
    if (localp) break;
    env = sexp_env_parent(env);
  } while (env && sexp_envp(env));
  return NULL;
}

static sexp sexp_env_cell_loc (sexp ctx, sexp env, sexp key, int localp, sexp *varenv) {
  sexp cell, ls = sexp_vectorp(sexp_context_specific(ctx)) ? sexp_memq(ctx, sexp_id_name(key), sexp_context_fv(ctx)) : SEXP_NULL;
  for ( ; sexp_pairp(ls); ls=sexp_cdr(ls))
    if (sexp_envp(sexp_car(ls))) {
      env = sexp_car(ls);
      break;
    }
  cell = sexp_env_cell_loc1(env, key, localp, varenv);
  while (!cell && key && sexp_synclop(key)) {
    if (!sexp_pairp(ls) && sexp_not(sexp_memq(ctx, sexp_synclo_expr(key), sexp_synclo_free_vars(key))))
      env = sexp_synclo_env(key);
    key = sexp_synclo_expr(key);
    cell = sexp_env_cell_loc1(env, key, localp, varenv);
  }
  return cell;
}

sexp sexp_env_cell (sexp ctx, sexp env, sexp key, int localp) {
  return sexp_env_cell_loc(ctx, env, key, localp, NULL);
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

sexp sexp_env_cell_define (sexp ctx, sexp env, sexp key,
                           sexp value, sexp *varenv) {
  sexp_gc_var2(cell, ls);
  while (sexp_env_lambda(env) || sexp_env_syntactic_p(env))
    env = sexp_env_parent(env);
  if (varenv) *varenv = env;
#if SEXP_USE_RENAME_BINDINGS
  /* remove any existing renamed definition */
  for (ls=sexp_env_renames(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
    if (sexp_car(ls) == key) {
      sexp_car(ls) = SEXP_FALSE;
      break;
    }
#endif
  for (ls=sexp_env_bindings(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
    if (sexp_car(ls) == key) {
      sexp_cdr(ls) = value;
      return ls;
    }
  sexp_gc_preserve2(ctx, cell, ls);
  sexp_env_push(ctx, env, cell, key, value);
  sexp_gc_release2(ctx);
  return cell;
}

static sexp sexp_env_cell_create (sexp ctx, sexp env, sexp key,
                                  sexp value, sexp *varenv) {
  sexp cell = sexp_env_cell_loc(ctx, env, key, 0, varenv);
  if (!cell) cell = sexp_env_cell_define(ctx, env, key, value, varenv);
  return cell;
}

sexp sexp_env_ref (sexp ctx, sexp env, sexp key, sexp dflt) {
  sexp cell = sexp_env_cell(ctx, env, key, 0);
  return (cell ? sexp_cdr(cell) : dflt);
}

sexp sexp_env_define (sexp ctx, sexp env, sexp key, sexp value) {
  sexp cell, tmp, res = SEXP_VOID;
  if (sexp_immutablep(env))
    return sexp_user_exception(ctx, NULL, "immutable binding", key);
  cell = sexp_env_cell(ctx, env, key, 1);
  if (!cell) {
    while (sexp_env_syntactic_p(env) && sexp_env_parent(env))
      env = sexp_env_parent(env);
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

sexp sexp_env_exports_op (sexp ctx, sexp self, sexp_sint_t n, sexp env) {
  sexp ls;
  sexp_gc_var1(res);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_NULL;
#if SEXP_USE_RENAME_BINDINGS
  for (ls=sexp_env_renames(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
    sexp_push(ctx, res, sexp_cadr(ls));
#endif
  for (ls=sexp_env_bindings(env); sexp_pairp(ls); ls=sexp_env_next_cell(ls))
    if (sexp_env_value(ls) != SEXP_UNDEF)
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
    if (!e2) { return sexp_global(ctx, SEXP_G_OOM_ERROR); }
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

int sexp_param_index (sexp ctx, sexp lambda, sexp name) {
  sexp ls;
  int i;
  while (1) {
    ls = sexp_lambda_params(lambda);
    for (i=0; sexp_pairp(ls); ls=sexp_cdr(ls), i++)
      if (sexp_car(ls) == name)
        return i;
    if (ls == name)
      return i;
    ls = sexp_lambda_locals(lambda);
    for (i=-1; sexp_pairp(ls); ls=sexp_cdr(ls), i--)
      if (sexp_car(ls) == name)
        return i-4;
    if (sexp_synclop(name))
      name = sexp_synclo_expr(name);
    else
      break;
  }
  sexp_warn(ctx, "can't happen: no argument: ", name);
  return -10000;
}

/************************* bytecode utilities ***************************/

void sexp_shrink_bcode (sexp ctx, sexp_uint_t i) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(ctx)) != i) {
    tmp = sexp_alloc_bytecode(ctx, i);
    if (!sexp_exceptionp(tmp)) {
      sexp_bytecode_name(tmp) = sexp_bytecode_name(sexp_context_bc(ctx));
      sexp_bytecode_length(tmp) = i;
      sexp_bytecode_literals(tmp)
        = sexp_bytecode_literals(sexp_context_bc(ctx));
      sexp_bytecode_source(tmp)
        = sexp_bytecode_source(sexp_context_bc(ctx));
      memcpy(sexp_bytecode_data(tmp),
             sexp_bytecode_data(sexp_context_bc(ctx)),
             i);
      sexp_context_bc(ctx) = tmp;
    }
  }
}

void sexp_expand_bcode (sexp ctx, sexp_sint_t size) {
  sexp tmp;
  if ((sexp_sint_t)sexp_bytecode_length(sexp_context_bc(ctx))
      < (sexp_unbox_fixnum(sexp_context_pos(ctx)))+size) {
    tmp=sexp_alloc_bytecode(ctx, sexp_bytecode_length(sexp_context_bc(ctx))*2);
    if (sexp_exceptionp(tmp)) {
      sexp_context_exception(ctx) = tmp;
    } else {
      sexp_bytecode_name(tmp) = sexp_bytecode_name(sexp_context_bc(ctx));
      sexp_bytecode_length(tmp)
        = sexp_bytecode_length(sexp_context_bc(ctx))*2;
      sexp_bytecode_literals(tmp)
        = sexp_bytecode_literals(sexp_context_bc(ctx));
      sexp_bytecode_source(tmp)
        = sexp_bytecode_source(sexp_context_bc(ctx));
      memcpy(sexp_bytecode_data(tmp),
             sexp_bytecode_data(sexp_context_bc(ctx)),
             sexp_bytecode_length(sexp_context_bc(ctx)));
      sexp_context_bc(ctx) = tmp;
    }
  }
}

void sexp_emit (sexp ctx, unsigned char c)  {
  sexp_expand_bcode(ctx, 1);
  if (sexp_exceptionp(sexp_context_exception(ctx)))
    return;
  sexp_bytecode_data(sexp_context_bc(ctx))[sexp_unbox_fixnum(sexp_context_pos(ctx))] = c;
  sexp_context_pos(ctx) = sexp_fx_add(sexp_context_pos(ctx), SEXP_ONE);
}

sexp sexp_complete_bytecode (sexp ctx) {
  sexp bc;
  sexp_emit_return(ctx);
  sexp_shrink_bcode(ctx, sexp_unbox_fixnum(sexp_context_pos(ctx)));
  bc = sexp_context_bc(ctx);
  if (sexp_pairp(sexp_bytecode_literals(bc))) { /* compress literals */
    if (sexp_nullp(sexp_cdr(sexp_bytecode_literals(bc))))
      sexp_bytecode_literals(bc) = sexp_car(sexp_bytecode_literals(bc));
    else if (sexp_nullp(sexp_cddr(sexp_bytecode_literals(bc))))
      sexp_cdr(sexp_bytecode_literals(bc)) = sexp_cadr(sexp_bytecode_literals(bc));
    else
      sexp_bytecode_literals(bc) = sexp_list_to_vector(ctx, sexp_bytecode_literals(bc));
    if (sexp_exceptionp(sexp_bytecode_literals(bc)))
      return sexp_bytecode_literals(bc);
  }
  sexp_bytecode_max_depth(bc) = sexp_unbox_fixnum(sexp_context_max_depth(ctx));
#if SEXP_USE_FULL_SOURCE_INFO
  if (sexp_bytecode_source(bc) && sexp_pairp(sexp_bytecode_source(bc))) {
    sexp_bytecode_source(bc) = sexp_nreverse(ctx, sexp_bytecode_source(bc));
    sexp_bytecode_source(bc) = sexp_list_to_vector(ctx, sexp_bytecode_source(bc));
  }
#endif
  sexp_bless_bytecode(ctx, bc);
  if (sexp_exceptionp(sexp_context_exception(ctx)))
    return sexp_context_exception(ctx);
  return bc;
}

sexp sexp_make_procedure_op (sexp ctx, sexp self, sexp_sint_t n, sexp flags,
                             sexp num_args, sexp bc, sexp vars) {
  sexp proc = sexp_alloc_type(ctx, procedure, SEXP_PROCEDURE);
  sexp_procedure_flags(proc) = (char) (sexp_uint_t) flags;
  sexp_procedure_num_args(proc) = sexp_unbox_fixnum(num_args);
  sexp_procedure_code(proc) = bc;
  sexp_procedure_vars(proc) = vars;
  return proc;
}

static sexp sexp_make_macro (sexp ctx, sexp p, sexp e) {
  sexp mac = sexp_alloc_type(ctx, macro, SEXP_MACRO);
  sexp_macro_env(mac) = e;
  sexp_macro_proc(mac) = p;
  sexp_macro_aux(mac) = SEXP_FALSE;
  return mac;
}

sexp sexp_make_synclo_op (sexp ctx, sexp self, sexp_sint_t n, sexp env, sexp fv, sexp expr) {
  sexp res;
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  if (! (sexp_symbolp(expr) || sexp_pairp(expr) || sexp_synclop(expr)))
    return expr;
  res = sexp_alloc_type(ctx, synclo, SEXP_SYNCLO);
  if (SEXP_USE_FLAT_SYNTACTIC_CLOSURES && sexp_synclop(expr)) {
    sexp_synclo_env(res) = sexp_synclo_env(expr);
    sexp_synclo_free_vars(res) = sexp_synclo_free_vars(expr);
    sexp_synclo_expr(res) = sexp_synclo_expr(expr);
    sexp_synclo_rename(res) = sexp_synclo_rename(expr);
  } else {
    sexp_synclo_env(res) = env;
    sexp_synclo_free_vars(res) = fv;
    sexp_synclo_expr(res) = expr;
    sexp_synclo_rename(res) = SEXP_FALSE;
  }
  return res;
}

/* internal AST */

sexp sexp_make_lambda (sexp ctx, sexp params) {
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

sexp sexp_make_ref (sexp ctx, sexp name, sexp cell) {
  sexp res = sexp_alloc_type(ctx, ref, SEXP_REF);
  sexp_ref_name(res) = name;
  sexp_ref_cell(res) = cell;
  return res;
}

static sexp sexp_make_set (sexp ctx, sexp var, sexp value) {
  sexp res = sexp_alloc_type(ctx, set, SEXP_SET);
  sexp_set_var(res) = var;
  sexp_set_value(res) = value;
  return res;
}

static sexp sexp_make_cnd (sexp ctx, sexp test, sexp pass, sexp fail) {
  sexp res = sexp_alloc_type(ctx, cnd, SEXP_CND);
  sexp_cnd_test(res) = test;
  sexp_cnd_pass(res) = pass;
  sexp_cnd_fail(res) = fail;
  return res;
}

sexp sexp_make_lit (sexp ctx, sexp value) {
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
    sexp_immutablep(sexp_global(ctx, SEXP_G_MODULE_PATH)) = 1;
  }
}

#if ! SEXP_USE_NATIVE_X86
static void sexp_init_eval_context_bytecodes (sexp ctx) {
  sexp_gc_var3(tmp, vec, ctx2);
  sexp_gc_preserve3(ctx, tmp, vec, ctx2);
  sexp_emit(ctx, SEXP_OP_RESUMECC);
  sexp_global(ctx, SEXP_G_RESUMECC_BYTECODE) = sexp_complete_bytecode(ctx);
  ctx2 = sexp_make_child_context(ctx, NULL);
  sexp_emit(ctx2, SEXP_OP_DONE);
  tmp = sexp_complete_bytecode(ctx2);
  vec = sexp_make_vector(ctx, 0, SEXP_VOID);
  sexp_global(ctx, SEXP_G_FINAL_RESUMER)
    = sexp_make_procedure(ctx, SEXP_ZERO, SEXP_ZERO, tmp, vec);
  sexp_bytecode_name(sexp_procedure_code(sexp_global(ctx, SEXP_G_FINAL_RESUMER)))
    = sexp_intern(ctx, "final-resumer", -1);
  sexp_gc_release3(ctx);
}
#endif

void sexp_init_eval_context_globals (sexp ctx) {
  const char* no_sys_path;
  const char* user_path;
  ctx = sexp_make_child_context(ctx, NULL);
#if ! SEXP_USE_NATIVE_X86
  sexp_init_eval_context_bytecodes(ctx);
#endif
  sexp_global(ctx, SEXP_G_MODULE_PATH) = SEXP_NULL;
  no_sys_path = getenv(SEXP_NO_SYSTEM_PATH_VAR);
  if (!no_sys_path || strcmp(no_sys_path, "0")==0)
    sexp_add_path(ctx, sexp_default_module_path);
  user_path = getenv(SEXP_MODULE_PATH_VAR);
  if (!user_path) user_path = sexp_default_user_module_path;
  sexp_add_path(ctx, user_path);
#if SEXP_USE_GREEN_THREADS
  sexp_global(ctx, SEXP_G_IO_BLOCK_ERROR)
    = sexp_user_exception(ctx, SEXP_FALSE, "I/O would block", SEXP_NULL);
  sexp_global(ctx, SEXP_G_IO_BLOCK_ONCE_ERROR)
    = sexp_user_exception(ctx, SEXP_FALSE, "I/O would block once", SEXP_NULL);
  sexp_global(ctx, SEXP_G_THREAD_TERMINATE_ERROR)
    = sexp_user_exception(ctx, SEXP_FALSE, "thread terminated", SEXP_NULL);
  sexp_global(ctx, SEXP_G_THREADS_FRONT) = SEXP_NULL;
  sexp_global(ctx, SEXP_G_THREADS_BACK) = SEXP_NULL;
  sexp_global(ctx, SEXP_G_THREADS_SIGNALS) = SEXP_ZERO;
  sexp_global(ctx, SEXP_G_THREADS_SIGNAL_RUNNER) = SEXP_FALSE;
  sexp_global(ctx, SEXP_G_ATOMIC_P) = SEXP_FALSE;
#endif
}

sexp sexp_make_eval_context (sexp ctx, sexp stack, sexp env, sexp_uint_t size, sexp_uint_t max_size) {
  sexp_gc_var1(res);
  res = sexp_make_context(ctx, size, max_size);
  if (!res || sexp_exceptionp(res))
    return res;
  if (ctx) sexp_gc_preserve1(ctx, res);
  sexp_context_env(res) = (env ? env : sexp_make_primitive_env(res, SEXP_SEVEN));
  sexp_context_specific(res) = sexp_make_vector(res, SEXP_SEVEN, SEXP_ZERO);
  sexp_context_lambda(res) = SEXP_FALSE;
  sexp_context_fv(res) = SEXP_NULL;
  sexp_context_bc(res) = sexp_alloc_bytecode(res, SEXP_INIT_BCODE_SIZE);
  if (sexp_exceptionp(sexp_context_env(res))) {
    res = sexp_context_env(res);
  } else if (sexp_exceptionp(sexp_context_specific(res))) {
    res = sexp_context_specific(res);
  } else if (sexp_exceptionp(sexp_context_bc(res))) {
    res = sexp_context_bc(res);
  } else {
    sexp_bytecode_name(sexp_context_bc(res)) = SEXP_FALSE;
    sexp_bytecode_length(sexp_context_bc(res)) = SEXP_INIT_BCODE_SIZE;
    sexp_bytecode_literals(sexp_context_bc(res)) = SEXP_NULL;
    sexp_bytecode_source(sexp_context_bc(res)) = SEXP_NULL;
    if ((! stack) || (stack == SEXP_FALSE)) {
      stack = sexp_alloc_tagged(res, SEXP_STACK_SIZE, SEXP_STACK);
      if (sexp_exceptionp(stack)) {
        if (ctx) sexp_gc_release1(ctx);
        return stack;
      } else {
        sexp_stack_length(stack) = SEXP_INIT_STACK_SIZE;
        sexp_stack_top(stack) = 0;
      }
    }
    sexp_context_stack(res) = stack;
    if (! ctx) sexp_init_eval_context_globals(res);
    if (ctx) {
      sexp_context_params(res) = sexp_context_params(ctx);
      sexp_context_tracep(res) = sexp_context_tracep(ctx);
      sexp_context_dk(res) = sexp_context_dk(ctx);
      sexp_gc_release1(ctx);
    } else {
      /* TODO: make the root a global (with friendly error in/out) */
      sexp_context_dk(res) = sexp_make_vector(res, SEXP_FOUR, SEXP_FALSE);
      sexp_vector_set(sexp_context_dk(res), SEXP_ZERO, SEXP_ZERO);
    }
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

sexp sexp_id_name (sexp x) {
  while (sexp_synclop(x)) x = sexp_synclo_expr(x);
  return x;
}

int sexp_idp (sexp x) {
  return sexp_symbolp(sexp_id_name(x));
}

sexp sexp_identifierp_op (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  return sexp_make_boolean(sexp_idp(x));
}

sexp sexp_syntactic_closure_expr_op (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  return (sexp_synclop(x) ? sexp_synclo_expr(x) : x);
}

static int sexp_contains_syntax_p_bound(sexp x, int depth) {
  int i;
  sexp ls1, ls2;
  if (sexp_synclop(x))
    return 1;
  if (depth <= 0)
    return 0;
  if (sexp_pairp(x)) {
    for (i=0, ls1=x, ls2=x; sexp_pairp(ls1); ls1=sexp_cdr(ls1), ls2=(i++ & 1 ? sexp_cdr(ls2) : ls2)) {
      if (sexp_contains_syntax_p_bound(sexp_car(ls1), depth-1))
        return 1;
      if (i > 0 && (ls1 == ls2 || ls1 == sexp_car(ls2)))
        return 0; /* cycle, no synclo found, assume none */
    }
    if (sexp_synclop(ls1))
      return sexp_contains_syntax_p_bound(sexp_id_name(ls1), depth-1);
  } else if (sexp_vectorp(x)) {
    for (i = 0; i < sexp_vector_length(x); ++i)
      if (sexp_contains_syntax_p_bound(sexp_vector_ref(x, sexp_make_fixnum(i)), depth-1))
        return 1;
  }
  return 0;
}

sexp sexp_strip_synclos_bound (sexp ctx, sexp x, int depth) {
  int i;
  sexp_gc_var3(res, kar, kdr);
  if (depth <= 0) return x;
  sexp_gc_preserve3(ctx, res, kar, kdr);
  x = sexp_id_name(x);
  if (sexp_pairp(x)) {
    kar = sexp_strip_synclos_bound(ctx, sexp_car(x), depth-1);
    kdr = sexp_strip_synclos_bound(ctx, sexp_cdr(x), depth-1);
    res = sexp_cons(ctx, kar, kdr);
    sexp_pair_source(res) = sexp_pair_source(x);
    sexp_immutablep(res) = 1;
  } else {
    if (sexp_vectorp(x))
      for (i = 0; i < sexp_vector_length(x); ++i)
        sexp_vector_set(x, sexp_make_fixnum(i), sexp_strip_synclos_bound(ctx, sexp_vector_ref(x, sexp_make_fixnum(i)), depth-1));
    res = x;
  }
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_strip_synclos (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (!sexp_contains_syntax_p_bound(x, SEXP_STRIP_SYNCLOS_BOUND))
    return x;
  return sexp_strip_synclos_bound(ctx, x, SEXP_STRIP_SYNCLOS_BOUND);
}

sexp sexp_identifier_eq_op (sexp ctx, sexp self, sexp_sint_t n, sexp e1, sexp id1, sexp e2, sexp id2) {
  sexp cell1, cell2;
  cell1 = sexp_env_cell(ctx, e1, id1, 0);
  cell2 = sexp_env_cell(ctx, e2, id2, 0);
  if (cell1 && (cell1 == cell2))
    return SEXP_TRUE;
  else if (!cell1 && !cell2 && (id1 == id2))
    return SEXP_TRUE;
  /* If the symbols are the same and the cells are either unbound or
   * (optionally) bound to top-level variables, consider them the
   * same.  Local (non-toplevel) bindings must still match exactly.
   */
  while (sexp_synclop(id1))
    id1 = sexp_synclo_expr(id1);
  while (sexp_synclop(id2))
    id2 = sexp_synclo_expr(id2);
  if ((id1 == id2)
      && ((!cell1 && !cell2)
#if !SEXP_USE_STRICT_TOPLEVEL_BINDINGS
          || ((!cell1 || (!sexp_lambdap(sexp_cdr(cell1)) &&
                         !sexp_env_cell_syntactic_p(cell1))) &&
              (!cell2 || (!sexp_lambdap(sexp_cdr(cell2)) &&
                          !sexp_env_cell_syntactic_p(cell2))))
#endif
              ))
    return SEXP_TRUE;
  return SEXP_FALSE;
}

/************************* the compiler ***************************/

static int lambda_envp(sexp ctx) {
  sexp env;
  for (env=sexp_context_env(ctx); env && sexp_envp(env); env=sexp_env_parent(env))
    if (sexp_env_lambda(env))
      return 1;
  return 0;
}

static int nondefp(sexp x) {
  sexp ls;
  if (sexp_pairp(x) || sexp_cndp(x))
    return 1;
  if (sexp_seqp(x))
    for (ls=sexp_seq_ls(x); sexp_pairp(ls); ls=sexp_cdr(ls))
      if (nondefp(sexp_car(ls)))
        return 1;
  return 0;
}

static sexp analyze_list (sexp ctx, sexp x, int depth, int defok) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  for (res=SEXP_NULL; sexp_pairp(x); x=sexp_cdr(x)) {
    sexp_push(ctx, res, SEXP_FALSE);
    tmp = analyze(ctx, sexp_car(x), depth, defok);
    if (sexp_exceptionp(tmp)) {
      res = tmp;
      break;
    } else {
      if (lambda_envp(ctx) && nondefp(tmp)) defok = -1;  /* -1 to warn */
      sexp_pair_source(res) = sexp_pair_source(x);
      sexp_car(res) = tmp;
    }
  }
  if (sexp_pairp(res)) res = sexp_nreverse(ctx, res);
  sexp_gc_release2(ctx);
  return res;
}

static sexp analyze_app (sexp ctx, sexp x, int depth) {
  sexp p, res, tmp;
  res = analyze_list(ctx, x, depth, 0);
  if (sexp_lambdap(sexp_car(res))) {       /* fill in lambda names */
    p=sexp_lambda_params(sexp_car(res));
    for (tmp=sexp_cdr(res); sexp_pairp(tmp)&&sexp_pairp(p); tmp=sexp_cdr(tmp), p=sexp_cdr(p))
      if (sexp_lambdap(sexp_car(tmp)))
        sexp_lambda_name(sexp_car(tmp)) = sexp_car(p);
  }
  return res;
}

static sexp analyze_seq (sexp ctx, sexp ls, int depth, int defok) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  if (sexp_nullp(ls))
    res = SEXP_VOID;
  else if (sexp_nullp(sexp_cdr(ls)))
    res = analyze(ctx, sexp_car(ls), depth, defok);
  else {
    res = sexp_alloc_type(ctx, seq, SEXP_SEQ);
    sexp_seq_source(res) = sexp_pair_source(ls);
    tmp = analyze_list(ctx, ls, depth, defok);
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
  cell = sexp_env_cell_loc(ctx, env, x, 0, varenv);
  if (! cell) {
    cell = sexp_env_cell_create(ctx, env, x, SEXP_UNDEF, varenv);
  }
  if (sexp_macrop(sexp_cdr(cell)) || sexp_corep(sexp_cdr(cell))) {
    res = sexp_compile_error(ctx, "invalid use of syntax as value", x);
  } else {
    res = sexp_make_ref(ctx, sexp_car(cell), cell);
  }
  sexp_gc_release1(ctx);
  return res;
}

static sexp analyze_set (sexp ctx, sexp x, int depth) {
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
    value = analyze(ctx, sexp_caddr(x), depth, 0);
    if (sexp_exceptionp(ref)) {
      res = ref;
    } else if (sexp_exceptionp(value)) {
      res = value;
    } else if (sexp_immutablep(sexp_ref_cell(ref))
               || (varenv && sexp_immutablep(varenv))) {
      res = sexp_compile_error(ctx, "immutable binding", sexp_cadr(x));
    } else {
      res = sexp_make_set(ctx, ref, value);
      sexp_set_source(res) = sexp_pair_source(x);
    }
  }
  sexp_gc_release2(ctx);
  return res;
}

#define sexp_return(res, val) do {res=val; goto cleanup;} while (0)

static sexp analyze_lambda (sexp ctx, sexp x, int depth) {
  int trailing_non_procs;
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
  if (! sexp_nullp(ls)) {
    if (! sexp_idp(ls))
      sexp_return(res, sexp_compile_error(ctx, "non-symbol parameter", x));
    else if (sexp_truep(sexp_memq(ctx, ls, sexp_cadr(x))))
      sexp_return(res, sexp_compile_error(ctx, "duplicate parameter", x));
  }
  /* build lambda and analyze body */
  res = sexp_make_lambda(ctx, tmp=sexp_copy_list(ctx, sexp_cadr(x)));
  if (sexp_exceptionp(res)) sexp_return(res, res);
  sexp_lambda_source(res) = sexp_pair_source(x);
  if (! (sexp_lambda_source(res) && sexp_pairp(sexp_lambda_source(res))))
    sexp_lambda_source(res) = sexp_pair_source(sexp_cdr(x));
  if (! (sexp_lambda_source(res) && sexp_pairp(sexp_lambda_source(res))))
    sexp_lambda_source(res) = sexp_pair_source(sexp_cddr(x));
  ctx2 = sexp_make_child_context(ctx, res);
  if (sexp_exceptionp(ctx2)) sexp_return(res, ctx2);
  tmp = sexp_flatten_dot(ctx2, sexp_lambda_params(res));
  sexp_context_env(ctx2) = sexp_extend_env(ctx2, sexp_context_env(ctx2), tmp, res);
  if (sexp_exceptionp(sexp_context_env(ctx2))) sexp_return(res, sexp_context_env(ctx2));
  sexp_env_lambda(sexp_context_env(ctx2)) = res;
  body = analyze_seq(ctx2, sexp_cddr(x), depth, 1);
  if (sexp_exceptionp(body)) sexp_return(res, body);
  /* delayed analyze internal defines */
  trailing_non_procs = 0;
  defs = SEXP_NULL;
  for (ls=sexp_lambda_defs(res); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    tmp = sexp_car(ls);
    ctx3 = sexp_cdr(tmp);
    if (sexp_pairp(sexp_caar(tmp))) {
      name = sexp_caaar(tmp);
      tmp = sexp_cons(ctx3, sexp_cdaar(tmp), sexp_cdar(tmp));
      tmp = sexp_cons(ctx3, SEXP_VOID, tmp);
      sexp_pair_source(tmp) = sexp_pair_source(sexp_caar(ls));
      value = analyze_lambda(ctx3, tmp, depth);
    } else {
      name = sexp_caar(tmp);
      value = analyze(ctx3, sexp_cadar(tmp), depth, 0);
    }
    if (sexp_exceptionp(value)) sexp_return(res, value);
    if (sexp_lambdap(value)) sexp_lambda_name(value) = name;
    tmp = analyze_var_ref(ctx3, name, NULL);
    if (sexp_exceptionp(tmp)) sexp_return(res, tmp);
    tmp = sexp_make_set(ctx3, tmp, value);
    if (sexp_exceptionp(tmp)) sexp_return(res, tmp);
    sexp_push(ctx3, defs, tmp);
    if (!sexp_lambdap(value)) trailing_non_procs = 1;
    if (trailing_non_procs || !SEXP_USE_UNBOXED_LOCALS)
      sexp_insert(ctx3, sexp_lambda_sv(res), name);
  }
  if (sexp_pairp(defs)) {
    if (! sexp_seqp(body)) {
      tmp = sexp_alloc_type(ctx2, seq, SEXP_SEQ);
      sexp_seq_ls(tmp) = sexp_list1(ctx2, body);
      body = tmp;
    }
    sexp_seq_ls(body) = sexp_append2(ctx2, defs, sexp_seq_ls(body));
    if (sexp_exceptionp(sexp_seq_ls(body))) sexp_return(res, sexp_seq_ls(body));
  }
  if (sexp_exceptionp(body)) res = body;
  else sexp_lambda_body(res) = body;
 cleanup:
  sexp_gc_release6(ctx);
  return res;
}

static sexp analyze_if (sexp ctx, sexp x, int depth) {
  sexp res, fail_expr;
  sexp_gc_var3(test, pass, fail);
  sexp_gc_preserve3(ctx, test, pass, fail);
  if (! (sexp_pairp(sexp_cdr(x)) && sexp_pairp(sexp_cddr(x)))) {
    res = sexp_compile_error(ctx, "not enough args to if", x);
  } else if (sexp_pairp(sexp_cdddr(x)) && sexp_cdr(sexp_cdddr(x)) != SEXP_NULL) {
    res = sexp_compile_error(ctx, "too many args to if", x);
  } else {
    test = analyze(ctx, sexp_cadr(x), depth, 0);
    pass = analyze(ctx, sexp_caddr(x), depth, 0);
    fail_expr = sexp_pairp(sexp_cdddr(x)) ? sexp_cadddr(x) : SEXP_VOID;
    fail = analyze(ctx, fail_expr, depth, 0);
    res = (sexp_exceptionp(test) ? test : sexp_exceptionp(pass) ? pass :
           sexp_exceptionp(fail) ? fail : sexp_make_cnd(ctx, test, pass, fail));
    if (sexp_cndp(res)) sexp_cnd_source(res) = sexp_pair_source(x);
  }
  sexp_gc_release3(ctx);
  return res;
}

static sexp analyze_define (sexp ctx, sexp x, int depth) {
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
      sexp_push(ctx, sexp_lambda_locals(sexp_env_lambda(env)), name);
      tmp = sexp_cons(ctx, sexp_cdr(x), ctx);
      sexp_pair_source(sexp_cdr(x)) = sexp_pair_source(x);
      sexp_push(ctx, sexp_lambda_defs(sexp_env_lambda(env)), tmp);
      res = SEXP_VOID;
    } else {
#if SEXP_USE_UNWRAPPED_TOPLEVEL_BINDINGS
      if (sexp_synclop(name)) name = sexp_synclo_expr(name);
#endif
      sexp_env_cell_define(ctx, env, name, SEXP_VOID, &varenv);
      if (sexp_pairp(sexp_cadr(x))) {
        tmp = sexp_cons(ctx, sexp_cdadr(x), sexp_cddr(x));
        tmp = sexp_cons(ctx, SEXP_VOID, tmp);
        sexp_pair_source(tmp) = sexp_pair_source(x);
        value = analyze_lambda(ctx, tmp, depth);
      } else
        value = analyze(ctx, sexp_caddr(x), depth, 0);
      tmp = sexp_env_cell_loc(ctx, env, name, 0, &varenv);
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
        if (sexp_setp(res)) sexp_set_source(res) = sexp_pair_source(x);
      }
    }
  }
  sexp_gc_release4(ctx);
  return res;
}

static sexp analyze_bind_syntax (sexp ls, sexp eval_ctx, sexp bind_ctx, int localp) {
  sexp res = SEXP_VOID, name;
  sexp_gc_var2(mac, tmp);
  sexp_gc_preserve2(eval_ctx, mac, tmp);
  for ( ; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    if (! (sexp_pairp(sexp_car(ls)) && sexp_pairp(sexp_cdar(ls))
           && sexp_idp(sexp_caar(ls)) && sexp_nullp(sexp_cddar(ls)))) {
      res = sexp_compile_error(eval_ctx, "bad syntax binding", sexp_pairp(ls) ? sexp_car(ls) : ls);
      break;
    }
    if (sexp_idp(sexp_cadar(ls)))
      mac = sexp_env_ref(eval_ctx, sexp_context_env(eval_ctx), sexp_cadar(ls), SEXP_FALSE);
    else
      mac = sexp_eval(eval_ctx, sexp_cadar(ls), NULL);
    if (sexp_procedurep(mac))
      mac = sexp_make_macro(eval_ctx, mac, sexp_context_env(eval_ctx));
    if (!(sexp_macrop(mac)||sexp_corep(mac))) {
      res = (sexp_exceptionp(mac) ? mac
             : sexp_compile_error(eval_ctx, "non-procedure macro", mac));
      break;
    }
    name = sexp_caar(ls);
    if (sexp_synclop(name) && sexp_env_global_p(sexp_context_env(bind_ctx)))
      name = sexp_synclo_expr(name);
    if (sexp_macrop(mac) && sexp_pairp(sexp_cadar(ls)))
      sexp_macro_source(mac) = sexp_pair_source(sexp_cadar(ls));
    if (localp)
      sexp_env_push(eval_ctx, sexp_context_env(bind_ctx), tmp, name, mac);
    else
      sexp_env_define(eval_ctx, sexp_context_env(bind_ctx), name, mac);
#if !SEXP_USE_STRICT_TOPLEVEL_BINDINGS
    if (localp)
      sexp_env_cell_syntactic_p(sexp_env_cell(eval_ctx, sexp_context_env(bind_ctx), name, 0)) = 1;
#endif
  }
  sexp_gc_release2(eval_ctx);
  return res;
}

static sexp analyze_define_syntax (sexp ctx, sexp x) {
  sexp res;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  tmp = sexp_list1(ctx, sexp_cdr(x));
  res = sexp_exceptionp(tmp) ? tmp : analyze_bind_syntax(tmp, ctx, ctx, 0);
  sexp_gc_release1(ctx);
  return res;
}

static sexp analyze_let_syntax_aux (sexp ctx, sexp x, int recp, int depth) {
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
    tmp = analyze_bind_syntax(sexp_cadr(x), (recp ? ctx2 : ctx), ctx2, 1);
    res = (sexp_exceptionp(tmp) ? tmp : analyze_seq(ctx2, sexp_cddr(x), depth, 1));
  }
  sexp_gc_release3(ctx);
  return res;
}

static sexp analyze_let_syntax (sexp ctx, sexp x, int depth) {
  return analyze_let_syntax_aux(ctx, x, 0, depth);
}

static sexp analyze_letrec_syntax (sexp ctx, sexp x, int depth) {
  return analyze_let_syntax_aux(ctx, x, 1, depth);
}

static sexp analyze (sexp ctx, sexp object, int depth, int defok) {
  sexp op;
  sexp_gc_var4(res, tmp, x, cell);
  sexp_gc_preserve4(ctx, res, tmp, x, cell);
  x = object;

  if (++depth > SEXP_MAX_ANALYZE_DEPTH) {
    res = sexp_compile_error(ctx, "SEXP_MAX_ANALYZE_DEPTH exceeded", x);
    goto error;
  }

 loop:
  if (sexp_pairp(x)) {
    cell = sexp_idp(sexp_car(x)) ? sexp_env_cell(ctx, sexp_context_env(ctx), sexp_car(x), 0) : NULL;
    if (sexp_not(sexp_listp(ctx, x))
        && !(cell && sexp_macrop(sexp_cdr(cell)))) {
      res = sexp_compile_error(ctx, "dotted list in source", x);
    } else if (sexp_idp(sexp_car(x))) {
      if (! cell) {
        res = analyze_app(ctx, x, depth);
        if (sexp_exceptionp(res))
          sexp_warn(ctx, "exception inside undefined operator: ", sexp_car(x));
      } else {
        op = sexp_cdr(cell);
        if (sexp_corep(op)) {
          switch (sexp_core_code(op)) {
          case SEXP_CORE_DEFINE:
            if (defok < 0)
              sexp_warn(ctx, "out of order define: ", x);
            res = defok ? analyze_define(ctx, x, depth)
              : sexp_compile_error(ctx, "unexpected define", x);
            break;
          case SEXP_CORE_SET:
            res = analyze_set(ctx, x, depth); break;
          case SEXP_CORE_LAMBDA:
            res = analyze_lambda(ctx, x, depth); break;
          case SEXP_CORE_IF:
            res = analyze_if(ctx, x, depth); break;
          case SEXP_CORE_BEGIN:
            res = analyze_seq(ctx, sexp_cdr(x), depth, defok); break;
          case SEXP_CORE_QUOTE:
          case SEXP_CORE_SYNTAX_QUOTE:
            if (! (sexp_pairp(sexp_cdr(x)) && sexp_nullp(sexp_cddr(x))))
              res = sexp_compile_error(ctx, "bad quote form", x);
            else
              res = sexp_make_lit(ctx,
                                  (sexp_core_code(op) == SEXP_CORE_QUOTE) ?
                                  tmp=sexp_strip_synclos(ctx , NULL, 1, sexp_cadr(x)) :
                                  sexp_cadr(x));
            break;
          case SEXP_CORE_DEFINE_SYNTAX:
            if (defok < 0)
              sexp_warn(ctx, "out of order define-syntax: ", x);
            res = defok ? analyze_define_syntax(ctx, x)
              : sexp_compile_error(ctx, "unexpected define-syntax", x);
            break;
          case SEXP_CORE_LET_SYNTAX:
            res = analyze_let_syntax(ctx, x, depth); break;
          case SEXP_CORE_LETREC_SYNTAX:
            res = analyze_letrec_syntax(ctx, x, depth); break;
          default:
            res = sexp_compile_error(ctx, "unknown core form", op); break;
          }
        } else if (sexp_macrop(op)) {
          tmp = sexp_cons(ctx, sexp_macro_env(op), SEXP_NULL);
          tmp = sexp_cons(ctx, sexp_context_env(ctx), tmp);
          tmp = sexp_cons(ctx, x, tmp);
          x = sexp_exceptionp(tmp) ? tmp : sexp_make_child_context(ctx, sexp_context_lambda(ctx));
          if (!sexp_exceptionp(x) && !sexp_exceptionp(sexp_context_exception(ctx)))
            x = sexp_apply(x, sexp_macro_proc(op), tmp);
          if (sexp_exceptionp(x) && sexp_not(sexp_exception_source(x)))
            sexp_exception_source(x) = sexp_pair_source(sexp_car(tmp));
          goto loop;
        } else if (sexp_opcodep(op)) {
          res = sexp_length(ctx, sexp_cdr(x));
          if (sexp_unbox_fixnum(res) < sexp_opcode_num_args(op)) {
            sexp_warn(ctx, "not enough args for opcode: ", x);
            op = analyze_var_ref(ctx, sexp_car(x), NULL);
          } else if ((sexp_unbox_fixnum(res) > sexp_opcode_num_args(op))
                     && (! sexp_opcode_variadic_p(op))) {
            sexp_warn(ctx, "too many args for opcode: ", x);
            op = analyze_var_ref(ctx, sexp_car(x), NULL);
          }
          res = analyze_list(ctx, sexp_cdr(x), 0, 0);
          if (! sexp_exceptionp(res)) {
            /* push op, which will be a direct opcode if the call is valid */
            sexp_push(ctx, res, op);
            if (sexp_pairp(res))
              sexp_pair_source(res) = sexp_pair_source(x);
          }
        } else {
          res = analyze_app(ctx, x, depth);
        }
      }
    } else {
      res = analyze_app(ctx, x, depth);
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
    if (sexp_pairp(sexp_synclo_free_vars(x))) {
      sexp_push(ctx, sexp_context_fv(tmp), sexp_context_env(ctx));
      sexp_context_fv(tmp) = sexp_append2(tmp,
                                          sexp_synclo_free_vars(x),
                                          sexp_context_fv(tmp));
    }
    sexp_context_env(tmp) = sexp_extend_synclo_env(tmp, sexp_synclo_env(x));
    x = sexp_synclo_expr(x);
    res = analyze(tmp, x, depth, defok);
  } else if (sexp_nullp(x)) {
    res = sexp_compile_error(ctx, "empty application in source", x);
  } else {
    if (sexp_pointerp(x)) {    /* accept vectors and other literals directly, */
      sexp_immutablep(x) = 1;  /* but they must be immutable */
      x = sexp_strip_synclos(ctx , NULL, 1, x);
    }
    res = x;
  }

error:
  if (sexp_exceptionp(res) && sexp_not(sexp_exception_source(res))
      && sexp_pairp(x))
    sexp_exception_source(res) = sexp_pair_source(x);
  sexp_gc_release4(ctx);
  return res;
}

sexp sexp_analyze (sexp ctx, sexp x) {return analyze(ctx, x, 0, 1);}

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

sexp sexp_open_input_file_op (sexp ctx, sexp self, sexp_sint_t n, sexp path) {
  FILE *in;
  int count = 0;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, path);
  do {
    if (count != 0) sexp_gc(ctx, NULL);
    in = fopen(sexp_string_data(path), "r");
  } while (!in && sexp_out_of_file_descriptors() && !count++);
  if (!in)
    return sexp_file_exception(ctx, self, "couldn't open input file", path);
#if SEXP_USE_GREEN_THREADS
  fcntl(fileno(in), F_SETFL, O_NONBLOCK);
#endif
  return sexp_make_input_port(ctx, in, path);
}

sexp sexp_open_output_file_op (sexp ctx, sexp self, sexp_sint_t n, sexp path) {
  FILE *out;
  int count = 0;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, path);
  do {
    if (count != 0) sexp_gc(ctx, NULL);
    out = fopen(sexp_string_data(path), "w");
  } while (!out && sexp_out_of_file_descriptors() && !count++);
  if (!out)
    return sexp_file_exception(ctx, self, "couldn't open output file", path);
#if SEXP_USE_GREEN_THREADS
  fcntl(fileno(out), F_SETFL, O_NONBLOCK);
#endif
  return sexp_make_output_port(ctx, out, path);
}

sexp sexp_open_binary_input_file (sexp ctx, sexp self, sexp_sint_t n, sexp path) {
  sexp res = sexp_open_input_file_op(ctx, self, n, path);
  if (sexp_portp(res)) sexp_port_binaryp(res) = 1;
  return res;
}

sexp sexp_open_binary_output_file (sexp ctx, sexp self, sexp_sint_t n, sexp path) {
  sexp res = sexp_open_output_file_op(ctx, self, n, path);
  if (sexp_portp(res)) sexp_port_binaryp(res) = 1;
  return res;
}

sexp sexp_close_port_op (sexp ctx, sexp self, sexp_sint_t n, sexp port) {
  sexp res = SEXP_VOID;
  sexp_assert_type(ctx, sexp_portp, SEXP_OPORT, port);
  /* we can't run arbitrary scheme code in the finalizer, so we need */
  /* to flush and run the closer here */
  if (sexp_port_customp(port)) {
    if (sexp_oportp(port)) res = sexp_flush_output(ctx, port);
    if (sexp_exceptionp(res)) return res;
    if (sexp_applicablep(sexp_port_closer(port)))
      res = sexp_apply1(ctx, sexp_port_closer(port), port);
    if (sexp_exceptionp(res)) return res;
  }
  return sexp_finalize_port(ctx, self, n, port);
}

sexp sexp_set_port_line_op (sexp ctx, sexp self, sexp_sint_t n, sexp port, sexp line) {
  sexp_assert_type(ctx, sexp_iportp, SEXP_IPORT, port);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, line);
  sexp_port_sourcep(port) = 1;
  sexp_port_line(port) = sexp_unbox_fixnum(line);
  return SEXP_VOID;
}

#ifndef PLAN9
sexp sexp_get_port_fileno (sexp ctx, sexp self, sexp_sint_t n, sexp port) {
  int fd;
  sexp_assert_type(ctx, sexp_portp, SEXP_IPORT, port);
  fd = sexp_port_fileno(port);
  if (fd >= 0)
    return sexp_make_fixnum(fd);
  return SEXP_FALSE;
}
sexp sexp_stream_portp_op (sexp ctx, sexp self, sexp_sint_t n, sexp port) {
  sexp_assert_type(ctx, sexp_portp, SEXP_IPORT, port);
  return sexp_make_boolean(sexp_stream_portp(port));
}
#endif

#if SEXP_USE_STATIC_LIBS
#if SEXP_USE_STATIC_LIBS_NO_INCLUDE
extern struct sexp_library_entry_t* sexp_static_libraries;
#else
#include "clibs.c"
#endif
static struct sexp_library_entry_t *sexp_find_static_library(const char *file)
{
  size_t base_len;
  struct sexp_library_entry_t *entry;

  if (file[0] == '.' && file[1] == '/')
    file += 2;
  base_len = strlen(file) - strlen(sexp_so_extension);
  if (strcmp(file + base_len, sexp_so_extension))
    return NULL;
  for (entry = &sexp_static_libraries[0]; entry->name; entry++)
    if (! strncmp(file, entry->name, base_len))
      return entry;
  return NULL;
}
#else
#define sexp_find_static_library(path) NULL
#endif

#if SEXP_USE_DL
#ifdef _WIN32
#include <windows.h>
static sexp sexp_load_dl (sexp ctx, sexp file, sexp env) {
  sexp res;
  sexp_init_proc init;
  HINSTANCE handle = LoadLibraryA(sexp_string_data(file));
  if (!handle)
    return sexp_compile_error(ctx, "couldn't load dynamic library", file);
  init = (sexp_init_proc) GetProcAddress(handle, "sexp_init_library");
  if (!init) {
    FreeLibrary(handle);
    return sexp_compile_error(ctx, "dynamic library has no sexp_init_library", file);
  }
  res = init(ctx, NULL, 3, env, sexp_version, SEXP_ABI_IDENTIFIER);
  if (res == SEXP_ABI_ERROR) res = sexp_global(ctx, SEXP_G_ABI_ERROR);
  return res;
}
#else
static sexp sexp_make_dl (sexp ctx, sexp file, void* handle) {
  sexp res = sexp_alloc_type(ctx, dl, SEXP_DL);
  sexp_dl_file(res) = file;
  sexp_dl_handle(res) = handle;
  return res;
}
static sexp sexp_load_dl (sexp ctx, sexp file, sexp env) {
  sexp_init_proc init;
  sexp_gc_var2(res, old_dl);
  void *handle = dlopen(sexp_string_data(file), RTLD_LAZY);
  if (! handle) {
    return sexp_compile_error(ctx, "couldn't load dynamic library", file);
  }
  init = dlsym(handle, "sexp_init_library");
  if (! init) {
    res = sexp_c_string(ctx, dlerror(), -1);
    res = sexp_list2(ctx, file, res);
    dlclose(handle);
    return sexp_compile_error(ctx, "dynamic library has no sexp_init_library", res);
  }
  sexp_gc_preserve2(ctx, res, old_dl);
  old_dl = sexp_context_dl(ctx);
  sexp_context_dl(ctx) = sexp_make_dl(ctx, file, handle);
  res = init(ctx, NULL, 3, env, sexp_version, SEXP_ABI_IDENTIFIER);
  /* If the ABI is incompatible the library may not even be able to
     properly reference a global, so it returns a special immediate
     which we need to translate. */
  if (res == SEXP_ABI_ERROR) res = sexp_global(ctx, SEXP_G_ABI_ERROR);
  sexp_context_dl(ctx) = old_dl;
  sexp_gc_release2(ctx);
  return res;
}
#endif
#else
#define sexp_load_dl(ctx, file, env) SEXP_UNDEF
#endif

#if SEXP_USE_DL || SEXP_USE_STATIC_LIBS
static sexp sexp_load_binary(sexp ctx, sexp file, sexp env) {
#if SEXP_USE_STATIC_LIBS
  struct sexp_library_entry_t *entry;
#endif
  sexp res = sexp_load_dl(ctx, file, env);
#if SEXP_USE_STATIC_LIBS
  if (res == SEXP_UNDEF || sexp_exceptionp(res)) {
    entry = sexp_find_static_library(sexp_string_data(file));
    if (entry == NULL)
      res = (res == SEXP_UNDEF ? sexp_compile_error(ctx, "couldn't find builtin library", file) : res);
    else
      res = entry->init(ctx, NULL, 3, env, sexp_version, SEXP_ABI_IDENTIFIER);
  }
#endif
  return res;
}
#endif

sexp sexp_load_op (sexp ctx, sexp self, sexp_sint_t n, sexp source, sexp env) {
#if SEXP_USE_DL || SEXP_USE_STATIC_LIBS
  const char *suffix;
#endif
  sexp_gc_var5(ctx2, x, in, res, out);
  if (!env) env = sexp_context_env(ctx);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
#if SEXP_USE_DL || SEXP_USE_STATIC_LIBS
  suffix = sexp_stringp(source) ? sexp_string_data(source)
    + sexp_string_size(source) - strlen(sexp_so_extension) : "...";
  if (strcmp(suffix, sexp_so_extension) == 0) {
    res = sexp_load_binary(ctx, source, env);
  } else {
#endif
  res = SEXP_VOID;
  if (sexp_iportp(source)) {
    in = source;
  } else {
    sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, source);
    in = sexp_open_input_file(ctx, source);
  }
  sexp_gc_preserve5(ctx, ctx2, x, in, res, out);
  if (sexp_exceptionp(in)) {
    out = sexp_current_error_port(ctx);
    if (sexp_not(out)) out = sexp_current_error_port(ctx);
    if (sexp_oportp(out))
      sexp_print_exception(ctx, in, out);
    res = in;
  } else {
    sexp_port_sourcep(in) = 1;
    ctx2 = sexp_make_eval_context(ctx, NULL, env, 0, 0);
    sexp_context_parent(ctx2) = ctx;
    sexp_context_tailp(ctx2) = 0;
    while ((x=sexp_read(ctx2, in)) != (sexp) SEXP_EOF) {
      res = sexp_exceptionp(x) ? x : sexp_eval(ctx2, x, env);
      if (sexp_exceptionp(res))
        break;
    }
    sexp_context_last_fp(ctx) = sexp_context_last_fp(ctx2);
    if (x == SEXP_EOF)
      res = SEXP_VOID;
    sexp_close_port(ctx, in);
  }
  sexp_gc_release5(ctx);
#if SEXP_USE_DL || SEXP_USE_STATIC_LIBS
  }
#endif
  return res;
}

sexp sexp_register_optimization (sexp ctx, sexp self, sexp_sint_t n, sexp f, sexp priority) {
  sexp_assert_type(ctx, sexp_applicablep, SEXP_PROCEDURE, f);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, priority);
  sexp_push(ctx, sexp_global(ctx, SEXP_G_OPTIMIZATIONS), SEXP_VOID);
  sexp_car(sexp_global(ctx, SEXP_G_OPTIMIZATIONS)) = sexp_cons(ctx, priority, f);
  return SEXP_VOID;
}

#if SEXP_USE_MATH

#if SEXP_USE_BIGNUMS
#define maybe_convert_bignum(z)                                 \
  else if (sexp_bignump(z)) d = sexp_bignum_to_double(z);
#else
#define maybe_convert_bignum(z)
#endif

#if SEXP_USE_RATIOS
#define maybe_convert_ratio(z)                          \
  else if (sexp_ratiop(z)) d = sexp_ratio_to_double(z);
#else
#define maybe_convert_ratio(z)
#endif

#if SEXP_USE_COMPLEX
#define maybe_convert_complex(z, f)                                     \
  else if (sexp_complexp(z)) return sexp_complex_normalize(f(ctx, z));
#define sexp_complex_dummy(ctx, z) 0
#else
#define maybe_convert_complex(z, f)
#endif

#define define_math_op(name, cname, f)                                  \
  sexp name (sexp ctx, sexp self, sexp_sint_t n, sexp z) {              \
    double d;                                                           \
    if (sexp_flonump(z))                                                \
      d = sexp_flonum_value(z);                                         \
    else if (sexp_fixnump(z))                                           \
      d = (double)sexp_unbox_fixnum(z);                                 \
    maybe_convert_ratio(z)                                              \
    maybe_convert_bignum(z)                                             \
    maybe_convert_complex(z, f)                                         \
    else                                                                \
      return sexp_type_exception(ctx, self, SEXP_NUMBER, z);            \
    return sexp_make_flonum(ctx, cname(d));                             \
  }

#if SEXP_USE_COMPLEX
#define define_complex_math_op(name, cname, f, a, b)		\
  sexp name (sexp ctx, sexp self, sexp_sint_t n, sexp z) {	\
    double d;                                                           \
    if (sexp_flonump(z))                                                \
      d = sexp_flonum_value(z);                                         \
    else if (sexp_fixnump(z))                                           \
      d = (double)sexp_unbox_fixnum(z);                                 \
    maybe_convert_ratio(z)                                              \
    maybe_convert_bignum(z)                                             \
    maybe_convert_complex(z, f)                                         \
    else                                                                \
      return sexp_type_exception(ctx, self, SEXP_NUMBER, z);            \
    if (d < a || d > b)							\
      return sexp_complex_normalize					\
	(f(ctx, sexp_make_complex(ctx, z, SEXP_ZERO)));			\
    return sexp_make_flonum(ctx, cname(d));                             \
  }
#else
#define define_complex_math_op(name, cname, f, a, b)	\
  define_math_op(name, cname, f)
#endif

define_math_op(sexp_exp, exp, sexp_complex_exp)
define_math_op(sexp_sin, sin, sexp_complex_sin)
define_math_op(sexp_cos, cos, sexp_complex_cos)
define_math_op(sexp_tan, tan, sexp_complex_tan)
define_complex_math_op(sexp_asin, asin, sexp_complex_asin, -1, 1)
define_complex_math_op(sexp_acos, acos, sexp_complex_acos, -1, 1)
define_math_op(sexp_atan, atan, sexp_complex_atan)

#if SEXP_USE_RATIOS
#define maybe_round_ratio(ctx, q, f)            \
  if (sexp_ratiop(q)) return f(ctx, q);
#else
#define maybe_round_ratio(ctx, q, f)
#endif

#define define_math_rounder(name, cname, f)                             \
  sexp name (sexp ctx, sexp self, sexp_sint_t n, sexp z) {              \
    maybe_round_ratio(ctx, z, f)                                        \
    if (sexp_flonump(z))                                                \
      return sexp_make_flonum(ctx, cname(sexp_flonum_value(z)));        \
    else if (sexp_fixnump(z) || sexp_bignump(z))                        \
      return z;                                                         \
    return sexp_type_exception(ctx, self, SEXP_NUMBER, z);              \
  }

static double even_round (double d) {
  double res = round(d);
  if (fabs(d - res) == 0.5 && ((long)res & 1))
    res += (res < 0) ? 1 : -1;
  return res;
}

define_math_rounder(sexp_round, even_round, sexp_ratio_round)
define_math_rounder(sexp_trunc, trunc, sexp_ratio_trunc)
define_math_rounder(sexp_floor, floor, sexp_ratio_floor)
define_math_rounder(sexp_ceiling, ceil, sexp_ratio_ceiling)

sexp sexp_log (sexp ctx, sexp self, sexp_sint_t n, sexp z) {
  double d;
#if SEXP_USE_COMPLEX
  sexp_gc_var1(tmp);
  if (sexp_complexp(z))
    return sexp_complex_log(ctx, z);
#endif
  if (sexp_flonump(z))
    d = sexp_flonum_value(z);
  else if (sexp_fixnump(z))
    d = (double)sexp_unbox_fixnum(z);
  maybe_convert_ratio(z)
  maybe_convert_bignum(z)
  else
    return sexp_type_exception(ctx, self, SEXP_NUMBER, z);
#if SEXP_USE_COMPLEX
  if (d < 0) {
    sexp_gc_preserve1(ctx, tmp);
    tmp = sexp_make_flonum(ctx, d);
    tmp = sexp_make_complex(ctx, tmp, SEXP_ZERO);
    tmp = sexp_complex_log(ctx, tmp);
    sexp_gc_release1(ctx);
    return tmp;
  }
#endif
  return sexp_make_flonum(ctx, log(d));
}

sexp sexp_inexact_sqrt (sexp ctx, sexp self, sexp_sint_t n, sexp z) {
#if SEXP_USE_COMPLEX
  int negativep = 0;
#endif
  double d, r;
  sexp_gc_var1(res);
  if (sexp_flonump(z))
    d = sexp_flonum_value(z);
  else if (sexp_fixnump(z))
    d = (double)sexp_unbox_fixnum(z);
  maybe_convert_ratio(z)        /* XXXX add ratio sqrt */
  maybe_convert_complex(z, sexp_complex_sqrt)
  else
    return sexp_type_exception(ctx, self, SEXP_NUMBER, z);
#if SEXP_USE_COMPLEX
  if (d < 0) {
    negativep = 1;
    d = -d;
  }
#endif
  sexp_gc_preserve1(ctx, res);
  r = sqrt(d);
  if (sexp_fixnump(z)
      && (((sexp_uint_t)r*(sexp_uint_t)r)==labs(sexp_unbox_fixnum(z))))
    res = sexp_make_fixnum(round(r));
  else
    res = sexp_make_flonum(ctx, r);
#if SEXP_USE_COMPLEX
  if (negativep)
    res = sexp_make_complex(ctx, SEXP_ZERO, res);
#endif
  sexp_gc_release1(ctx);
  return res;
}

#if SEXP_USE_BIGNUMS
sexp sexp_exact_sqrt (sexp ctx, sexp self, sexp_sint_t n, sexp z) {
  sexp_gc_var2(res, rem);
  sexp_gc_preserve2(ctx, res, rem);
  if (sexp_bignump(z)) {
    res = sexp_bignum_sqrt(ctx, z, &rem);
    res = sexp_cons(ctx, res, rem);
  } else {
    res = sexp_inexact_sqrt(ctx, self, n, z);
    if (sexp_flonump(res)) {
      res = sexp_bignum_normalize(sexp_double_to_bignum(ctx, trunc(sexp_flonum_value(res))));
    }
    if (!sexp_exceptionp(res)) {
      rem = sexp_mul(ctx, res, res);
      rem = sexp_sub(ctx, z, rem);
      res = sexp_cons(ctx, res, rem);
    }
  }
  sexp_gc_release2(ctx);
  return res;
}
#endif

sexp sexp_sqrt (sexp ctx, sexp self, sexp_sint_t n, sexp z) {
#if SEXP_USE_BIGNUMS
  sexp_gc_var2(res, rem);
  if (sexp_bignump(z)) {
    sexp_gc_preserve2(ctx, res, rem);
    res = sexp_bignum_sqrt(ctx, z, &rem);
    rem = sexp_bignum_normalize(rem);
    if (rem != SEXP_ZERO)
      res = sexp_make_flonum(ctx, sexp_fixnump(res) ? sexp_unbox_fixnum(res) : sexp_bignum_to_double(res));
    sexp_gc_release2(ctx);
    return res;
  }
#endif
  return sexp_inexact_sqrt(ctx, self, n, z);
}

#endif  /* SEXP_USE_MATH */

#if SEXP_USE_RATIOS || !SEXP_USE_FLONUMS
sexp sexp_generic_expt (sexp ctx, sexp x, sexp_sint_t e) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  for (res = SEXP_ONE, tmp = x; e > 0; e >>= 1) {
    if (e&1) res = sexp_mul(ctx, res, tmp);
    tmp = sexp_mul(ctx, tmp, tmp);
  }
  sexp_gc_release2(ctx);
  return res;
}
#endif

sexp sexp_expt_op (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp e) {
#if !SEXP_USE_FLONUMS
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, x);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, e);
  return sexp_generic_expt(ctx, x, sexp_unbox_fixnum(e));
#else
  long double f, x1, e1;
  sexp res;
#if SEXP_USE_BIGNUMS
  sexp_gc_var1(tmp);
#endif
#if SEXP_USE_COMPLEX
  if (sexp_complexp(x) || sexp_complexp(e))
    return sexp_complex_expt(ctx, x, e);
#endif
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
  else if (sexp_flonump(x))
    x1 = sexp_flonum_value(x);
#if SEXP_USE_RATIOS
  else if (sexp_ratiop(x)) {
    if (sexp_fixnump(e)) {
      return sexp_generic_expt(ctx, x, sexp_unbox_fixnum(e));
    } else {
      x1 = sexp_ratio_to_double(x);
    }
  }
#endif
  else
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, x);
  if (sexp_fixnump(e))
    e1 = sexp_unbox_fixnum(e);
  else if (sexp_flonump(e))
    e1 = sexp_flonum_value(e);
#if SEXP_USE_RATIOS
  else if (sexp_ratiop(e))
    e1 = sexp_ratio_to_double(e);
#endif
  else
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, e);
  f = pow(x1, e1);
  if ((f*1000.0 > SEXP_MAX_FIXNUM) || (f*1000.0 < SEXP_MIN_FIXNUM)
      || (! sexp_fixnump(x)) || (! sexp_fixnump(e)) || (e1 < 0.0)) {
#if SEXP_USE_BIGNUMS
    if (sexp_fixnump(x) && sexp_fixnump(e)) {
      sexp_gc_preserve1(ctx, tmp);
      tmp = sexp_fixnum_to_bignum(ctx, x);
      res = sexp_bignum_expt(ctx, tmp, e);
      sexp_gc_release1(ctx);
    } else
#endif
      res = sexp_make_flonum(ctx, f);
  } else
    res = sexp_make_fixnum((sexp_sint_t)round(f));
#if SEXP_USE_BIGNUMS
  }
#endif
  return res;
#endif  /* !SEXP_USE_FLONUMS */
}

#if SEXP_USE_RATIOS
sexp sexp_ratio_numerator_op (sexp ctx, sexp self, sexp_sint_t n, sexp rat) {
  sexp_assert_type(ctx, sexp_ratiop, SEXP_RATIO, rat);
  return sexp_ratio_numerator(rat);
}
sexp sexp_ratio_denominator_op (sexp ctx, sexp self, sexp_sint_t n, sexp rat) {
  sexp_assert_type(ctx, sexp_ratiop, SEXP_RATIO, rat);
  return sexp_ratio_denominator(rat);
}
#endif

#if SEXP_USE_COMPLEX
sexp sexp_complex_real_op (sexp ctx, sexp self, sexp_sint_t n, sexp cpx) {
  sexp_assert_type(ctx, sexp_complexp, SEXP_COMPLEX, cpx);
  return sexp_complex_real(cpx);
}
sexp sexp_complex_imag_op (sexp ctx, sexp self, sexp_sint_t n, sexp cpx) {
  sexp_assert_type(ctx, sexp_complexp, SEXP_COMPLEX, cpx);
  return sexp_complex_imag(cpx);
}
#endif

sexp sexp_exact_to_inexact (sexp ctx, sexp self, sexp_sint_t n, sexp i) {
  sexp_gc_var1(res);
  res = i;
  if (sexp_fixnump(i))
    res = sexp_fixnum_to_flonum(ctx, i);
#if SEXP_USE_FLONUMS
  else if (sexp_flonump(i))
    res = i;
#endif
#if SEXP_USE_BIGNUMS
  else if (sexp_bignump(i))
    res = sexp_make_flonum(ctx, sexp_bignum_to_double(i));
#endif
#if SEXP_USE_RATIOS
  else if (sexp_ratiop(i))
    res = sexp_make_flonum(ctx, sexp_ratio_to_double(i));
#endif
#if SEXP_USE_COMPLEX
  else if (sexp_complexp(i)) {
    sexp_gc_preserve1(ctx, res);
    res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
    sexp_complex_real(res) = sexp_exact_to_inexact(ctx, self, 1, sexp_complex_real(i));
    sexp_complex_imag(res) = sexp_exact_to_inexact(ctx, self, 1, sexp_complex_imag(i));
    sexp_gc_release1(ctx);
  }
#endif
  else
    res = sexp_type_exception(ctx, self, SEXP_FIXNUM, i);
  return res;
}

sexp sexp_inexact_to_exact (sexp ctx, sexp self, sexp_sint_t n, sexp z) {
  sexp_gc_var1(res);
  if (sexp_exactp(z)) {
    res = z;
  }
#if SEXP_USE_FLONUMS
  else if (sexp_flonump(z)) {
    if (isinf(sexp_flonum_value(z)) || isnan(sexp_flonum_value(z))) {
      res = sexp_xtype_exception(ctx, self, "exact: not a finite number", z);
    } else if (sexp_flonum_value(z) != trunc(sexp_flonum_value(z))) {
#if SEXP_USE_RATIOS
      res = sexp_double_to_ratio_2(ctx, sexp_flonum_value(z));
#else
      res = sexp_xtype_exception(ctx, self, "exact: not an integer", z);
#endif
#if SEXP_USE_BIGNUMS
    } else if ((sexp_flonum_value(z) > SEXP_MAX_FIXNUM)
               || sexp_flonum_value(z) < SEXP_MIN_FIXNUM) {
      res = sexp_double_to_bignum(ctx, sexp_flonum_value(z));
#endif
    } else {
      res = sexp_make_fixnum((sexp_sint_t)sexp_flonum_value(z));
    }
  }
#endif
#if SEXP_USE_COMPLEX
  else if (sexp_complexp(z)) {
    sexp_gc_preserve1(ctx, res);
    res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
    sexp_complex_real(res) = sexp_inexact_to_exact(ctx, self, 1, sexp_complex_real(z));
    sexp_complex_imag(res) = sexp_inexact_to_exact(ctx, self, 1, sexp_complex_imag(z));
    if (sexp_exceptionp(sexp_complex_real(res)))
      res = sexp_complex_real(res);
    else if (sexp_exceptionp(sexp_complex_imag(res)))
      res = sexp_complex_imag(res);
    else if (sexp_complex_imag(res) == SEXP_ZERO)
      res = sexp_complex_real(res);
    sexp_gc_release1(ctx);
  }
#endif
  else {
    res = sexp_type_exception(ctx, self, SEXP_FLONUM, z);
  }
  return res;
}

sexp sexp_string_cmp_op (sexp ctx, sexp self, sexp_sint_t n, sexp str1, sexp str2, sexp ci) {
  sexp_sint_t len1, len2, len, diff;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str1);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str2);
  len1 = sexp_string_size(str1);
  len2 = sexp_string_size(str2);
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

sexp sexp_string_utf8_index_ref (sexp ctx, sexp self, sexp_sint_t n, sexp str, sexp i) {
  sexp off;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, i);
  off = sexp_string_index_to_cursor(ctx, self, n, str, i);
  if (sexp_exceptionp(off)) return off;
  if (sexp_unbox_string_cursor(off) >= (sexp_sint_t)sexp_string_size(str))
    return sexp_user_exception(ctx, self, "string-ref: index out of range", i);
  return sexp_string_utf8_ref(ctx, str, off);
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
      i = ((i&0x0F)<<18) + ((sexp_read_char(ctx, port)&0x3F)<<12);
      i += (sexp_read_char(ctx, port)&0x3F)<<6;
      i += sexp_read_char(ctx, port)&0x3F;
    }
  }
  return sexp_make_character(i);
}

void sexp_push_utf8_char (sexp ctx, int i, sexp port) {
  unsigned char ch[6];
  int len = sexp_utf8_char_byte_count(i);
  sexp_utf8_encode_char(ch, len, i);
  if (sexp_port_stream(port))  {
    while (len>0)
      ungetc(ch[--len], sexp_port_stream(port));
  } else {
    while (len>0)
      sexp_port_buf(port)[--sexp_port_offset(port)] = ch[--len];
  }
}

#if SEXP_USE_MUTABLE_STRINGS

void sexp_string_utf8_set (sexp ctx, sexp str, sexp index, sexp ch) {
  sexp b;
  unsigned char *p, *q;
  int i = sexp_unbox_string_cursor(index), c = sexp_unbox_character(ch),
    old_len, new_len, len;
  p = (unsigned char*)sexp_string_data(str) + i;
  old_len = sexp_utf8_initial_byte_count(*p);
  new_len = sexp_utf8_char_byte_count(c);
  if (old_len != new_len) { /* resize bytes if needed */
    len = sexp_string_size(str)+(new_len-old_len);
    b = sexp_make_bytes(ctx, sexp_make_fixnum(len), SEXP_VOID);
    if (! sexp_exceptionp(b)) {
      q = (unsigned char*)sexp_bytes_data(b);
      memcpy(q, sexp_string_data(str), i);
      memcpy(q+i+new_len, p+old_len, len-i-new_len+1);
      sexp_string_bytes(str) = b;
      p = q + i;
    }
    sexp_string_size(str) += new_len - old_len;
  }
  sexp_utf8_encode_char(p, new_len, c);
  if (old_len != new_len)
    sexp_update_string_index_lookup(ctx, str);
}

sexp sexp_string_utf8_index_set (sexp ctx, sexp self, sexp_sint_t n, sexp str, sexp i, sexp ch) {
  sexp off;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, i);
  sexp_assert_type(ctx, sexp_charp, SEXP_CHAR, ch);
  off = sexp_string_index_to_cursor(ctx, self, n, str, i);
  if (sexp_exceptionp(off)) return off;
  if (sexp_unbox_string_cursor(off) >= (sexp_sint_t)sexp_string_size(str))
    return sexp_user_exception(ctx, self, "string-set!: index out of range", i);
  sexp_string_utf8_set(ctx, str, off, ch);
  return SEXP_VOID;
}

#endif
#endif

#if SEXP_USE_AUTO_FORCE
sexp sexp_make_promise (sexp ctx, sexp self, sexp_sint_t n, sexp done, sexp val) {
  sexp res = sexp_alloc_type(ctx, promise, SEXP_PROMISE);
  sexp_promise_donep(res) = sexp_unbox_boolean(done);
  sexp_promise_value(res) = val;
  return res;
}
#endif

/***************************** opcodes ********************************/

#if SEXP_USE_TYPE_DEFS

sexp sexp_type_slot_offset_op (sexp ctx , sexp self, sexp_sint_t n, sexp type, sexp slot) {
  sexp cpl, slots, *v;
  int i, offset, len;
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
  for (i=len-1; i>=0; --i)
    for (slots=sexp_type_slots(v[i]), offset=0; sexp_pairp(slots); slots=sexp_cdr(slots), ++offset)
      if (sexp_car(slots) == slot) {
        while (--i>=0)
          offset += sexp_unbox_fixnum(sexp_length(ctx, sexp_type_slots(v[i])));
        return sexp_make_fixnum(offset);
      }
  return SEXP_FALSE;
}

sexp sexp_make_type_predicate_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp type) {
  if (sexp_typep(type)) type = sexp_make_fixnum(sexp_type_tag(type));
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, type);
  return sexp_make_opcode(ctx, self, name, sexp_make_fixnum(SEXP_OPC_TYPE_PREDICATE),
                          sexp_make_fixnum(SEXP_OP_TYPEP), SEXP_ONE, SEXP_ZERO,
                          SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, type, NULL, NULL);
}

sexp sexp_make_constructor_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp type) {
  sexp_uint_t type_size;
  if (sexp_typep(type)) type = sexp_make_fixnum(sexp_type_tag(type));
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, type);
  type_size = sexp_type_size_base(sexp_type_by_index(ctx, sexp_unbox_fixnum(type)));
  return sexp_make_opcode(ctx, self, name, sexp_make_fixnum(SEXP_OPC_CONSTRUCTOR),
                          sexp_make_fixnum(SEXP_OP_MAKE), SEXP_ZERO, SEXP_ZERO,
                          SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, type,
                          sexp_make_fixnum(type_size), NULL);
}

sexp sexp_make_getter_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp type, sexp index) {
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

sexp sexp_make_setter_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp type, sexp index) {
  sexp res;
  if (sexp_typep(type)) type = sexp_make_fixnum(sexp_type_tag(type));
  if ((! sexp_fixnump(type))  || (sexp_unbox_fixnum(type) < 0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, type);
  if ((! sexp_fixnump(index)) || (sexp_unbox_fixnum(index) < 0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, index);
  res
    = sexp_make_opcode(ctx, self, name, sexp_make_fixnum(SEXP_OPC_SETTER),
                       sexp_make_fixnum(SEXP_OP_SLOT_SET), SEXP_TWO, SEXP_ZERO,
                       type, SEXP_ZERO, SEXP_ZERO, type, index, NULL);
  if (!sexp_exceptionp(res)) sexp_opcode_return_type(res) = SEXP_VOID;
  return res;
}

#endif

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
    sexp_opcode_class(res) = (unsigned char)sexp_unbox_fixnum(op_class);
    sexp_opcode_code(res) = (unsigned char)sexp_unbox_fixnum(code);
    sexp_opcode_num_args(res) = (unsigned char)sexp_unbox_fixnum(num_args);
    sexp_opcode_flags(res) = (unsigned char)sexp_unbox_fixnum(flags);
    sexp_opcode_arg1_type(res) = arg1t;
    sexp_opcode_arg2_type(res) = arg2t;
    sexp_opcode_inverse(res) = (unsigned char)sexp_unbox_fixnum(invp);
    sexp_opcode_data(res) = data;
    sexp_opcode_data2(res) = data2;
    sexp_opcode_func(res) = func;
    sexp_opcode_name(res) = name;
#if SEXP_USE_DL
    sexp_opcode_dl(res) = sexp_context_dl(ctx);
#endif
  }
  return res;
}

sexp sexp_make_foreign (sexp ctx, const char *name, int num_args,
                        int flags, const char *fname, sexp_proc1 f, sexp data) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
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
  sexp_opcode_name(res) = sexp_c_string(ctx, name, -1);
  sexp_opcode_data(res) = data;
  sexp_opcode_func(res) = f;
  if (fname) {
    sexp_opcode_data2(res) = sexp_c_string(ctx, fname, -1);
  }
#if SEXP_USE_DL
  sexp_opcode_dl(res) = sexp_context_dl(ctx);
#endif
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_define_foreign_aux (sexp ctx, sexp env, const char *name, int num_args,
                              int flags, const char *fname, sexp_proc1 f, sexp data) {
  sexp_gc_var2(sym, res);
  sexp_gc_preserve2(ctx, sym, res);
  res = sexp_make_foreign(ctx, name, num_args, flags, fname, f, data);
  if (!sexp_exceptionp(res))
    sexp_env_define(ctx, env, sym = sexp_intern(ctx, name, -1), res);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_define_foreign_param_aux (sexp ctx, sexp env, const char *name,
                                    int num_args, const char *fname, sexp_proc1 f, const char *param) {
  sexp res = SEXP_FALSE;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  tmp = sexp_intern(ctx, param, -1);
  tmp = sexp_env_ref(ctx, env, tmp, SEXP_FALSE);
  if (sexp_opcodep(tmp))
    res = sexp_define_foreign_aux(ctx, env, name, num_args, 3, fname, f, tmp);
  sexp_gc_release1(ctx);
  return res;
}

/*********************** standard environment *************************/

/* The 10 core forms.  Note quote can be defined as derived syntax: */

/*  (define-syntax quote */
/*    (lambda (expr use-env mac-env) */
/*      (list */
/*       (make-syntactic-closure mac-env (list) (syntax-quote syntax-quote)) */
/*       (strip-syntactic-closures (car (cdr expr)))))) */

static struct sexp_core_form_struct core_forms[] = {
  {SEXP_CORE_DEFINE, (sexp)"define"},
  {SEXP_CORE_SET, (sexp)"set!"},
  {SEXP_CORE_LAMBDA, (sexp)"lambda"},
  {SEXP_CORE_IF, (sexp)"if"},
  {SEXP_CORE_BEGIN, (sexp)"begin"},
  {SEXP_CORE_QUOTE, (sexp)"quote"},
  {SEXP_CORE_SYNTAX_QUOTE, (sexp)"syntax-quote"},
  {SEXP_CORE_DEFINE_SYNTAX, (sexp)"%define-syntax"},
  {SEXP_CORE_LET_SYNTAX, (sexp)"%let-syntax"},
  {SEXP_CORE_LETREC_SYNTAX, (sexp)"%letrec-syntax"},
};

sexp sexp_make_env_op (sexp ctx, sexp self, sexp_sint_t n) {
  sexp e = sexp_alloc_type(ctx, env, SEXP_ENV);
  sexp_env_lambda(e) = NULL;
  sexp_env_parent(e) = NULL;
  sexp_env_bindings(e) = SEXP_NULL;
#if SEXP_USE_RENAME_BINDINGS
  sexp_env_renames(e) = SEXP_NULL;
#endif
  return e;
}

sexp sexp_make_null_env_op (sexp ctx, sexp self, sexp_sint_t n, sexp version) {
  sexp_uint_t i;
  sexp_gc_var2(e, core);
  sexp_gc_preserve2(ctx, e, core);
  e = sexp_make_env(ctx);
  for (i=0; i<(sizeof(core_forms)/sizeof(core_forms[0])); i++) {
    core = sexp_copy_core(ctx, &core_forms[i]);
    sexp_env_define(ctx, e, sexp_intern(ctx, (char*)sexp_core_name(core), -1), core);
    sexp_core_name(core) = sexp_c_string(ctx, (char*)sexp_core_name(core), -1);
  }
  sexp_gc_release2(ctx);
  return e;
}

sexp sexp_make_primitive_env_op (sexp ctx, sexp self, sexp_sint_t n, sexp version) {
  int i;
  sexp_gc_var4(e, op, sym, name);
  sexp_gc_preserve4(ctx, e, op, sym, name);
  e = sexp_make_null_env(ctx, version);
  for (i=0; sexp_primitive_opcodes[i].op_class; i++) {
    op = sexp_copy_opcode(ctx, &sexp_primitive_opcodes[i]);
    name = sexp_intern(ctx, (char*)sexp_opcode_name(op), -1);
    sexp_opcode_name(op) = sexp_c_string(ctx, (char*)sexp_opcode_name(op), -1);
    if (sexp_opcode_opt_param_p(op) && sexp_opcode_data(op)) {
      sym = sexp_intern(ctx, (char*)sexp_opcode_data(op), -1);
      sexp_opcode_data(op) = sexp_env_ref(ctx, e, sym, SEXP_FALSE);
    } else if (sexp_opcode_class(op) == SEXP_OPC_PARAMETER) {
      sexp_opcode_data(op) = sexp_cons(ctx, name, SEXP_FALSE);
    }
    if (sexp_opcode_class(op) == SEXP_OPC_FOREIGN && sexp_opcode_data2(op)) {
      sexp_opcode_data2(op) = sexp_c_string(ctx, (char*)sexp_opcode_data2(op), -1);
    }
    sexp_env_define(ctx, e, name, op);
  }
  sexp_gc_release4(ctx);
  return e;
}

char* sexp_find_module_file_raw (sexp ctx, const char *file) {
  sexp ls;
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
  for ( ; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    dir = sexp_string_data(sexp_car(ls));
    dirlen = sexp_string_size(sexp_car(ls));
    slash = dir[dirlen-1] == '/';
    len = dirlen+filelen+2-slash;
    path = (char*) sexp_malloc(len);
    if (! path) return NULL;
    memcpy(path, dir, dirlen);
    if (! slash) path[dirlen] = '/';
    memcpy(path+len-filelen-1, file, filelen);
    path[len-1] = '\0';
    if (sexp_find_static_library(path) || file_exists_p(path, buf))
      return path;
    free(path);
  }

  return NULL;
}

sexp sexp_find_module_file (sexp ctx, const char *file) {
  char* path = sexp_find_module_file_raw(ctx, file);
  sexp res = sexp_c_string(ctx, path, -1);
  if (path) free(path);
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

sexp sexp_current_environment (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_context_env(ctx);
}

#if SEXP_USE_MODULES
sexp sexp_current_module_path_op (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (sexp_pairp(x) && sexp_stringp(sexp_car(x))) {
    sexp_global(ctx, SEXP_G_MODULE_PATH) = sexp_reverse(ctx, x);
    sexp_global(ctx, SEXP_G_MODULE_PATH) = sexp_reverse(ctx, sexp_global(ctx, SEXP_G_MODULE_PATH));
  }
  return sexp_global(ctx, SEXP_G_MODULE_PATH);
}
sexp sexp_find_module_file_op (sexp ctx, sexp self, sexp_sint_t n, sexp file) {
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, file);
  return sexp_find_module_file(ctx, sexp_string_data(file));
}
sexp sexp_load_module_file_op (sexp ctx, sexp self, sexp_sint_t n, sexp file, sexp env) {
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, file);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  return sexp_load_module_file(ctx, sexp_string_data(file), env);
}
sexp sexp_set_current_environment (sexp ctx, sexp self, sexp_sint_t n, sexp env) {
  sexp oldenv;
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  oldenv = sexp_context_env(ctx);
  sexp_context_env(ctx) = env;
  return oldenv;
}
sexp sexp_meta_environment (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_global(ctx, SEXP_G_META_ENV);
}
#endif

sexp sexp_add_module_directory_op (sexp ctx, sexp self, sexp_sint_t n, sexp dir, sexp appendp) {
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

#if SEXP_USE_GREEN_THREADS
sexp sexp_dk (sexp ctx, sexp self, sexp_sint_t n, sexp val) {
  if (sexp_not(val)) {
    return sexp_context_dk(ctx) ? sexp_context_dk(ctx) : SEXP_FALSE;
  } else {
    sexp_context_dk(ctx) = val;
    return SEXP_VOID;
  }
}
#endif

sexp sexp_thread_parameters (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res = sexp_context_params(ctx);
  return res ? res : SEXP_NULL;
}

sexp sexp_thread_parameters_set (sexp ctx, sexp self, sexp_sint_t n, sexp new) {
  sexp_context_params(ctx) = new;
  return SEXP_VOID;
}

void sexp_set_parameter (sexp ctx, sexp env, sexp name, sexp value) {
  sexp param = sexp_env_ref(ctx, env, name, SEXP_FALSE);
  if (sexp_opcodep(param)) {
    if (! sexp_pairp(sexp_opcode_data(param)))
      sexp_opcode_data(param) = sexp_cons(ctx, name, value);
    else
      sexp_cdr(sexp_opcode_data(param)) = value;
  } else {
    sexp_warn(ctx, "can't set non-parameter: ", name);
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
  int len;
  char init_file[128];
  sexp_gc_var3(op, tmp, sym);
  sexp_gc_preserve3(ctx, op, tmp, sym);
  if (!e) e = sexp_context_env(ctx);
  sexp_env_define(ctx, e, sym=sexp_intern(ctx, "*shared-object-extension*", -1),
                  tmp=sexp_c_string(ctx, sexp_so_extension, -1));
  sexp_env_define(ctx, e, sym=sexp_intern(ctx, "*features*", -1), sexp_global(ctx, SEXP_G_FEATURES));
  sexp_global(ctx, SEXP_G_OPTIMIZATIONS) = SEXP_NULL;
#if SEXP_USE_SIMPLIFY
  op = sexp_make_foreign(ctx, "sexp_simplify", 1, 0,
                         NULL, (sexp_proc1)sexp_simplify, SEXP_VOID);
  tmp = sexp_cons(ctx, sexp_make_fixnum(500), op);
  sexp_push(ctx, sexp_global(ctx, SEXP_G_OPTIMIZATIONS), tmp);
#endif
  sexp_global(ctx, SEXP_G_ERR_HANDLER)
    = sexp_env_ref(ctx, e, sym=sexp_intern(ctx, "current-exception-handler", -1), SEXP_FALSE);
  /* load init-7.scm */
  len = strlen(sexp_init_file);
  strncpy(init_file, sexp_init_file, len);
  init_file[len] = (char)sexp_unbox_fixnum(version) + '0';
  strncpy(init_file + len + 1, sexp_init_file_suffix, strlen(sexp_init_file_suffix));
  init_file[len + 1 + strlen(sexp_init_file_suffix)] = 0;
  tmp = sexp_load_module_file(ctx, init_file, e);
  sexp_set_parameter(ctx, e, sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL), e);
  /* load and bind meta-7.scm env */
#if SEXP_USE_MODULES
  if (!sexp_exceptionp(tmp)) {
    if (!sexp_envp(tmp=sexp_global(ctx, SEXP_G_META_ENV))) {
      tmp = sexp_make_env(ctx);
      if (! sexp_exceptionp(tmp)) {
        sexp_global(ctx, SEXP_G_META_ENV) = tmp;
        sexp_env_parent(tmp) = e;
        op = sexp_load_module_file(ctx, sexp_meta_file, tmp);
        if (sexp_exceptionp(op))
          sexp_print_exception(ctx, op, sexp_current_error_port(ctx));
      }
    }
    if (!sexp_exceptionp(tmp)) {
      sym = sexp_intern(ctx, "repl-import", -1);
      tmp = sexp_env_ref(ctx, tmp, sym, SEXP_VOID);
      sym = sexp_intern(ctx, "import", -1);
      /* splice import in place to mutate both this env and the */
      /* frozen version in the meta env) */
      tmp = sexp_cons(ctx, sym, tmp);
      sexp_env_next_cell(tmp) = sexp_env_next_cell(sexp_env_bindings(e));
      sexp_env_next_cell(sexp_env_bindings(e)) = tmp;
    }
  }
#endif
  sexp_gc_release3(ctx);
  return sexp_exceptionp(tmp) ? tmp : e;
}

sexp sexp_make_standard_env_op (sexp ctx, sexp self, sexp_sint_t n, sexp version) {
  sexp_gc_var1(env);
  sexp_gc_preserve1(ctx, env);
  env = sexp_make_primitive_env(ctx, version);
  if (! sexp_exceptionp(env)) env = sexp_load_standard_env(ctx, env, SEXP_SEVEN);
  sexp_gc_release1(ctx);
  return env;
}

sexp sexp_env_parent_op (sexp ctx, sexp self, sexp_sint_t n, sexp e) {
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, e);
  return sexp_env_parent(e) ? sexp_env_parent(e) : SEXP_FALSE;
}

#if SEXP_USE_RENAME_BINDINGS
#define sexp_same_bindingp(x, y) ((x) == (y))
#else
#define sexp_same_bindingp(x, y) (sexp_env_value(x) == sexp_env_value(y))
#endif

/* Rewrite to in place: to => empty->imports->to */
sexp sexp_env_import_op (sexp ctx, sexp self, sexp_sint_t n, sexp to, sexp from, sexp ls, sexp immutp) {
  sexp oldname, newname;
  sexp_gc_var3(value, oldcell, tmp);
  sexp_gc_preserve3(ctx, value, oldcell, tmp);
  if (! sexp_envp(to)) to = sexp_context_env(ctx);
  if (! sexp_envp(from)) from = sexp_context_env(ctx);
  /* create an empty imports env frame */
  value = sexp_make_env(ctx);
  sexp_env_parent(value) = sexp_env_parent(to);
  sexp_env_parent(to) = value;
  sexp_env_lambda(value) = sexp_env_lambda(to);
  sexp_env_lambda(to) = NULL;
  sexp_env_bindings(value) = sexp_env_bindings(to);
  sexp_env_bindings(to) = SEXP_NULL;
#if SEXP_USE_RENAME_BINDINGS
  sexp_env_renames(value) = sexp_env_renames(to);
  sexp_env_renames(to) = SEXP_NULL;
#endif
  sexp_immutablep(value) = sexp_immutablep(to);
  sexp_immutablep(to) = sexp_truep(immutp);
  /* import the bindings, one at a time or in bulk */
  if (sexp_not(ls)) {
    sexp_env_bindings(to) = sexp_env_bindings(from);
#if SEXP_USE_RENAME_BINDINGS
    sexp_env_renames(to) = sexp_env_renames(from);
#endif
  } else {
    for ( ; sexp_pairp(ls); ls=sexp_cdr(ls)) {
      if (sexp_pairp(sexp_car(ls))) {
        newname = sexp_caar(ls); oldname = sexp_cdar(ls);
      } else {
        newname = oldname = sexp_car(ls);
      }
      oldcell = sexp_env_cell(ctx, to, newname, 0);
      value = sexp_env_cell(ctx, from, oldname, 0);
      if (value) {
#if SEXP_USE_RENAME_BINDINGS
        sexp_env_rename(ctx, to, newname, value);
#else
        sexp_env_push(ctx, to, tmp, newname, sexp_cdr(value));
#endif
#if SEXP_USE_WARN_UNDEFS
        if (oldcell
            && sexp_cdr(oldcell) != SEXP_UNDEF
            && !sexp_same_bindingp(oldcell, value))
          sexp_warn(ctx, "importing already defined binding: ", newname);
      } else {
        sexp_warn(ctx, "importing undefined variable: ", oldname);
#endif
      }
    }
  }
  /* create a new empty frame for future defines */
  value = sexp_make_env(ctx);
  sexp_env_parent(value) = sexp_env_parent(to);
  sexp_env_lambda(value) = sexp_env_lambda(to);
  sexp_env_bindings(value) = sexp_env_bindings(to);
#if SEXP_USE_RENAME_BINDINGS
  sexp_env_renames(value) = sexp_env_renames(to);
  sexp_env_renames(to) = SEXP_NULL;
#endif
  sexp_env_parent(to) = value;
  sexp_env_bindings(to) = SEXP_NULL;
  sexp_immutablep(to) = 0;
  sexp_gc_release3(ctx);
  return SEXP_VOID;
}

/************************** eval interface ****************************/

sexp sexp_generate_op (sexp ctx, sexp self, sexp_sint_t n, sexp ast, sexp env) {
  sexp_gc_var3(ctx2, vec, res);
  if (sexp_contextp(env)) {
    ctx2 = env;
  } else {
    sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
    ctx2 = sexp_make_eval_context(ctx, NULL, env, 0, 0);
  }
  sexp_gc_preserve3(ctx, ctx2, vec, res);
  sexp_free_vars(ctx2, ast, SEXP_NULL);    /* should return SEXP_NULL */
  sexp_emit_enter(ctx2);
  sexp_generate(ctx2, 0, 0, 0, ast);
  res = sexp_complete_bytecode(ctx2);
  if (!sexp_exceptionp(res)) {
    sexp_context_specific(ctx2) = SEXP_FALSE;
    vec = sexp_make_vector(ctx2, 0, SEXP_VOID);
    if (sexp_exceptionp(vec)) res = vec;
    else res = sexp_make_procedure(ctx2, SEXP_ZERO, SEXP_ZERO, res, vec);
  }
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_compile_op (sexp ctx, sexp self, sexp_sint_t n, sexp obj, sexp env) {
  sexp_gc_var3(ast, tmp, res);
  sexp ctx2;
  if (! env) env = sexp_context_env(ctx);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  sexp_gc_preserve3(ctx, ast, tmp, res);
  ctx2 = sexp_make_eval_context(ctx, NULL, env, 0, 0);
  if (sexp_exceptionp(ctx2)) {
    res = ctx2;
  } else {
    tmp = sexp_context_child(ctx);
    sexp_context_child(ctx) = ctx2;
    ast = sexp_analyze(ctx2, obj);
    if (sexp_exceptionp(ast)) {
      res = ast;
    } else {
      res = sexp_global(ctx2, SEXP_G_OPTIMIZATIONS);
      for ( ; sexp_pairp(res) && !sexp_exceptionp(ast); res=sexp_cdr(res))
        ast = sexp_apply1(ctx2, sexp_cdar(res), ast);
      if (sexp_exceptionp(ast)) {
        res = ast;
      } else {
        res = sexp_generate_op(ctx2, self, n, ast, ctx2);
      }
    }
    sexp_context_child(ctx) = tmp;
    sexp_context_last_fp(ctx) = sexp_context_last_fp(ctx2);
  }
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_eval_op (sexp ctx, sexp self, sexp_sint_t n, sexp obj, sexp env) {
  sexp_sint_t top;
  sexp ctx2;
  sexp_gc_var3(res, tmp, params);
  if (! env) env = sexp_context_env(ctx);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  sexp_gc_preserve3(ctx, res, tmp, params);
  top = sexp_context_top(ctx);
  params = sexp_context_params(ctx);
  sexp_context_params(ctx) = SEXP_NULL;
  ctx2 = sexp_make_eval_context(ctx, NULL, env, 0, 0);
  tmp = sexp_context_child(ctx);
  sexp_context_child(ctx) = ctx2;
  res = sexp_exceptionp(ctx2) ? ctx2 : sexp_compile_op(ctx2, self, n, obj, env);
  if (! sexp_exceptionp(res))
    res = sexp_apply(ctx2, res, SEXP_NULL);
  sexp_context_child(ctx) = tmp;
  sexp_context_params(ctx) = params;
  sexp_context_top(ctx) = top;
  sexp_context_last_fp(ctx) = sexp_context_last_fp(ctx2);
  sexp_gc_release3(ctx);
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
