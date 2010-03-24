/*  eval.c -- evaluator library implementation                */
/*  Copyright (c) 2009-2010 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"

/************************************************************************/

static int scheme_initialized_p = 0;

#if SEXP_USE_DEBUG_VM
static void sexp_print_stack (sexp ctx, sexp *stack, int top, int fp, sexp out) {
  int i;
  if (! sexp_oportp(out)) out = sexp_current_error_port(ctx);
  for (i=0; i<top; i++) {
    sexp_printf(ctx, out, "%s %02d: ", ((i==fp) ? "*" : " "), i);
    sexp_write(ctx, stack[i], out);
    sexp_printf(ctx, out, "\n");
  }
}
#endif

static sexp analyze (sexp ctx, sexp x);
static void generate (sexp ctx, sexp x);

#if SEXP_USE_MODULES
static sexp sexp_load_module_file_op (sexp ctx sexp_api_params(self, n), sexp file, sexp env);
static sexp sexp_find_module_file_op (sexp ctx sexp_api_params(self, n), sexp file);
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

/********************** environment utilities ***************************/

static sexp sexp_env_cell_loc (sexp env, sexp key, sexp *varenv) {
  sexp ls;

  do {
    for (ls=sexp_env_bindings(env); sexp_pairp(ls); ls=sexp_cdr(ls))
      if (sexp_caar(ls) == key) {
        if (varenv) *varenv = env;
        return sexp_car(ls);
      }
    env = sexp_env_parent(env);
  } while (env);

  return NULL;
}

sexp sexp_env_cell (sexp env, sexp key) {
  return sexp_env_cell_loc(env, key, NULL);
}

static sexp sexp_env_cell_create (sexp ctx, sexp env, sexp key,
                                  sexp value, sexp *varenv) {
  sexp_gc_var1(cell);
  cell = sexp_env_cell_loc(env, key, varenv);
  if (! cell) {
    sexp_gc_preserve1(ctx, cell);
    cell = sexp_cons(ctx, key, value);
    while (sexp_env_lambda(env) || sexp_env_syntactic_p(env))
      env = sexp_env_parent(env);
    sexp_env_bindings(env) = sexp_cons(ctx, cell, sexp_env_bindings(env));
    if (varenv) *varenv = env;
    sexp_gc_release1(ctx);
  }
  return cell;
}

sexp sexp_env_ref (sexp env, sexp key, sexp dflt) {
  sexp cell = sexp_env_cell(env, key);
  return (cell ? sexp_cdr(cell) : dflt);
}

sexp sexp_env_global_ref (sexp env, sexp key, sexp dflt) {
  while (sexp_env_lambda(env) && sexp_env_parent(env))
    env = sexp_env_parent(env);
  return sexp_env_ref(env, key, dflt);
}

sexp sexp_env_define (sexp ctx, sexp env, sexp key, sexp value) {
  sexp cell = sexp_assq(ctx, key, sexp_env_bindings(env)), res=SEXP_VOID;
  sexp_gc_var1(tmp);
  if (sexp_immutablep(env)) {
    res = sexp_type_exception(ctx, "immutable binding", key);
  } else {
    sexp_gc_preserve1(ctx, tmp);
    if (sexp_truep(cell)) {
      if (sexp_immutablep(cell))
        res = sexp_type_exception(ctx, "immutable binding", key);
      else
        sexp_cdr(cell) = value;
    } else {
      tmp = sexp_cons(ctx, key, value);
      sexp_push(ctx, sexp_env_bindings(env), tmp);
    }
    sexp_gc_release1(ctx);
  }
  return res;
}

sexp sexp_env_exports_op (sexp ctx sexp_api_params(self, n), sexp env) {
  sexp ls;
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_NULL;
  for (ls=sexp_env_bindings(env); sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_push(ctx, res, sexp_caar(ls));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_extend_env (sexp ctx, sexp env, sexp vars, sexp value) {
  sexp_gc_var2(e, tmp);
  sexp_gc_preserve2(ctx, e, tmp);
  e = sexp_alloc_type(ctx, env, SEXP_ENV);
  sexp_env_parent(e) = env;
  sexp_env_bindings(e) = SEXP_NULL;
  for ( ; sexp_pairp(vars); vars = sexp_cdr(vars)) {
    tmp = sexp_cons(ctx, sexp_car(vars), value);
    sexp_push(ctx, sexp_env_bindings(e), tmp);
  }
  sexp_gc_release2(ctx);
  return e;
}

static sexp sexp_reverse_flatten_dot (sexp ctx, sexp ls) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  for (res=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_push(ctx, res, sexp_car(ls));
  sexp_gc_release1(ctx);
  return (sexp_nullp(ls) ? res : sexp_cons(ctx, ls, res));
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

static void expand_bcode (sexp ctx, sexp_uint_t size) {
  sexp tmp;
  if (sexp_bytecode_length(sexp_context_bc(ctx))
      < (sexp_context_pos(ctx))+size) {
    tmp=sexp_alloc_bytecode(ctx, sexp_bytecode_length(sexp_context_bc(ctx))*2);
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

static void emit (sexp ctx, char c)  {
  expand_bcode(ctx, 1);
  sexp_bytecode_data(sexp_context_bc(ctx))[sexp_context_pos(ctx)++] = c;
}

static void emit_word (sexp ctx, sexp_uint_t val)  {
  unsigned char *data;
  expand_bcode(ctx, sizeof(sexp));
  data = sexp_bytecode_data(sexp_context_bc(ctx));
  sexp_context_align_pos(ctx);
  *((sexp_uint_t*)(&(data[sexp_context_pos(ctx)]))) = val;
  sexp_context_pos(ctx) += sizeof(sexp);
}

static void emit_push (sexp ctx, sexp obj) {
  emit(ctx, SEXP_OP_PUSH);
  emit_word(ctx, (sexp_uint_t)obj);
  if (sexp_pointerp(obj) && ! sexp_symbolp(obj))
    sexp_push(ctx, sexp_bytecode_literals(sexp_context_bc(ctx)), obj);
}

static sexp finalize_bytecode (sexp ctx) {
  sexp bc;
  emit(ctx, SEXP_OP_RET);
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
  return bc;
}

sexp sexp_make_procedure (sexp ctx, sexp flags, sexp num_args,
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
  sexp_gc_var2(tmp, vec);
  ctx = sexp_make_child_context(ctx, NULL);
  sexp_gc_preserve2(ctx, tmp, vec);
  tmp = sexp_intern(ctx, "*current-exception-handler*", -1);
  sexp_global(ctx, SEXP_G_ERR_HANDLER)
    = sexp_env_cell_create(ctx, sexp_context_env(ctx), tmp, SEXP_FALSE, NULL);
  emit(ctx, SEXP_OP_RESUMECC);
  sexp_global(ctx, SEXP_G_RESUMECC_BYTECODE) = finalize_bytecode(ctx);
  ctx = sexp_make_child_context(ctx, NULL);
  emit(ctx, SEXP_OP_DONE);
  tmp = finalize_bytecode(ctx);
  vec = sexp_make_vector(ctx, 0, SEXP_VOID);
  sexp_global(ctx, SEXP_G_FINAL_RESUMER)
    = sexp_make_procedure(ctx, SEXP_ZERO, SEXP_ZERO, tmp, vec);
  sexp_bytecode_name(sexp_procedure_code(sexp_global(ctx, SEXP_G_FINAL_RESUMER)))
    = sexp_intern(ctx, "final-resumer", -1);
  sexp_global(ctx, SEXP_G_MODULE_PATH) = SEXP_NULL;
  sexp_add_path(ctx, sexp_default_module_dir);
  sexp_add_path(ctx, getenv(SEXP_MODULE_PATH_VAR));
  tmp = sexp_c_string(ctx, "./lib", 5);
  sexp_push(ctx, sexp_global(ctx, SEXP_G_MODULE_PATH), tmp);
  tmp = sexp_c_string(ctx, ".", 1);
  sexp_push(ctx, sexp_global(ctx, SEXP_G_MODULE_PATH), tmp);
  sexp_gc_release2(ctx);
}

sexp sexp_make_eval_context (sexp ctx, sexp stack, sexp env, sexp_uint_t size) {
  sexp_gc_var1(res);
  if (ctx) sexp_gc_preserve1(ctx, res);
  res = sexp_make_context(ctx, size);
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
  sexp_context_env(res) = (env ? env : sexp_make_primitive_env(res, SEXP_FIVE));
  if (! ctx) sexp_init_eval_context_globals(res);
  if (ctx) sexp_gc_release1(ctx);
  return res;
}

sexp sexp_make_child_context (sexp ctx, sexp lambda) {
  sexp res = sexp_make_eval_context(ctx,
                                    sexp_context_stack(ctx),
                                    sexp_context_env(ctx),
                                    0);
  sexp_context_lambda(res) = lambda;
  sexp_context_top(res) = sexp_context_top(ctx);
  sexp_context_fv(res) = sexp_context_fv(ctx);
  sexp_context_tracep(res) = sexp_context_tracep(ctx);
  return res;
}

/**************************** identifiers *****************************/

static sexp sexp_identifierp_op (sexp ctx sexp_api_params(self, n), sexp x) {
  return sexp_make_boolean(sexp_idp(x));
}

static sexp sexp_syntactic_closure_expr_op (sexp ctx sexp_api_params(self, n), sexp x) {
  return (sexp_synclop(x) ? sexp_synclo_expr(x) : x);
}

static sexp sexp_strip_synclos (sexp ctx, sexp x) {
  sexp res;
  sexp_gc_var2(kar, kdr);
  sexp_gc_preserve2(ctx, kar, kdr);
 loop:
  if (sexp_synclop(x)) {
    x = sexp_synclo_expr(x);
    goto loop;
  } else if (sexp_pairp(x)) {
    kar = sexp_strip_synclos(ctx, sexp_car(x));
    kdr = sexp_strip_synclos(ctx, sexp_cdr(x));
    res = sexp_cons(ctx, kar, kdr);
    sexp_immutablep(res) = 1;
  } else {
    res = x;
  }
  sexp_gc_release2(ctx);
  return res;
}

static sexp sexp_identifier_eq_op (sexp ctx sexp_api_params(self, n), sexp e1, sexp id1, sexp e2, sexp id2) {
  sexp cell, lam1=SEXP_FALSE, lam2=SEXP_FALSE;
  if (sexp_synclop(id1)) {
    e1 = sexp_synclo_env(id1);
    id1 = sexp_synclo_expr(id1);
  }
  if (sexp_synclop(id2)) {
    e2 = sexp_synclo_env(id2);
    id2 = sexp_synclo_expr(id2);
  }
  cell = sexp_env_cell(e1, id1);
  if (cell && sexp_lambdap(sexp_cdr(cell)))
    lam1 = sexp_cdr(cell);
  cell = sexp_env_cell(e2, id2);
  if (cell && sexp_lambdap(sexp_cdr(cell)))
    lam2 = sexp_cdr(cell);
  return sexp_make_boolean((id1 == id2) && (lam1 == lam2));
}

/************************* the compiler ***************************/

static sexp analyze_app (sexp ctx, sexp x) {
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
  sexp_gc_release2(ctx);
  return (sexp_pairp(res) ? sexp_nreverse(ctx, res) : res);
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
  cell = sexp_env_cell_loc(env, x, varenv);
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
  sexp name, ls;
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
  res = sexp_make_lambda(ctx, sexp_cadr(x));
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
              sexp_make_set(ctx2, analyze_var_ref(ctx2, name, NULL), value));
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
  sexp_gc_release1(ctx);
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
      tmp = sexp_cons(ctx, name, sexp_context_lambda(ctx));
      sexp_push(ctx, sexp_env_bindings(env), tmp);
      sexp_push(ctx, sexp_lambda_sv(sexp_env_lambda(env)), name);
      sexp_push(ctx, sexp_lambda_locals(sexp_env_lambda(env)), name);
      sexp_push(ctx, sexp_lambda_defs(sexp_env_lambda(env)), x);
      res = SEXP_VOID;
    } else {
      if (sexp_synclop(name)) name = sexp_synclo_expr(name);
      sexp_env_cell_create(ctx, env, name, SEXP_VOID, NULL);
      if (sexp_pairp(sexp_cadr(x))) {
        tmp = sexp_cons(ctx, sexp_cdadr(x), sexp_cddr(x));
        tmp = sexp_cons(ctx, SEXP_VOID, tmp);
        value = analyze_lambda(ctx, tmp);
      } else
        value = analyze(ctx, sexp_caddr(x));
      ref = analyze_var_ref(ctx, name, &varenv);
      if (sexp_exceptionp(ref))
        res = ref;
      else if (sexp_exceptionp(value))
        res = value;
      else if (varenv && sexp_immutablep(varenv))
        res = sexp_compile_error(ctx, "immutable binding", name);
      else
        res = sexp_make_set(ctx, ref, value);
    }
  }
  sexp_gc_release4(ctx);
  return res;
}

static sexp analyze_bind_syntax (sexp ls, sexp eval_ctx, sexp bind_ctx) {
  sexp res = SEXP_VOID, name;
  sexp_gc_var3(proc, mac, tmp);
  sexp_gc_preserve3(eval_ctx, proc, mac, tmp);
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
        tmp = sexp_cons(eval_ctx, name, mac);
        sexp_push(eval_ctx, sexp_env_bindings(sexp_context_env(bind_ctx)), tmp);
      } else {
        res = (sexp_exceptionp(proc) ? proc
               : sexp_compile_error(eval_ctx, "non-procedure macro:", proc));
        break;
      }
    }
  }
  sexp_gc_release3(eval_ctx);
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
      cell = sexp_env_cell(sexp_context_env(ctx), sexp_car(x));
      if (! cell && sexp_synclop(sexp_car(x)))
        cell = sexp_env_cell(sexp_synclo_env(sexp_car(x)),
                             sexp_synclo_expr(sexp_car(x)));
      if (! cell) {
        res = analyze_app(ctx, x);
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
                                  sexp_strip_synclos(ctx, sexp_cadr(x)) :
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
    } else if (sexp_truep(sexp_listp(ctx, sexp_car(x)))
               || (sexp_synclop(sexp_car(x))
                   && sexp_truep(sexp_listp(ctx,
                                            sexp_synclo_expr(sexp_car(x)))))) {
      res = analyze_app(ctx, x);
    } else {
      res = sexp_compile_error(ctx, "invalid operand in application", x);
    }
  } else if (sexp_idp(x)) {
    res = analyze_var_ref(ctx, x, NULL);
  } else if (sexp_synclop(x)) {
    tmp = sexp_make_child_context(ctx, sexp_context_lambda(ctx));
    sexp_context_env(tmp) = sexp_synclo_env(x);
    sexp_context_fv(tmp) = sexp_append2(tmp,
                                        sexp_synclo_free_vars(x),
                                        sexp_context_fv(tmp));
    x = sexp_synclo_expr(x);
    res = analyze(tmp, x);
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

static sexp_sint_t sexp_context_make_label (sexp ctx) {
  sexp_sint_t label;
  sexp_context_align_pos(ctx);
  label = sexp_context_pos(ctx);
  sexp_context_pos(ctx) += sizeof(sexp_uint_t);
  return label;
}

static void sexp_context_patch_label (sexp ctx, sexp_sint_t label) {
  sexp bc = sexp_context_bc(ctx);
  unsigned char *data = sexp_bytecode_data(bc)+label;
  *((sexp_sint_t*)data) = sexp_context_pos(ctx)-label;
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
      emit(ctx, SEXP_OP_DROP);
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
  emit(ctx, SEXP_OP_JUMP_UNLESS);
  sexp_context_depth(ctx)--;
  label1 = sexp_context_make_label(ctx);
  generate(ctx, sexp_cnd_pass(cnd));
  sexp_context_tailp(ctx) = tailp;
  emit(ctx, SEXP_OP_JUMP);
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
    emit(ctx, SEXP_OP_LOCAL_REF);
    emit_word(ctx, sexp_param_index(lambda, name));
  } else {
    /* closure ref */
    for (i=0; sexp_pairp(fv); fv=sexp_cdr(fv), i++)
      if ((name == sexp_ref_name(sexp_car(fv)))
          && (loc == sexp_ref_loc(sexp_car(fv))))
        break;
    emit(ctx, SEXP_OP_CLOSURE_REF);
    emit_word(ctx, i);
  }
  if (unboxp && (sexp_memq(ctx, name, sexp_lambda_sv(loc)) != SEXP_FALSE))
    emit(ctx, SEXP_OP_CDR);
  sexp_context_depth(ctx)++;
}

static void generate_ref (sexp ctx, sexp ref, int unboxp) {
  sexp lam;
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global ref */
    if (unboxp) {
      emit(ctx,
           (sexp_cdr(sexp_ref_cell(ref)) == SEXP_UNDEF)
           ? SEXP_OP_GLOBAL_REF : SEXP_OP_GLOBAL_KNOWN_REF);
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
    emit(ctx, SEXP_OP_SET_CDR);
  } else {
    lambda = sexp_ref_loc(ref);
    if (sexp_truep(sexp_memq(ctx, sexp_ref_name(ref), sexp_lambda_sv(lambda)))) {
      /* stack or closure mutable vars are boxed */
      generate_ref(ctx, ref, 0);
      emit(ctx, SEXP_OP_SET_CDR);
    } else {
      /* internally defined variable */
      emit(ctx, SEXP_OP_LOCAL_SET);
      emit_word(ctx, sexp_param_index(lambda, sexp_ref_name(ref)));
    }
  }
  sexp_context_depth(ctx)--;
}

static void generate_opcode_app (sexp ctx, sexp app) {
  sexp op = sexp_car(app);
  sexp_sint_t i, num_args, inv_default=0;
  sexp_gc_var1(ls);
  sexp_gc_preserve1(ctx, ls);

  num_args = sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(app)));
  sexp_context_tailp(ctx) = 0;

  /* maybe push the default for an optional argument */
  if ((num_args == sexp_opcode_num_args(op))
      && sexp_opcode_variadic_p(op)
      && sexp_opcode_data(op)
      && (sexp_opcode_class(op) != SEXP_OPC_PARAMETER)) {
    if (sexp_opcode_inverse(op)) {
      inv_default = 1;
    } else {
      emit_push(ctx, sexp_opcode_data(op));
      if (sexp_opcode_opt_param_p(op)) emit(ctx, SEXP_OP_CDR);
      sexp_context_depth(ctx)++;
      num_args++;
    }
  }

  /* push the arguments onto the stack in reverse order */
  ls = ((sexp_opcode_inverse(op)
         && (sexp_opcode_class(op) != SEXP_OPC_ARITHMETIC))
        ? sexp_cdr(app) : sexp_reverse(ctx, sexp_cdr(app)));
  for ( ; sexp_pairp(ls); ls = sexp_cdr(ls))
    generate(ctx, sexp_car(ls));

  /* push the default for inverse opcodes */
  if (inv_default) {
    emit_push(ctx, sexp_opcode_data(op));
    if (sexp_opcode_opt_param_p(op)) emit(ctx, SEXP_OP_CDR);
    sexp_context_depth(ctx)++;
    num_args++;
  }

  /* emit the actual operator call */
  switch (sexp_opcode_class(op)) {
  case SEXP_OPC_ARITHMETIC:
    /* fold variadic arithmetic operators */
    for (i=num_args-1; i>0; i--)
      emit(ctx, sexp_opcode_code(op));
    break;
  case SEXP_OPC_ARITHMETIC_CMP:
    if (num_args > 2) {
      emit(ctx, SEXP_OP_STACK_REF);
      emit_word(ctx, 2);
      emit(ctx, SEXP_OP_STACK_REF);
      emit_word(ctx, 2);
      emit(ctx, sexp_opcode_code(op));
      emit(ctx, SEXP_OP_AND);
      for (i=num_args-2; i>0; i--) {
        emit(ctx, SEXP_OP_STACK_REF);
        emit_word(ctx, 3);
        emit(ctx, SEXP_OP_STACK_REF);
        emit_word(ctx, 3);
        emit(ctx, sexp_opcode_code(op));
        emit(ctx, SEXP_OP_AND);
        emit(ctx, SEXP_OP_AND);
      }
    } else
      emit(ctx, sexp_opcode_code(op));
    break;
  case SEXP_OPC_FOREIGN:
    emit(ctx, sexp_opcode_code(op));
    emit_word(ctx, (sexp_uint_t)op);
    break;
  case SEXP_OPC_TYPE_PREDICATE:
  case SEXP_OPC_GETTER:
  case SEXP_OPC_SETTER:
  case SEXP_OPC_CONSTRUCTOR:
    emit(ctx, sexp_opcode_code(op));
    if ((sexp_opcode_class(op) != SEXP_OPC_CONSTRUCTOR)
        || sexp_opcode_code(op) == SEXP_OP_MAKE) {
      if (sexp_opcode_data(op))
        emit_word(ctx, sexp_unbox_fixnum(sexp_opcode_data(op)));
      if (sexp_opcode_data2(op))
        emit_word(ctx, sexp_unbox_fixnum(sexp_opcode_data2(op)));
    }
    break;
  case SEXP_OPC_PARAMETER:
    emit_push(ctx, sexp_opcode_data(op));
    emit(ctx, ((num_args == 0) ? SEXP_OP_CDR : SEXP_OP_SET_CDR));
    break;
  default:
    emit(ctx, sexp_opcode_code(op));
  }

  sexp_context_depth(ctx) -= (num_args-1);
  sexp_gc_release1(ctx);
}

static void generate_general_app (sexp ctx, sexp app) {
  sexp_uint_t len = sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(app))),
    tailp = sexp_context_tailp(ctx);
  sexp_gc_var1(ls);
  sexp_gc_preserve1(ctx, ls);

  /* push the arguments onto the stack */
  sexp_context_tailp(ctx) = 0;
  for (ls=sexp_reverse(ctx, sexp_cdr(app)); sexp_pairp(ls); ls=sexp_cdr(ls))
    generate(ctx, sexp_car(ls));

  /* push the operator onto the stack */
  generate(ctx, sexp_car(app));

  /* maybe overwrite the current frame */
  emit(ctx, (tailp ? SEXP_OP_TAIL_CALL : SEXP_OP_CALL));
  emit_word(ctx, (sexp_uint_t)sexp_make_fixnum(len));

  sexp_context_tailp(ctx) = tailp;
  sexp_context_depth(ctx) -= len;
  sexp_gc_release1(ctx);
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
  sexp_gc_var2(tmp, bc);
  sexp_gc_preserve2(ctx, tmp, bc);
  prev_lambda = sexp_context_lambda(ctx);
  prev_fv = sexp_lambdap(prev_lambda) ? sexp_lambda_fv(prev_lambda) : SEXP_NULL;
  fv = sexp_lambda_fv(lambda);
  ctx2 = sexp_make_eval_context(ctx, sexp_context_stack(ctx), sexp_context_env(ctx), 0);
  sexp_context_lambda(ctx2) = lambda;
  /* allocate space for local vars */
  for (ls=sexp_lambda_locals(lambda); sexp_pairp(ls); ls=sexp_cdr(ls))
    emit_push(ctx2, SEXP_VOID);
  /* box mutable vars */
  for (ls=sexp_lambda_sv(lambda); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    k = sexp_param_index(lambda, sexp_car(ls));
    if (k >= 0) {
      emit(ctx2, SEXP_OP_LOCAL_REF);
      emit_word(ctx2, k);
      emit_push(ctx2, sexp_car(ls));
      emit(ctx2, SEXP_OP_CONS);
      emit(ctx2, SEXP_OP_LOCAL_SET);
      emit_word(ctx2, k);
      emit(ctx2, SEXP_OP_DROP);
    }
  }
  sexp_context_tailp(ctx2) = 1;
  generate(ctx2, sexp_lambda_body(lambda));
  flags = sexp_make_fixnum((sexp_listp(ctx2, sexp_lambda_params(lambda))
                             == SEXP_FALSE) ? 1uL : 0uL);
  len = sexp_length(ctx2, sexp_lambda_params(lambda));
  bc = finalize_bytecode(ctx2);
  sexp_bytecode_name(bc) = sexp_lambda_name(lambda);
  if (sexp_nullp(fv)) {
    /* shortcut, no free vars */
    tmp = sexp_make_vector(ctx2, SEXP_ZERO, SEXP_VOID);
    tmp = sexp_make_procedure(ctx2, flags, len, bc, tmp);
    sexp_push(ctx, sexp_bytecode_literals(sexp_context_bc(ctx)), tmp);
    generate_lit(ctx, tmp);
  } else {
    /* push the closed vars */
    emit_push(ctx, SEXP_VOID);
    emit_push(ctx, sexp_length(ctx, fv));
    emit(ctx, SEXP_OP_MAKE_VECTOR);
    sexp_context_depth(ctx)--;
    for (k=0; sexp_pairp(fv); fv=sexp_cdr(fv), k++) {
      ref = sexp_car(fv);
      generate_non_global_ref(ctx, sexp_ref_name(ref), sexp_ref_cell(ref),
                              prev_lambda, prev_fv, 0);
      emit_push(ctx, sexp_make_fixnum(k));
      emit(ctx, SEXP_OP_STACK_REF);
      emit_word(ctx, 3);
      emit(ctx, SEXP_OP_VECTOR_SET);
      emit(ctx, SEXP_OP_DROP);
      sexp_context_depth(ctx)--;
    }
    /* push the additional procedure info and make the closure */
    emit_push(ctx, bc);
    emit_push(ctx, len);
    emit_push(ctx, flags);
    emit(ctx, SEXP_OP_MAKE_PROCEDURE);
  }
  sexp_gc_release2(ctx);
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

static sexp make_param_list (sexp ctx, sexp_uint_t i) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_NULL;
  for ( ; i>0; i--)
    res = sexp_cons(ctx, sexp_make_fixnum(i), res);
  sexp_gc_release1(ctx);
  return res;
}

static sexp make_opcode_procedure (sexp ctx, sexp op, sexp_uint_t i) {
  sexp ls, bc, res, env;
  sexp_gc_var5(params, ref, refs, lambda, ctx2);
  if (i == sexp_opcode_num_args(op)) { /* return before preserving */
    if (sexp_opcode_proc(op)) return sexp_opcode_proc(op);
  } else if (i < sexp_opcode_num_args(op)) {
    return sexp_compile_error(ctx, "not enough args for opcode", op);
  } else if (! sexp_opcode_variadic_p(op)) { /* i > num_args */
    return sexp_compile_error(ctx, "too many args for opcode", op);
  }
  sexp_gc_preserve5(ctx, params, ref, refs, lambda, ctx2);
  params = make_param_list(ctx, i);
  lambda = sexp_make_lambda(ctx, params);
  ctx2 = sexp_make_child_context(ctx, lambda);
  env = sexp_extend_env(ctx2, sexp_context_env(ctx), params, lambda);
  sexp_context_env(ctx2) = env;
  for (ls=params, refs=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    ref = sexp_make_ref(ctx2, sexp_car(ls), sexp_env_cell(env, sexp_car(ls)));
    sexp_push(ctx2, refs, ref);
  }
  refs = sexp_reverse(ctx2, refs);
  refs = sexp_cons(ctx2, op, refs);
  generate_opcode_app(ctx2, refs);
  bc = finalize_bytecode(ctx2);
  sexp_bytecode_name(bc) = sexp_c_string(ctx2, sexp_opcode_name(op), -1);
  res=sexp_make_procedure(ctx2, SEXP_ZERO, sexp_make_fixnum(i), bc, SEXP_VOID);
  if (i == sexp_opcode_num_args(op))
    sexp_opcode_proc(op) = res;
  sexp_gc_release5(ctx);
  return res;
}

/*********************** the virtual machine **************************/

static sexp sexp_save_stack (sexp ctx, sexp *stack, sexp_uint_t to) {
  sexp res, *data;
  sexp_uint_t i;
  res = sexp_make_vector(ctx, sexp_make_fixnum(to), SEXP_VOID);
  data = sexp_vector_data(res);
  for (i=0; i<to; i++)
    data[i] = stack[i];
  return res;
}

static sexp_uint_t sexp_restore_stack (sexp saved, sexp *current) {
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

#if SEXP_USE_ALIGNED_BYTECODE
#define _ALIGN_IP() ip = (unsigned char *)sexp_word_align((unsigned long)ip)
#else
#define _ALIGN_IP()
#endif

#define _WORD0 ((sexp*)ip)[0]
#define _UWORD0 ((sexp_uint_t*)ip)[0]
#define _SWORD0 ((sexp_sint_t*)ip)[0]
#define _WORD1 ((sexp*)ip)[1]
#define _UWORD1 ((sexp_uint_t*)ip)[1]
#define _SWORD1 ((sexp_sint_t*)ip)[1]

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
  sexp *stack = sexp_stack_data(sexp_context_stack(ctx));
  unsigned char *ip = sexp_bytecode_data(bc);
  sexp_sint_t i, j, k, fp, top = sexp_stack_top(sexp_context_stack(ctx));
#if SEXP_USE_BIGNUMS
  sexp_lsint_t prod;
#endif
  sexp_gc_var3(self, tmp1, tmp2);
  sexp_gc_preserve3(ctx, self, tmp1, tmp2);
  fp = top - 4;
  self = proc;

 loop:
#if SEXP_USE_DEBUG_VM
  if (sexp_context_tracep(ctx)) {
    sexp_print_stack(ctx, stack, top, fp, SEXP_FALSE);
    fprintf(stderr, "%s\n", (*ip<=SEXP_OP_NUM_OPCODES) ?
            reverse_opcode_names[*ip] : "UNKNOWN");
  }
#endif
  switch (*ip++) {
  case SEXP_OP_NOOP:
    break;
  case SEXP_OP_RAISE:
  call_error_handler:
    tmp1 = sexp_cdr(sexp_global(ctx, SEXP_G_ERR_HANDLER));
    if (! sexp_procedurep(tmp1)) goto end_loop;
    stack[top] = (sexp) 1;
    stack[top+1] = sexp_make_fixnum(ip-sexp_bytecode_data(bc));
    stack[top+2] = self;
    stack[top+3] = sexp_make_fixnum(fp);
    top += 4;
    self = tmp1;
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(self);
    fp = top-4;
    break;
  case SEXP_OP_RESUMECC:
    tmp1 = stack[fp-1];
    top = sexp_restore_stack(sexp_vector_ref(cp, 0), stack);
    fp = sexp_unbox_fixnum(_ARG1);
    self = _ARG2;
    bc = sexp_procedure_code(self);
    cp = sexp_procedure_vars(self);
    ip = sexp_bytecode_data(bc) + sexp_unbox_fixnum(_ARG3);
    i = sexp_unbox_fixnum(_ARG4);
    top -= 4;
    _ARG1 = tmp1;
    break;
  case SEXP_OP_CALLCC:
    stack[top] = SEXP_ONE;
    stack[top+1] = sexp_make_fixnum(ip-sexp_bytecode_data(bc));
    stack[top+2] = self;
    stack[top+3] = sexp_make_fixnum(fp);
    tmp1 = _ARG1;
    i = 1;
    sexp_context_top(ctx) = top;
    tmp2 = sexp_make_vector(ctx, SEXP_ONE, SEXP_UNDEF);
    sexp_vector_set(tmp2, SEXP_ZERO, sexp_save_stack(ctx, stack, top+4));
    _ARG1 = sexp_make_procedure(ctx,
                                SEXP_ZERO,
                                SEXP_ONE,
                                sexp_global(ctx, SEXP_G_RESUMECC_BYTECODE),
                                tmp2);
    top++;
    ip -= sizeof(sexp);
    goto make_call;
  case SEXP_OP_APPLY1:
    tmp1 = _ARG1;
    tmp2 = _ARG2;
    i = sexp_unbox_fixnum(sexp_length(ctx, tmp2));
    top += (i-2);
    for ( ; sexp_pairp(tmp2); tmp2=sexp_cdr(tmp2), top--)
      _ARG1 = sexp_car(tmp2);
    top += i+1;
    ip -= sizeof(sexp);
    goto make_call;
  case SEXP_OP_TAIL_CALL:
    _ALIGN_IP();
    i = sexp_unbox_fixnum(_WORD0);    /* number of params */
    tmp1 = _ARG1;                              /* procedure to call */
    /* save frame info */
    tmp2 = stack[fp+3];
    j = sexp_unbox_fixnum(stack[fp]);
    self = stack[fp+2];
    bc = sexp_procedure_vars(self);
    cp = sexp_procedure_vars(self);
    ip = (sexp_bytecode_data(bc)
          + sexp_unbox_fixnum(stack[fp+1])) - sizeof(sexp);
    /* copy new args into place */
    for (k=0; k<i; k++)
      stack[fp-j+k] = stack[top-1-i+k];
    top = fp+i-j+1;
    fp = sexp_unbox_fixnum(tmp2);
    goto make_call;
  case SEXP_OP_CALL:
#if SEXP_USE_CHECK_STACK
    if (top+16 >= SEXP_INIT_STACK_SIZE) {
      _ARG1 = sexp_global(ctx, SEXP_G_OOS_ERROR);
      goto end_loop;
    }
#endif
    _ALIGN_IP();
    i = sexp_unbox_fixnum(_WORD0);
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
    j = i - sexp_unbox_fixnum(sexp_procedure_num_args(tmp1));
    if (j < 0)
      sexp_raise("not enough args",
                 sexp_list2(ctx, tmp1, sexp_make_fixnum(i)));
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
                   sexp_list2(ctx, tmp1, sexp_make_fixnum(i)));
      }
    } else if (sexp_procedure_variadic_p(tmp1)) {
      /* shift stack, set extra arg to null */
      for (k=top; k>=top-i; k--)
        stack[k] = stack[k-1];
      stack[top-i-1] = SEXP_NULL;
      top++;
      i++;
    }
    _ARG1 = sexp_make_fixnum(i);
    stack[top] = sexp_make_fixnum(ip+sizeof(sexp)-sexp_bytecode_data(bc));
    stack[top+1] = self;
    stack[top+2] = sexp_make_fixnum(fp);
    top += 3;
    self = tmp1;
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(self);
    fp = top-4;
    break;
  case SEXP_OP_FCALL0:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _PUSH(((sexp_proc1)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 0)));
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL1:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG1 = ((sexp_proc2)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 1), _ARG1);
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL2:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG2 = ((sexp_proc3)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 2), _ARG1, _ARG2);
    top--;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL3:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG3 = ((sexp_proc4)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 3), _ARG1, _ARG2, _ARG3);
    top -= 2;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL4:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG4 = ((sexp_proc5)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 4), _ARG1, _ARG2, _ARG3, _ARG4);
    top -= 3;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL5:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG5 = ((sexp_proc6)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 5), _ARG1, _ARG2, _ARG3, _ARG4, _ARG5);
    top -= 4;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL6:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG6 = ((sexp_proc7)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 6), _ARG1, _ARG2, _ARG3, _ARG4, _ARG5, _ARG6);
    top -= 5;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_JUMP_UNLESS:
    _ALIGN_IP();
    if (stack[--top] == SEXP_FALSE)
      ip += _SWORD0;
    else
      ip += sizeof(sexp_sint_t);
    break;
  case SEXP_OP_JUMP:
    _ALIGN_IP();
    ip += _SWORD0;
    break;
  case SEXP_OP_PUSH:
    _ALIGN_IP();
    _PUSH(_WORD0);
    ip += sizeof(sexp);
    break;
  case SEXP_OP_DROP:
    top--;
    break;
  case SEXP_OP_GLOBAL_REF:
    _ALIGN_IP();
    if (sexp_cdr(_WORD0) == SEXP_UNDEF)
      sexp_raise("undefined variable", sexp_list1(ctx, sexp_car(_WORD0)));
    /* ... FALLTHROUGH ... */
  case SEXP_OP_GLOBAL_KNOWN_REF:
    _ALIGN_IP();
    _PUSH(sexp_cdr(_WORD0));
    ip += sizeof(sexp);
    break;
  case SEXP_OP_STACK_REF:            /* `pick' in forth */
    _ALIGN_IP();
    stack[top] = stack[top - _SWORD0];
    ip += sizeof(sexp);
    top++;
    break;
  case SEXP_OP_LOCAL_REF:
    _ALIGN_IP();
    stack[top] = stack[fp - 1 - _SWORD0];
    ip += sizeof(sexp);
    top++;
    break;
  case SEXP_OP_LOCAL_SET:
    _ALIGN_IP();
    stack[fp - 1 - _SWORD0] = _ARG1;
    _ARG1 = SEXP_VOID;
    ip += sizeof(sexp);
    break;
  case SEXP_OP_CLOSURE_REF:
    _ALIGN_IP();
    _PUSH(sexp_vector_ref(cp, sexp_make_fixnum(_WORD0)));
    ip += sizeof(sexp);
    break;
  case SEXP_OP_VECTOR_REF:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-ref: not a vector", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("vector-ref: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_vector_length(_ARG1)))
      sexp_raise("vector-ref: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_vector_ref(_ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_VECTOR_SET:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-set!: not a vector", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("vector-set!: immutable vector", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("vector-set!: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_vector_length(_ARG1)))
      sexp_raise("vector-set!: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    sexp_vector_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_VOID;
    top-=2;
    break;
  case SEXP_OP_VECTOR_LENGTH:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-length: not a vector", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_vector_length(_ARG1));
    break;
  case SEXP_OP_STRING_REF:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-ref: not a string", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("string-ref: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_string_length(_ARG1)))
      sexp_raise("string-ref: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_string_ref(_ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_STRING_SET:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-set!: not a string", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("string-set!: immutable string", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("string-set!: not an integer", sexp_list1(ctx, _ARG2));
    else if (! sexp_charp(_ARG3))
      sexp_raise("string-set!: not a char", sexp_list1(ctx, _ARG3));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_string_length(_ARG1)))
      sexp_raise("string-set!: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    sexp_string_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_VOID;
    top-=2;
    break;
  case SEXP_OP_STRING_LENGTH:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-length: not a string", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_string_length(_ARG1));
    break;
  case SEXP_OP_MAKE_PROCEDURE:
    sexp_context_top(ctx) = top;
    _ARG4 = sexp_make_procedure(ctx, _ARG1, _ARG2, _ARG3, _ARG4);
    top-=3;
    break;
  case SEXP_OP_MAKE_VECTOR:
    sexp_context_top(ctx) = top;
    if (! sexp_fixnump(_ARG1))
      sexp_raise("make-vector: not an integer", sexp_list1(ctx, _ARG1));
    _ARG2 = sexp_make_vector(ctx, _ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_MAKE_EXCEPTION:
    _ARG5 = sexp_make_exception(ctx, _ARG1, _ARG2, _ARG3, _ARG4, _ARG5);
    top -= 4;
    break;
  case SEXP_OP_AND:
    _ARG2 = sexp_make_boolean((_ARG1 != SEXP_FALSE) && (_ARG2 != SEXP_FALSE));
    top--;
    break;
  case SEXP_OP_EOFP:
    _ARG1 = sexp_make_boolean(_ARG1 == SEXP_EOF); break;
  case SEXP_OP_NULLP:
    _ARG1 = sexp_make_boolean(sexp_nullp(_ARG1)); break;
  case SEXP_OP_FIXNUMP:
    _ARG1 = sexp_make_boolean(sexp_fixnump(_ARG1)); break;
  case SEXP_OP_SYMBOLP:
    _ARG1 = sexp_make_boolean(sexp_symbolp(_ARG1)); break;
  case SEXP_OP_CHARP:
    _ARG1 = sexp_make_boolean(sexp_charp(_ARG1)); break;
  case SEXP_OP_TYPEP:
    _ALIGN_IP();
    _ARG1 = sexp_make_boolean(sexp_check_tag(_ARG1, _UWORD0));
    ip += sizeof(sexp);
    break;
  case SEXP_OP_MAKE:
    _ALIGN_IP();
    _PUSH(sexp_alloc_tagged(ctx, _UWORD1, _UWORD0));
    ip += sizeof(sexp)*2;
    break;
  case SEXP_OP_SLOT_REF:
    _ALIGN_IP();
    if (! sexp_check_tag(_ARG1, _UWORD0))
      sexp_raise("slot-ref: bad type", sexp_list2(ctx, sexp_c_string(ctx, sexp_type_name_by_index(ctx, _UWORD0), -1), _ARG1));
    _ARG1 = sexp_slot_ref(_ARG1, _UWORD1);
    ip += sizeof(sexp)*2;
    break;
  case SEXP_OP_SLOT_SET:
    _ALIGN_IP();
    if (! sexp_check_tag(_ARG1, _UWORD0))
      sexp_raise("slot-set!: bad type", sexp_list2(ctx, sexp_c_string(ctx, sexp_type_name_by_index(ctx, _UWORD0), -1), _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("slot-set!: immutable object", sexp_list1(ctx, _ARG1));
    sexp_slot_set(_ARG1, _UWORD1, _ARG2);
    _ARG2 = SEXP_VOID;
    ip += sizeof(sexp)*2;
    top--;
    break;
  case SEXP_OP_CAR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("car: not a pair", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_car(_ARG1); break;
  case SEXP_OP_CDR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("cdr: not a pair", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_cdr(_ARG1); break;
  case SEXP_OP_SET_CAR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-car!: not a pair", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("set-car!: immutable pair", sexp_list1(ctx, _ARG1));
    sexp_car(_ARG1) = _ARG2;
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case SEXP_OP_SET_CDR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-cdr!: not a pair", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("set-cdr!: immutable pair", sexp_list1(ctx, _ARG1));
    sexp_cdr(_ARG1) = _ARG2;
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case SEXP_OP_CONS:
    sexp_context_top(ctx) = top;
    _ARG2 = sexp_cons(ctx, _ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_ADD:
#if SEXP_USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      j = sexp_unbox_fixnum(tmp1) + sexp_unbox_fixnum(tmp2);
      if ((j < SEXP_MIN_FIXNUM) || (j > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_add(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_fixnum(j);
    }
    else
      _ARG2 = sexp_add(ctx, tmp1, tmp2);
#else
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_fx_add(_ARG1, _ARG2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_add(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) + (double)sexp_unbox_fixnum(_ARG2));
    else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(_ARG1) + sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("+: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case SEXP_OP_SUB:
#if SEXP_USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      j = sexp_unbox_fixnum(tmp1) - sexp_unbox_fixnum(tmp2);
      if ((j < SEXP_MIN_FIXNUM) || (j > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_sub(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_fixnum(j);
    }
    else
      _ARG2 = sexp_sub(ctx, tmp1, tmp2);
#else
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_fx_sub(_ARG1, _ARG2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_sub(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) - (double)sexp_unbox_fixnum(_ARG2));
    else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(_ARG1) - sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("-: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case SEXP_OP_MUL:
#if SEXP_USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      prod = (sexp_lsint_t)sexp_unbox_fixnum(tmp1) * sexp_unbox_fixnum(tmp2);
      if ((prod < SEXP_MIN_FIXNUM) || (prod > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_mul(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_fixnum(prod);
    }
    else
      _ARG2 = sexp_mul(ctx, tmp1, tmp2);
#else
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_fx_mul(_ARG1, _ARG2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_mul(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) * (double)sexp_unbox_fixnum(_ARG2));
    else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(_ARG1) * sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("*: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case SEXP_OP_DIV:
    sexp_context_top(ctx) = top;
    if (_ARG2 == SEXP_ZERO) {
#if SEXP_USE_FLONUMS
      if (sexp_flonump(_ARG1) && sexp_flonum_value(_ARG1) == 0.0)
        _ARG2 = sexp_make_flonum(ctx, 0.0/0.0);
      else
#endif
        sexp_raise("divide by zero", SEXP_NULL);
    } else if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
#if SEXP_USE_FLONUMS
      _ARG1 = sexp_fixnum_to_flonum(ctx, _ARG1);
      _ARG2 = sexp_fixnum_to_flonum(ctx, _ARG2);
      _ARG2 = sexp_fp_div(ctx, _ARG1, _ARG2);
      if (sexp_flonum_value(_ARG2) == trunc(sexp_flonum_value(_ARG2)))
        _ARG2 = sexp_make_fixnum(sexp_flonum_value(_ARG2));
#else
      _ARG2 = sexp_fx_div(_ARG1, _ARG2);
#endif
    }
#if SEXP_USE_BIGNUMS
    else
      _ARG2 = sexp_div(ctx, _ARG1, _ARG2);
#else
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_div(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) / (double)sexp_unbox_fixnum(_ARG2));
    else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(_ARG1) / sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("/: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case SEXP_OP_QUOTIENT:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      if (_ARG2 == SEXP_ZERO)
        sexp_raise("divide by zero", SEXP_NULL);
      _ARG2 = sexp_fx_div(_ARG1, _ARG2);
      top--;
    }
#if SEXP_USE_BIGNUMS
    else {
      sexp_context_top(ctx) = top;
      _ARG2 = sexp_quotient(ctx, _ARG1, _ARG2);
      top--;
    }
#else
    else sexp_raise("quotient: not an integer", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    break;
  case SEXP_OP_REMAINDER:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      if (_ARG2 == SEXP_ZERO)
        sexp_raise("divide by zero", SEXP_NULL);
      tmp1 = sexp_fx_rem(_ARG1, _ARG2);
      top--;
      _ARG1 = tmp1;
    }
#if SEXP_USE_BIGNUMS
    else {
      sexp_context_top(ctx) = top;
      _ARG2 = sexp_remainder(ctx, _ARG1, _ARG2);
      top--;
    }
#else
    else sexp_raise("remainder: not an integer", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    break;
  case SEXP_OP_LT:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = (sexp_sint_t)_ARG1 < (sexp_sint_t)_ARG2;
#if SEXP_USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_fixnump(tmp1)
        ? sexp_make_boolean(sexp_unbox_fixnum(tmp1) < 0) : tmp1;
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) < sexp_flonum_value(_ARG2);
    } else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) < (double)sexp_unbox_fixnum(_ARG2); 
    } else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2)) {
      i = (double)sexp_unbox_fixnum(_ARG1) < sexp_flonum_value(_ARG2);
#endif
    } else sexp_raise("<: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case SEXP_OP_LE:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = (sexp_sint_t)_ARG1 <= (sexp_sint_t)_ARG2;
#if SEXP_USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_fixnump(tmp1)
        ? sexp_make_boolean(sexp_unbox_fixnum(tmp1) <= 0) : tmp1;
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) <= sexp_flonum_value(_ARG2);
    } else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) <= (double)sexp_unbox_fixnum(_ARG2);
    } else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2)) {
      i = (double)sexp_unbox_fixnum(_ARG1) <= sexp_flonum_value(_ARG2);
#endif
    } else sexp_raise("<=: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case SEXP_OP_EQN:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = _ARG1 == _ARG2;
#if SEXP_USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_fixnump(tmp1)
        ? sexp_make_boolean(sexp_unbox_fixnum(tmp1) == 0) : tmp1;
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) == sexp_flonum_value(_ARG2);
    } else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) == (double)sexp_unbox_fixnum(_ARG2);
    } else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2)) {
      i = (double)sexp_unbox_fixnum(_ARG1) == sexp_flonum_value(_ARG2);
#endif
    } else sexp_raise("=: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case SEXP_OP_EQ:
    _ARG2 = sexp_make_boolean(_ARG1 == _ARG2);
    top--;
    break;
  case SEXP_OP_FIX2FLO:
    if (sexp_fixnump(_ARG1))
      _ARG1 = sexp_fixnum_to_flonum(ctx, _ARG1);
#if SEXP_USE_BIGNUMS
    else if (sexp_bignump(_ARG1))
      _ARG1 = sexp_make_flonum(ctx, sexp_bignum_to_double(_ARG1));
#endif
    else if (! sexp_flonump(_ARG1))
      sexp_raise("exact->inexact: not a number", sexp_list1(ctx, _ARG1));
    break;
  case SEXP_OP_FLO2FIX:
    if (sexp_flonump(_ARG1)) {
      if (sexp_flonum_value(_ARG1) != trunc(sexp_flonum_value(_ARG1))) {
        sexp_raise("inexact->exact: not an integer", sexp_list1(ctx, _ARG1));
#if SEXP_USE_BIGNUMS
      } else if ((sexp_flonum_value(_ARG1) > SEXP_MAX_FIXNUM)
                 || sexp_flonum_value(_ARG1) < SEXP_MIN_FIXNUM) {
        _ARG1 = sexp_double_to_bignum(ctx, sexp_flonum_value(_ARG1));
#endif
      } else {
        _ARG1 = sexp_make_fixnum((sexp_sint_t)sexp_flonum_value(_ARG1));
      }
    } else if (! sexp_fixnump(_ARG1) && ! sexp_bignump(_ARG1)) {
      sexp_raise("inexact->exact: not a number", sexp_list1(ctx, _ARG1));
    }
    break;
  case SEXP_OP_CHAR2INT:
    if (! sexp_charp(_ARG1))
      sexp_raise("char->integer: not a character", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_unbox_character(_ARG1));
    break;
  case SEXP_OP_INT2CHAR:
    if (! sexp_fixnump(_ARG1))
      sexp_raise("integer->char: not an integer", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_character(sexp_unbox_fixnum(_ARG1));
    break;
  case SEXP_OP_CHAR_UPCASE:
    if (! sexp_charp(_ARG1))
      sexp_raise("char-upcase: not a character", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_character(toupper(sexp_unbox_character(_ARG1)));
    break;
  case SEXP_OP_CHAR_DOWNCASE:
    if (! sexp_charp(_ARG1))
      sexp_raise("char-downcase: not a character", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_character(tolower(sexp_unbox_character(_ARG1)));
    break;
  case SEXP_OP_WRITE_CHAR:
    if (! sexp_charp(_ARG1))
      sexp_raise("write-char: not a character", sexp_list1(ctx, _ARG1));
    if (! sexp_oportp(_ARG2))
      sexp_raise("write-char: not an output-port", sexp_list1(ctx, _ARG2));
    sexp_write_char(ctx, sexp_unbox_character(_ARG1), _ARG2);
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case SEXP_OP_NEWLINE:
    if (! sexp_oportp(_ARG1))
      sexp_raise("newline: not an output-port", sexp_list1(ctx, _ARG1));
    sexp_newline(ctx, _ARG1);
    _ARG1 = SEXP_VOID;
    break;
  case SEXP_OP_READ_CHAR:
    if (! sexp_iportp(_ARG1))
      sexp_raise("read-char: not an intput-port", sexp_list1(ctx, _ARG1));
    i = sexp_read_char(ctx, _ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case SEXP_OP_PEEK_CHAR:
    if (! sexp_iportp(_ARG1))
      sexp_raise("peek-char: not an intput-port", sexp_list1(ctx, _ARG1));
    i = sexp_read_char(ctx, _ARG1);
    sexp_push_char(ctx, i, _ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case SEXP_OP_RET:
    i = sexp_unbox_fixnum(stack[fp]);
    stack[fp-i] = _ARG1;
    top = fp-i+1;
    self = stack[fp+2];
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc) + sexp_unbox_fixnum(stack[fp+1]);
    cp = sexp_procedure_vars(self);
    fp = sexp_unbox_fixnum(stack[fp+3]);
    break;
  case SEXP_OP_DONE:
    goto end_loop;
  default:
    sexp_raise("unknown opcode", sexp_list1(ctx, sexp_make_fixnum(*(ip-1))));
  }
  goto loop;

 end_loop:
  sexp_gc_release3(ctx);
  sexp_context_top(ctx) = top;
  return _ARG1;
}

/************************ library procedures **************************/

static sexp sexp_exception_type_op (sexp ctx sexp_api_params(self, n), sexp exn) {
  if (sexp_exceptionp(exn))
    return sexp_exception_kind(exn);
  else
    return sexp_type_exception(ctx, "not an exception", exn);
}

static sexp sexp_open_input_file_op (sexp ctx sexp_api_params(self, n), sexp path) {
  FILE *in;
  if (! sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);
  in = fopen(sexp_string_data(path), "r");
  if (! in)
    return
      sexp_user_exception(ctx, SEXP_FALSE, "couldn't open input file", path);
  return sexp_make_input_port(ctx, in, path);
}

static sexp sexp_open_output_file_op (sexp ctx sexp_api_params(self, n), sexp path) {
  FILE *out;
  if (! sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);
  out = fopen(sexp_string_data(path), "w");
  if (! out)
    return
      sexp_user_exception(ctx, SEXP_FALSE, "couldn't open output file", path);
  return sexp_make_output_port(ctx, out, path);
}

static sexp sexp_close_port_op (sexp ctx sexp_api_params(self, n), sexp port) {
  if (! sexp_portp(port))
    return sexp_type_exception(ctx, "not a port", port);
  if (! sexp_port_openp(port))
    return sexp_user_exception(ctx, SEXP_FALSE, "port already closed", port);
  return sexp_finalize_port(ctx sexp_api_pass(self, n), port);
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
  if (! sexp_stringp(source))
    return sexp_type_exception(ctx, "not a string", source);
  if (! sexp_envp(env))
    return sexp_type_exception(ctx, "not an environment", env);
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
  ctx2 = sexp_make_eval_context(ctx, NULL, env, 0);
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
  sexp_gc_release4(ctx);
#if SEXP_USE_DL
  }
#endif
#if SEXP_USE_WARN_UNDEFS
  if (sexp_oportp(out) && ! sexp_exceptionp(res))
    sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp, out);
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

#define define_math_op(name, cname)       \
  static sexp name (sexp ctx, sexp z) {   \
    double d;                             \
    if (sexp_flonump(z))                  \
      d = sexp_flonum_value(z);           \
    else if (sexp_fixnump(z))            \
      d = (double)sexp_unbox_fixnum(z);  \
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
define_math_op(sexp_round, round)
define_math_op(sexp_trunc, trunc)
define_math_op(sexp_floor, floor)
define_math_op(sexp_ceiling, ceil)

static sexp sexp_sqrt (sexp ctx, sexp z) {
  double d, r;
  if (sexp_flonump(z))
    d = sexp_flonum_value(z);
  else if (sexp_fixnump(z))
    d = (double)sexp_unbox_fixnum(z);
  maybe_convert_bignum(z)       /* XXXX add bignum sqrt */
  else
    return sexp_type_exception(ctx, "not a number", z);
  r = sqrt(d);
  if (sexp_fixnump(z) && ((r*r) == (double)sexp_unbox_fixnum(z)))
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
    return sexp_type_exception(ctx, "expt: not a number", x);
  if (sexp_fixnump(e))
    e1 = sexp_unbox_fixnum(e);
#if SEXP_USE_FLONUMS
  else if (sexp_flonump(e))
    e1 = sexp_flonum_value(e);
#endif
  else
    return sexp_type_exception(ctx, "expt: not a number", e);
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
  return sexp_make_fixnum(diff);
}

#ifdef PLAN9
#include "opt/plan9.c"
#endif

/************************** optimizations *****************************/

sexp sexp_apply_optimization (sexp ctx, sexp proc, sexp ast) {
  sexp res;
  sexp_gc_var1(args);
  if (sexp_opcodep(proc)) {
    res = ((sexp_proc2)sexp_opcode_func(proc))(ctx sexp_api_pass(proc, 1), ast);
  } else {
    sexp_gc_preserve1(ctx, args);
    res = sexp_apply(ctx, proc, args=sexp_list1(ctx, ast));
    sexp_gc_release1(ctx);
  }
  return res;
}

#if SEXP_USE_SIMPLIFY
#include "opt/simplify.c"
#endif

/***************************** opcodes ********************************/

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

sexp sexp_make_opcode (sexp ctx, sexp name, sexp op_class, sexp code,
                       sexp num_args, sexp flags, sexp arg1t, sexp arg2t,
                       sexp invp, sexp data, sexp data2, sexp_proc1 func) {
  sexp res;
  if (! sexp_stringp(name))
    res = sexp_type_exception(ctx, "make-opcode: not a string", name);
  else if ((! sexp_fixnump(op_class)) || (sexp_unbox_fixnum(op_class) <= 0)
      || (sexp_unbox_fixnum(op_class) >= SEXP_OPC_NUM_OP_CLASSES))
    res = sexp_type_exception(ctx, "make-opcode: bad opcode class", op_class);
  else if ((! sexp_fixnump(code)) || (sexp_unbox_fixnum(code) <= 0)
      || (sexp_unbox_fixnum(code) >= SEXP_OP_NUM_OPCODES))
    res = sexp_type_exception(ctx, "make-opcode: bad opcode", code);
  else if (! sexp_fixnump(num_args))
    res = sexp_type_exception(ctx, "make-opcode: bad num_args", num_args);
  else if (! sexp_fixnump(flags))
    res = sexp_type_exception(ctx, "make-opcode: bad flags", flags);
  else {
    res = sexp_alloc_type(ctx, opcode, SEXP_OPCODE);
    sexp_opcode_class(res) = sexp_unbox_fixnum(op_class);
    sexp_opcode_code(res) = sexp_unbox_fixnum(code);
    sexp_opcode_num_args(res) = sexp_unbox_fixnum(num_args);
    sexp_opcode_flags(res) = sexp_unbox_fixnum(flags);
    sexp_opcode_arg1_type(res) = sexp_unbox_fixnum(arg1t);
    sexp_opcode_arg2_type(res) = sexp_unbox_fixnum(arg2t);
    sexp_opcode_inverse(res) = sexp_unbox_fixnum(invp);
    sexp_opcode_data(res) = data;
    sexp_opcode_data2(res) = data2;
    sexp_opcode_func(res) = func;
    sexp_opcode_name(res) = strdup(sexp_string_data(name));
  }
  return res;
}

sexp sexp_make_foreign (sexp ctx, const char *name, int num_args,
                        int flags, sexp_proc1 f, sexp data) {
  sexp res;
  if (num_args > 6) {
    res = sexp_type_exception(ctx, "make-foreign: exceeded foreign arg limit",
                              sexp_make_fixnum(num_args));
  } else {
    res = sexp_alloc_type(ctx, opcode, SEXP_OPCODE);
    sexp_opcode_class(res) = SEXP_OPC_FOREIGN;
    sexp_opcode_code(res) = SEXP_OP_FCALL1+num_args-1;
    if (flags & 1) num_args--;
    sexp_opcode_num_args(res) = num_args;
    sexp_opcode_flags(res) = flags;
    sexp_opcode_name(res) = name;
    sexp_opcode_data(res) = data;
    sexp_opcode_func(res) = f;
  }
  return res;
}

sexp sexp_define_foreign_aux (sexp ctx, sexp env, const char *name, int num_args,
                              int flags, sexp_proc1 f, sexp data) {
  sexp_gc_var1(op);
  sexp_gc_preserve1(ctx, op);
  sexp res = SEXP_VOID;
  op = sexp_make_foreign(ctx, name, num_args, flags, f, data);
  if (sexp_exceptionp(op))
    res = op;
  else
    sexp_env_define(ctx, env, sexp_intern(ctx, name, -1), op);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_define_foreign_param (sexp ctx, sexp env, const char *name, int num_args,
                                sexp_proc1 f, const char *param) {
  sexp res;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  tmp = sexp_intern(ctx, param, -1);
  tmp = sexp_env_cell(env, tmp);
  res = sexp_define_foreign_aux(ctx, env, name, num_args, 3, f, tmp);
  sexp_gc_release1(ctx);
  return res;
}

#if SEXP_USE_TYPE_DEFS

sexp sexp_make_type_predicate_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type) {
  if (! sexp_fixnump(type))
    return sexp_type_exception(ctx, "make-type-predicate: bad type", type);
  return sexp_make_opcode(ctx, name, sexp_make_fixnum(SEXP_OPC_TYPE_PREDICATE),
                          sexp_make_fixnum(SEXP_OP_TYPEP), SEXP_ONE, SEXP_ZERO,
                          SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, type, NULL, NULL);
}

sexp sexp_make_constructor_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type) {
  sexp_uint_t type_size;
  if (! sexp_fixnump(type))
    return sexp_type_exception(ctx, "make-constructor: bad type", type);
  type_size = sexp_type_size_base(sexp_type_by_index(ctx, sexp_unbox_fixnum(type)));
  return sexp_make_opcode(ctx, name, sexp_make_fixnum(SEXP_OPC_CONSTRUCTOR),
                          sexp_make_fixnum(SEXP_OP_MAKE), SEXP_ZERO, SEXP_ZERO,
                          SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, type,
                          sexp_make_fixnum(type_size), NULL);
}

sexp sexp_make_getter_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type, sexp index) {
  if ((! sexp_fixnump(type))  || (sexp_unbox_fixnum(type) < 0))
    return sexp_type_exception(ctx, "make-getter: bad type", type);
  if ((! sexp_fixnump(index)) || (sexp_unbox_fixnum(index) < 0))
    return sexp_type_exception(ctx, "make-getter: bad index", index);
  return
    sexp_make_opcode(ctx, name, sexp_make_fixnum(SEXP_OPC_GETTER),
                     sexp_make_fixnum(SEXP_OP_SLOT_REF), SEXP_ONE, SEXP_ZERO,
                     type, SEXP_ZERO, SEXP_ZERO, type, index, NULL);
}

sexp sexp_make_setter_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type, sexp index) {
  if ((! sexp_fixnump(type))  || (sexp_unbox_fixnum(type) < 0))
    return sexp_type_exception(ctx, "make-setter: bad type", type);
  if ((! sexp_fixnump(index)) || (sexp_unbox_fixnum(index) < 0))
    return sexp_type_exception(ctx, "make-setter: bad index", index);
  return
    sexp_make_opcode(ctx, name, sexp_make_fixnum(SEXP_OPC_SETTER),
                     sexp_make_fixnum(SEXP_OP_SLOT_SET), SEXP_TWO, SEXP_ZERO,
                     type, SEXP_ZERO, SEXP_ZERO, type, index, NULL);
}

#endif

#if SEXP_USE_STATIC_LIBS
#include "clibs.c"
#endif

/*********************** standard environment *************************/

static struct sexp_struct core_forms[] = {
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_DEFINE, "define"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_SET, "set!"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_LAMBDA, "lambda"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_IF, "if"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_BEGIN, "begin"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_QUOTE, "quote"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_SYNTAX_QUOTE, "syntax-quote"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_DEFINE_SYNTAX, "define-syntax"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_LET_SYNTAX, "let-syntax"}}},
  {.tag=SEXP_CORE, .value={.core={SEXP_CORE_LETREC_SYNTAX, "letrec-syntax"}}},
};

sexp sexp_make_env_op (sexp ctx sexp_api_params(self, n)) {
  sexp e = sexp_alloc_type(ctx, env, SEXP_ENV);
  sexp_env_lambda(e) = NULL;
  sexp_env_parent(e) = NULL;
  sexp_env_bindings(e) = SEXP_NULL;
  return e;
}

sexp sexp_make_null_env_op (sexp ctx sexp_api_params(self, n), sexp version) {
  sexp_uint_t i;
  sexp e = sexp_make_env(ctx);
  for (i=0; i<(sizeof(core_forms)/sizeof(core_forms[0])); i++)
    sexp_env_define(ctx, e, sexp_intern(ctx, sexp_core_name(&core_forms[i]), -1),
                    sexp_copy_core(ctx, &core_forms[i]));
  return e;
}

sexp sexp_make_primitive_env (sexp ctx, sexp version) {
  int i;
  sexp_gc_var3(e, op, sym);
  sexp_gc_preserve3(ctx, e, op, sym);
  e = sexp_make_null_env(ctx, version);
  for (i=0; i<(sizeof(opcodes)/sizeof(opcodes[0])); i++) {
    op = sexp_copy_opcode(ctx, &opcodes[i]);
    if (sexp_opcode_opt_param_p(op) && sexp_opcode_data(op)) {
      sym = sexp_intern(ctx, (char*)sexp_opcode_data(op), -1);
      sexp_opcode_data(op) = sexp_env_cell_create(ctx, e, sym, SEXP_VOID, NULL);
    }
    sexp_env_define(ctx, e, sexp_intern(ctx, sexp_opcode_name(op), -1), op);
  }
  sexp_gc_release3(ctx);
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
static sexp sexp_find_module_file_op (sexp ctx sexp_api_params(self, n), sexp file) {
  if (! sexp_stringp(file))
    return sexp_type_exception(ctx, "not a string", file);
  else
    return sexp_find_module_file(ctx, sexp_string_data(file));
}
sexp sexp_load_module_file_op (sexp ctx sexp_api_params(self, n), sexp file, sexp env) {
  if (! sexp_stringp(file))
    return sexp_type_exception(ctx, "not a string", file);
  else if (! sexp_envp(env))
    return sexp_type_exception(ctx, "not an environment", env);
  return sexp_load_module_file(ctx, sexp_string_data(file), env);
}
#endif

sexp sexp_add_module_directory_op (sexp ctx sexp_api_params(self, n), sexp dir, sexp appendp) {
  sexp ls;
  if (! sexp_stringp(dir))
    return sexp_type_exception(ctx, "not a string", dir);
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

sexp sexp_load_standard_parameters (sexp ctx, sexp e) {
  /* add io port and interaction env parameters */
  sexp p = sexp_make_input_port(ctx, stdin, SEXP_FALSE);
  sexp_port_no_closep(p) = 1;
  sexp_env_define(ctx, e, sexp_global(ctx, SEXP_G_CUR_IN_SYMBOL), p);
  p = sexp_make_output_port(ctx, stdout, SEXP_FALSE);
  sexp_port_no_closep(p) = 1;
  sexp_env_define(ctx, e, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL), p);
  p = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
  sexp_port_no_closep(p) = 1;
  sexp_env_define(ctx, e, sexp_global(ctx, SEXP_G_CUR_ERR_SYMBOL), p);
  sexp_env_define(ctx, e, sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL), e);
  return SEXP_VOID;
}

sexp sexp_load_standard_env (sexp ctx, sexp e, sexp version) {
  sexp_gc_var3(op, tmp, sym);
  sexp_gc_preserve3(ctx, op, tmp, sym);
  sexp_load_standard_parameters(ctx, e);
#if SEXP_USE_DL
  sexp_env_define(ctx, e, sym=sexp_intern(ctx, "*shared-object-extension*", -1),
                  tmp=sexp_c_string(ctx, sexp_so_extension, -1));
#endif
  tmp = sexp_list1(ctx, sym=sexp_intern(ctx, sexp_platform, -1));
#if SEXP_USE_DL
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "dynamic-loading", -1));
#endif
#if SEXP_USE_MODULES
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "modules", -1));
#endif
#if SEXP_USE_BOEHM
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "boehm-gc", -1));
#endif
  sexp_push(ctx, tmp, sym=sexp_intern(ctx, "chibi", -1));
  sexp_env_define(ctx, e, sexp_intern(ctx, "*features*", -1), tmp);
  sexp_global(ctx, SEXP_G_OPTIMIZATIONS) = SEXP_NULL;
#if SEXP_USE_SIMPLIFY
  op = sexp_make_foreign(ctx, "simplify", 1, 0,
                         (sexp_proc1)sexp_simplify, SEXP_VOID);
  tmp = sexp_cons(ctx, sexp_make_fixnum(500), op);
  sexp_push(ctx, sexp_global(ctx, SEXP_G_OPTIMIZATIONS), tmp);
#endif
  /* load init.scm */
  tmp = sexp_load_module_file(ctx, sexp_init_file, e);
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

sexp sexp_env_copy_op (sexp ctx sexp_api_params(self, n), sexp to, sexp from, sexp ls, sexp immutp) {
  sexp oldname, newname, value, out;
  if (! sexp_envp(to)) to = sexp_context_env(ctx);
  if (! sexp_envp(from)) from = sexp_context_env(ctx);
  if (sexp_not(ls)) {
    if (sexp_truep(immutp)) {
      value = sexp_make_env(ctx);
      sexp_env_parent(value) = sexp_env_parent(to);
      sexp_env_parent(to) = value;
      sexp_immutablep(value) = 1;
      sexp_env_bindings(value) = sexp_env_bindings(from);
    } else {
      for (ls=sexp_env_bindings(from); sexp_pairp(ls); ls=sexp_cdr(ls))
        sexp_env_define(ctx, to, sexp_caar(ls), sexp_cdar(ls));
    }
  } else {
    for ( ; sexp_pairp(ls); ls=sexp_cdr(ls)) {
      if (sexp_pairp(sexp_car(ls))) {
        newname = sexp_caar(ls); oldname = sexp_cdar(ls);
      } else {
        newname = oldname = sexp_car(ls);
      }
      value = sexp_env_ref(from, oldname, SEXP_UNDEF);
      if (value != SEXP_UNDEF) {
        sexp_env_define(ctx, to, newname, value);
#if SEXP_USE_WARN_UNDEFS
      } else if (sexp_oportp(out=sexp_current_error_port(ctx))) {
        sexp_write_string(ctx, "WARNING: importing undefined variable: ", out);
        sexp_write(ctx, oldname, out);
        sexp_write_char(ctx, '\n', out);
#endif
      }
    }
  }
  return SEXP_VOID;
}

/************************** eval interface ****************************/

sexp sexp_apply (sexp ctx, sexp proc, sexp args) {
  sexp res, ls, *stack = sexp_stack_data(sexp_context_stack(ctx));
  sexp_sint_t top = sexp_context_top(ctx), len, offset;
  len = sexp_unbox_fixnum(sexp_length(ctx, args));
  if (sexp_opcodep(proc))
    proc = make_opcode_procedure(ctx, proc, len);
  if (! sexp_procedurep(proc)) {
    res = sexp_exceptionp(proc) ? proc :
      sexp_type_exception(ctx, "apply: not a procedure", proc);
  } else {
    offset = top + len;
    for (ls=args; sexp_pairp(ls); ls=sexp_cdr(ls), top++)
      stack[--offset] = sexp_car(ls);
    stack[top] = sexp_make_fixnum(len);
    top++;
    stack[top++] = SEXP_ZERO;
    stack[top++] = sexp_global(ctx, SEXP_G_FINAL_RESUMER);
    stack[top++] = SEXP_ZERO;
    sexp_context_top(ctx) = top;
    res = sexp_vm(ctx, proc);
    if (! res) res = SEXP_VOID; /* shouldn't happen */
  }
  return res;
}

sexp sexp_compile (sexp ctx, sexp x) {
  sexp_gc_var3(ast, vec, res);
  sexp_gc_preserve3(ctx, ast, vec, res);
  ast = sexp_analyze(ctx, x);
  if (sexp_exceptionp(ast)) {
    res = ast;
  } else {
    res = sexp_global(ctx, SEXP_G_OPTIMIZATIONS);
    for ( ; sexp_pairp(res); res=sexp_cdr(res))
      ast = sexp_apply_optimization(ctx, sexp_cdar(res), ast);
    sexp_free_vars(ctx, ast, SEXP_NULL);    /* should return SEXP_NULL */
    generate(ctx, ast);
    res = finalize_bytecode(ctx);
    vec = sexp_make_vector(ctx, 0, SEXP_VOID);
    res = sexp_make_procedure(ctx, SEXP_ZERO, SEXP_ZERO, res, vec);
  }
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_eval_op (sexp ctx sexp_api_params(self, n), sexp obj, sexp env) {
  sexp_sint_t top;
  sexp ctx2;
  sexp_gc_var2(res, err_handler);
  if (! env)
    env = sexp_context_env(ctx);
  else if (! sexp_envp(env))
    return sexp_type_exception(ctx, "eval: not an env", env);
  sexp_gc_preserve2(ctx, res, err_handler);
  top = sexp_context_top(ctx);
  err_handler = sexp_cdr(sexp_global(ctx, SEXP_G_ERR_HANDLER));
  sexp_cdr(sexp_global(ctx, SEXP_G_ERR_HANDLER)) = SEXP_FALSE;
  ctx2 = sexp_make_eval_context(ctx, sexp_context_stack(ctx), env, 0);
  res = sexp_compile(ctx2, obj);
  if (! sexp_exceptionp(res))
    res = sexp_apply(ctx2, res, SEXP_NULL);
  sexp_cdr(sexp_global(ctx, SEXP_G_ERR_HANDLER)) = err_handler;
  sexp_context_top(ctx) = top;
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
