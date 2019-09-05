/*  ast.c -- interface to the Abstract Syntax Tree            */
/*  Copyright (c) 2009-2015 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

#ifndef PLAN9
#include <stdlib.h>
#include <errno.h>
#endif

#ifdef _WIN32
#if defined(__MINGW32__) || defined(__MINGW64__)
/* Workaround MinGW header implementation */
errno_t getenv_s(size_t*, char*, size_t, const char*);
#endif
int setenv(const char *name, const char *value, int overwrite)
{
  int errcode = 0;
  if (!overwrite) {
    size_t envsize = 0;
    errcode = getenv_s(&envsize, NULL, 0, name);
    if (errcode || envsize) return errcode;
  }
  return _putenv_s(name, value);
}
int unsetenv(const char *name)
{
  return setenv(name, "", 1);
}
#endif

#if ! SEXP_USE_BOEHM
extern sexp sexp_gc (sexp ctx, size_t *sum_freed);
#endif

static void sexp_define_type_predicate (sexp ctx, sexp env, const char *cname, sexp_uint_t type) {
  sexp_gc_var2(name, op);
  sexp_gc_preserve2(ctx, name, op);
  name = sexp_c_string(ctx, cname, -1);
  op = sexp_make_type_predicate(ctx, name, sexp_make_fixnum(type));
  sexp_env_define(ctx, env, name=sexp_intern(ctx, cname, -1), op);
  sexp_gc_release2(ctx);
}

static void sexp_define_accessors (sexp ctx, sexp env, sexp_uint_t ctype,
                                   sexp_uint_t cindex,
                                   const char* get, const char *set) {
  sexp type, index;
  sexp_gc_var2(name, op);
  sexp_gc_preserve2(ctx, name, op);
  type = sexp_make_fixnum(ctype);
  index = sexp_make_fixnum(cindex);
  if (get) {
    op = sexp_make_getter(ctx, name=sexp_c_string(ctx, get, -1), type, index);
    sexp_env_define(ctx, env, name=sexp_intern(ctx, get, -1), op);
  }
  if (set) {
    op = sexp_make_setter(ctx, name=sexp_c_string(ctx, set, -1), type, index);
    sexp_env_define(ctx, env, name=sexp_intern(ctx, set, -1), op);
  }
  sexp_gc_release2(ctx);
}

sexp sexp_get_env_cell (sexp ctx, sexp self, sexp_sint_t n, sexp env, sexp id, sexp createp) {
  sexp cell;
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  cell = sexp_env_cell(ctx, env, id, 0);
  if (! cell) {
    if (sexp_synclop(id)) {
      env = sexp_synclo_env(id);
      id = sexp_synclo_expr(id);
    }
    cell = sexp_env_cell(ctx, env, id, 0);
    if (!cell && sexp_truep(createp))
      cell = sexp_env_cell_define(ctx, env, id, SEXP_UNDEF, NULL);
  }
  return cell ? cell : SEXP_FALSE;
}

sexp sexp_get_procedure_code (sexp ctx, sexp self, sexp_sint_t n, sexp proc) {
  sexp_assert_type(ctx, sexp_procedurep, SEXP_PROCEDURE, proc);
  return sexp_procedure_code(proc);
}

sexp sexp_get_procedure_vars (sexp ctx, sexp self, sexp_sint_t n, sexp proc) {
  sexp_assert_type(ctx, sexp_procedurep, SEXP_PROCEDURE, proc);
  return sexp_procedure_vars(proc);
}

sexp sexp_get_procedure_arity (sexp ctx, sexp self, sexp_sint_t n, sexp proc) {
  sexp_assert_type(ctx, sexp_procedurep, SEXP_PROCEDURE, proc);
  return sexp_make_fixnum(sexp_procedure_num_args(proc));
}

sexp sexp_get_procedure_variadic_p (sexp ctx, sexp self, sexp_sint_t n, sexp proc) {
  sexp_assert_type(ctx, sexp_procedurep, SEXP_PROCEDURE, proc);
  return sexp_make_boolean(sexp_procedure_variadic_p(proc));
}

sexp sexp_get_procedure_flags (sexp ctx, sexp self, sexp_sint_t n, sexp proc) {
  sexp_assert_type(ctx, sexp_procedurep, SEXP_PROCEDURE, proc);
  return sexp_make_fixnum(sexp_procedure_flags(proc));
}

sexp sexp_get_opcode_name (sexp ctx, sexp self, sexp_sint_t n, sexp op) {
  if (! sexp_opcodep(op))
    return sexp_type_exception(ctx, self, SEXP_OPCODE, op);
  else if (! sexp_opcode_name(op))
    return SEXP_FALSE;
  else
    return sexp_opcode_name(op);
}

static sexp sexp_translate_opcode_type (sexp ctx, sexp type) {
  sexp_gc_var2(res, tmp);
  res = type;
  if (! res) {
    res = sexp_type_by_index(ctx, SEXP_OBJECT);
  } if (sexp_fixnump(res)) {
    res = sexp_type_by_index(ctx, sexp_unbox_fixnum(res));
  } else if (sexp_nullp(res)) {        /* opcode list types */
    sexp_gc_preserve2(ctx, res, tmp);
    tmp = sexp_intern(ctx, "or", -1);
    res = sexp_cons(ctx, SEXP_NULL, SEXP_NULL);
    res = sexp_cons(ctx, sexp_type_by_index(ctx, SEXP_PAIR), res);
    res = sexp_cons(ctx, tmp, res);
    sexp_gc_release2(ctx);
  }
  return res;
}

sexp sexp_get_opcode_ret_type (sexp ctx, sexp self, sexp_sint_t n, sexp op) {
  sexp res;
  if (!op)
    return sexp_type_by_index(ctx, SEXP_OBJECT);
  if (! sexp_opcodep(op))
    return sexp_type_exception(ctx, self, SEXP_OPCODE, op);
  if (sexp_opcode_code(op) == SEXP_OP_RAISE)
    return sexp_list1(ctx, sexp_intern(ctx, "error", -1));
  res = sexp_opcode_return_type(op);
  if (sexp_fixnump(res))
    res = sexp_type_by_index(ctx, sexp_unbox_fixnum(res));
  return sexp_translate_opcode_type(ctx, res);
}

sexp sexp_get_opcode_param_type (sexp ctx, sexp self, sexp_sint_t n, sexp op, sexp k) {
  sexp res;
  int p = sexp_unbox_fixnum(k);
  if (! sexp_opcodep(op))
    return sexp_type_exception(ctx, self, SEXP_OPCODE, op);
  else if (! sexp_fixnump(k))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, k);
  if (p > sexp_opcode_num_args(op) && sexp_opcode_variadic_p(op))
    p = sexp_opcode_num_args(op);
  switch (p) {
  case 0:
    res = sexp_opcode_arg1_type(op);
    break;
  case 1:
    res = sexp_opcode_arg2_type(op);
    break;
  default:
    res = sexp_opcode_arg3_type(op);
    if (res && sexp_vectorp(res)) {
      if (sexp_vector_length(res) > (unsigned)(sexp_unbox_fixnum(k)-2))
        res = sexp_vector_ref(res, sexp_fx_sub(k, SEXP_TWO));
      else
        res = sexp_type_by_index(ctx, SEXP_OBJECT);
    }
    break;
  }
  return sexp_translate_opcode_type(ctx, res);
}

sexp sexp_get_opcode_class (sexp ctx, sexp self, sexp_sint_t n, sexp op) {
  sexp_assert_type(ctx, sexp_opcodep, SEXP_OPCODE, op);
  return sexp_make_fixnum(sexp_opcode_class(op));
}

sexp sexp_get_opcode_code (sexp ctx, sexp self, sexp_sint_t n, sexp op) {
  sexp_assert_type(ctx, sexp_opcodep, SEXP_OPCODE, op);
  return sexp_make_fixnum(sexp_opcode_code(op));
}

sexp sexp_get_opcode_data (sexp ctx, sexp self, sexp_sint_t n, sexp op) {
  sexp data;
  sexp_assert_type(ctx, sexp_opcodep, SEXP_OPCODE, op);
  data = sexp_opcode_data(op);
  if (!data) return SEXP_VOID;
  return sexp_opcode_class(op) == SEXP_OPC_TYPE_PREDICATE
    && 0 <= sexp_unbox_fixnum(data)
    && sexp_unbox_fixnum(data) <= sexp_context_num_types(ctx) ?
    sexp_type_by_index(ctx, sexp_unbox_fixnum(data)) : data;
}

sexp sexp_get_opcode_num_params (sexp ctx, sexp self, sexp_sint_t n, sexp op) {
  sexp_assert_type(ctx, sexp_opcodep, SEXP_OPCODE, op);
  return sexp_make_fixnum(sexp_opcode_num_args(op));
}

sexp sexp_get_opcode_variadic_p (sexp ctx, sexp self, sexp_sint_t n, sexp op) {
  sexp_assert_type(ctx, sexp_opcodep, SEXP_OPCODE, op);
  return sexp_make_boolean(sexp_opcode_variadic_p(op));
}

sexp sexp_get_port_line (sexp ctx, sexp self, sexp_sint_t n, sexp p) {
  sexp_assert_type(ctx, sexp_portp, SEXP_IPORT, p);
  return sexp_make_fixnum(sexp_port_line(p));
}

sexp sexp_set_port_line (sexp ctx, sexp self, sexp_sint_t n, sexp p, sexp i) {
  sexp_assert_type(ctx, sexp_portp, SEXP_IPORT, p);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, i);
  sexp_port_line(p) = sexp_unbox_fixnum(i);
  return SEXP_VOID;
}

sexp sexp_get_port_sourcep (sexp ctx, sexp self, sexp_sint_t n, sexp p) {
  sexp_assert_type(ctx, sexp_portp, SEXP_IPORT, p);
  return sexp_make_boolean(sexp_port_sourcep(p));
}

sexp sexp_set_port_sourcep (sexp ctx, sexp self, sexp_sint_t n, sexp p, sexp b) {
  sexp_assert_type(ctx, sexp_portp, SEXP_IPORT, p);
  sexp_assert_type(ctx, sexp_booleanp, SEXP_BOOLEAN, b);
  sexp_port_sourcep(p) = sexp_truep(b);
  return SEXP_VOID;
}

sexp sexp_type_of (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (!x)
    return sexp_type_by_index(ctx, SEXP_OBJECT);
  if (sexp_pointerp(x))
    return sexp_object_type(ctx, x);
  else if (sexp_fixnump(x))
    return sexp_type_by_index(ctx, SEXP_FIXNUM);
  else if (sexp_booleanp(x))
    return sexp_type_by_index(ctx, SEXP_BOOLEAN);
  else if (sexp_charp(x))
    return sexp_type_by_index(ctx, SEXP_CHAR);
#if SEXP_USE_HUFF_SYMS
  else if (sexp_symbolp(x))
    return sexp_type_by_index(ctx, SEXP_SYMBOL);
#endif
#if SEXP_USE_IMMEDIATE_FLONUMS
  else if (sexp_flonump(x))
    return sexp_type_by_index(ctx, SEXP_FLONUM);
#endif
  else
    return sexp_type_by_index(ctx, SEXP_OBJECT);
}

sexp sexp_env_parent_set_op (sexp ctx, sexp self, sexp_sint_t n, sexp e1, sexp e2) {
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, e1);
  if (sexp_truep(e2)) sexp_assert_type(ctx, sexp_envp, SEXP_ENV, e2);
  sexp_env_parent(e1) = e2;
  return SEXP_VOID;
}

sexp sexp_env_lambda_op (sexp ctx, sexp self, sexp_sint_t n, sexp e) {
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, e);
  return sexp_env_lambda(e) ? sexp_env_lambda(e) : SEXP_FALSE;
}

sexp sexp_env_lambda_set_op (sexp ctx, sexp self, sexp_sint_t n, sexp e, sexp lam) {
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, e);
  sexp_assert_type(ctx, sexp_lambdap, SEXP_LAMBDA, lam);
  sexp_env_lambda(e) = lam;
  return SEXP_VOID;
}

sexp sexp_env_syntactic_op (sexp ctx, sexp self, sexp_sint_t n, sexp e) {
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, e);
  return sexp_make_boolean(sexp_env_syntactic_p(e));
}

sexp sexp_env_syntactic_set_op (sexp ctx, sexp self, sexp_sint_t n, sexp e, sexp synp) {
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, e);
  sexp_env_syntactic_p(e) = sexp_truep(synp);
  return SEXP_VOID;
}

sexp sexp_env_define_op (sexp ctx, sexp self, sexp_sint_t n, sexp env, sexp name, sexp value) {
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  sexp_assert_type(ctx, sexp_idp, SEXP_SYMBOL, name);
  return sexp_env_cell_define(ctx, env, name, value, NULL);
}

sexp sexp_env_push_op (sexp ctx, sexp self, sexp_sint_t n, sexp env, sexp name, sexp value) {
  sexp_gc_var1(tmp);
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  sexp_assert_type(ctx, sexp_idp, SEXP_SYMBOL, name);
  sexp_gc_preserve1(ctx, tmp);
  sexp_env_push(ctx, env, tmp, name, value);
  sexp_gc_release1(ctx);
  return SEXP_VOID;
}

sexp sexp_core_code_op (sexp ctx, sexp self, sexp_sint_t n, sexp c) {
  sexp_assert_type(ctx, sexp_corep, SEXP_CORE, c);
  return sexp_make_fixnum(sexp_core_code(c));
}

sexp sexp_type_name_op (sexp ctx, sexp self, sexp_sint_t n, sexp t) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  return sexp_type_name(t);
}

sexp sexp_type_cpl_op (sexp ctx, sexp self, sexp_sint_t n, sexp t) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  return sexp_type_cpl(t);
}

sexp sexp_type_slots_op (sexp ctx, sexp self, sexp_sint_t n, sexp t) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  return sexp_type_slots(t);
}

sexp sexp_type_num_slots_op (sexp ctx, sexp self, sexp_sint_t n, sexp t) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  return sexp_truep(sexp_type_slots(t)) ? sexp_length(ctx, sexp_type_slots(t))
    : sexp_make_fixnum(sexp_type_field_eq_len_base(t));
}

sexp sexp_type_printer_op (sexp ctx, sexp self, sexp_sint_t n, sexp t) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  return sexp_type_print(t) ? sexp_type_print(t) : SEXP_FALSE;
}

sexp sexp_type_printer_set_op (sexp ctx, sexp self, sexp_sint_t n, sexp t, sexp p) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  sexp_assert_type(ctx, sexp_applicablep, SEXP_PROCEDURE, p);
  sexp_type_print(t) = p;
  return SEXP_VOID;
}

sexp sexp_object_size (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  sexp t;
  if ((! sexp_pointerp(x)) || (sexp_pointer_tag(x) >= sexp_context_num_types(ctx)))
    return SEXP_ZERO;
  t = sexp_object_type(ctx, x);
  return sexp_make_fixnum(sexp_type_size_of_object(t, x));
}

sexp sexp_immutablep_op (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  return sexp_pointerp(x) ? sexp_make_boolean(sexp_immutablep(x)) : SEXP_TRUE;
}

sexp sexp_make_immutable_op (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (sexp_pointerp(x)) {
    sexp_immutablep(x) = 1;
    return SEXP_TRUE;
  }
  return SEXP_FALSE;
}

sexp sexp_integer_to_immediate (sexp ctx, sexp self, sexp_sint_t n, sexp i, sexp dflt) {
  sexp x = (sexp)sexp_unbox_fixnum(i);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, i);
  if (!x || sexp_pointerp(x))
    return dflt;
  return x;
}

sexp sexp_object_to_integer (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  return sexp_make_integer(ctx, (sexp_uint_t)x);
}

sexp sexp_make_lambda_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp params, sexp body, sexp locals) {
  sexp res = sexp_alloc_type(ctx, lambda, SEXP_LAMBDA);
  sexp_lambda_name(res) = name;
  sexp_lambda_params(res) = params;
  sexp_lambda_body(res) = body;
  sexp_lambda_locals(res) = locals;
  sexp_lambda_fv(res) = SEXP_NULL;
  sexp_lambda_sv(res) = SEXP_NULL;
  sexp_lambda_defs(res) = SEXP_NULL;
  sexp_lambda_return_type(res) = SEXP_FALSE;
  sexp_lambda_param_types(res) = SEXP_NULL;
  return res;
}

sexp sexp_copy_lambda (sexp ctx, sexp self, sexp_sint_t n, sexp lambda) {
  sexp res = sexp_alloc_type(ctx, lambda, SEXP_LAMBDA);
  sexp_lambda_name(res) = sexp_lambda_name(lambda);
  sexp_lambda_params(res) = sexp_lambda_params(lambda);
  sexp_lambda_body(res) = sexp_lambda_body(lambda);
  sexp_lambda_locals(res) = sexp_lambda_locals(lambda);
  sexp_lambda_fv(res) = sexp_lambda_fv(lambda);
  sexp_lambda_sv(res) = sexp_lambda_sv(lambda);
  sexp_lambda_defs(res) = sexp_lambda_defs(lambda);
  sexp_lambda_return_type(res) = sexp_lambda_return_type(lambda);
  sexp_lambda_param_types(res) = sexp_lambda_param_types(lambda);
  return res;
}

sexp sexp_make_set_op (sexp ctx, sexp self, sexp_sint_t n, sexp var, sexp value) {
  sexp res = sexp_alloc_type(ctx, set, SEXP_SET);
  sexp_set_var(res) = var;
  sexp_set_value(res) = value;
  return res;
}

sexp sexp_make_ref_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp cell) {
  sexp res = sexp_alloc_type(ctx, ref, SEXP_REF);
  sexp_ref_name(res) = name;
  sexp_ref_cell(res) = cell;
  return res;
}

sexp sexp_make_cnd_op (sexp ctx, sexp self, sexp_sint_t n, sexp test, sexp pass, sexp fail) {
  sexp res = sexp_alloc_type(ctx, cnd, SEXP_CND);
  sexp_cnd_test(res) = test;
  sexp_cnd_pass(res) = pass;
  sexp_cnd_fail(res) = fail;
  return res;
}

sexp sexp_make_seq (sexp ctx, sexp self, sexp_sint_t n, sexp ls) {
  sexp res = sexp_alloc_type(ctx, seq, SEXP_SEQ);
  sexp_seq_ls(res) = ls;
  return res;
}

sexp sexp_make_lit_op (sexp ctx, sexp self, sexp_sint_t n, sexp value) {
  sexp res = sexp_alloc_type(ctx, lit, SEXP_LIT);
  sexp_lit_value(res) = value;
  return res;
}

sexp sexp_make_macro_op (sexp ctx, sexp self, sexp_sint_t n, sexp proc, sexp env) {
  sexp res = sexp_alloc_type(ctx, macro, SEXP_MACRO);
  sexp_macro_proc(res) = proc;
  sexp_macro_env(res) = env;
  return res;
}

sexp sexp_analyze_op (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp e) {
  sexp ctx2 = ctx;
  if (sexp_envp(e)) {
    ctx2 = sexp_make_child_context(ctx, NULL);
    sexp_context_env(ctx2) = e;
  }
  return sexp_analyze(ctx2, x);
}

sexp sexp_extend_env_op (sexp ctx, sexp self, sexp_sint_t n, sexp env, sexp vars, sexp value) {
  sexp_assert_type(ctx, sexp_envp, SEXP_ENV, env);
  return sexp_extend_env(ctx, env, vars, value);
}

sexp sexp_optimize (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  sexp_gc_var2(ls, res);
  sexp_gc_preserve2(ctx, ls, res);
  res = x;
  ls = sexp_global(ctx, SEXP_G_OPTIMIZATIONS);
  for ( ; sexp_pairp(ls); ls=sexp_cdr(ls))
    res = sexp_apply1(ctx, sexp_cdar(ls), res);
  sexp_free_vars(ctx, res, SEXP_NULL);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_gc_op (sexp ctx, sexp self, sexp_sint_t n) {
  size_t sum_freed=0;
#if SEXP_USE_BOEHM
  GC_gcollect();
#else
  sexp_gc(ctx, &sum_freed);
#endif
  return sexp_make_unsigned_integer(ctx, sum_freed);
}

sexp sexp_gc_count_op (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_make_unsigned_integer(ctx, sexp_context_gc_count(ctx));
}

sexp sexp_gc_usecs_op (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_make_unsigned_integer(ctx, sexp_context_gc_usecs(ctx));
}

#if SEXP_USE_GREEN_THREADS
sexp sexp_set_atomic (sexp ctx, sexp self, sexp_sint_t n, sexp new_val) {
  sexp res = sexp_global(ctx, SEXP_G_ATOMIC_P);
  sexp_global(ctx, SEXP_G_ATOMIC_P) = new_val;
  return res;
}
#endif

sexp sexp_thread_list (sexp ctx, sexp self, sexp_sint_t n) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_NULL;
#if SEXP_USE_GREEN_THREADS
  sexp ls;
  for (ls=sexp_global(ctx, SEXP_G_THREADS_FRONT); sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_push(ctx, res, sexp_car(ls));
  for (ls=sexp_global(ctx, SEXP_G_THREADS_PAUSED); sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_push(ctx, res, sexp_car(ls));
#endif
  if (sexp_not(sexp_memq(ctx, ctx, res))) sexp_push(ctx, res, ctx);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_string_contains (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp y, sexp start) {
  const char *res;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, x);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, y);
  sexp_assert_type(ctx, sexp_string_cursorp, SEXP_STRING_CURSOR, start);
  if (sexp_unbox_string_cursor(start) > sexp_string_size(x))
    return sexp_user_exception(ctx, self, "string-contains: start out of range", start);
  res = strstr(sexp_string_data(x) + sexp_unbox_string_cursor(start), sexp_string_data(y));
  return res ? sexp_make_string_cursor(res-sexp_string_data(x)) : SEXP_FALSE;
}

sexp sexp_string_cursor_copy (sexp ctx, sexp self, sexp_sint_t n, sexp dst, sexp sfrom, sexp src, sexp sstart, sexp send) {
  unsigned char *pfrom, *pto, *pstart, *pend, *prev, *p;
  sexp_sint_t from = sexp_unbox_fixnum(sfrom), to = sexp_string_size(dst),
    start = sexp_unbox_fixnum(sstart), end = sexp_unbox_fixnum(send);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, dst);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, src);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, sfrom);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, sstart);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, send);
  if (from < 0 || from > to)
    return sexp_user_exception(ctx, self, "string-cursor-copy!: from out of range", sfrom);
  if (start < 0 || start > (sexp_sint_t)sexp_string_size(src))
    return sexp_user_exception(ctx, self, "string-cursor-copy!: start out of range", sstart);
  if (end < start || end > (sexp_sint_t)sexp_string_size(src))
    return sexp_user_exception(ctx, self, "string-cursor-copy!: end out of range", send);
  pfrom = (unsigned char*)sexp_string_data(dst) + from;
  pto = (unsigned char*)sexp_string_data(dst) + to;
  pstart = (unsigned char*)sexp_string_data(src) + start;
  pend = (unsigned char*)sexp_string_data(src) + end;
  for ( ; pfrom < pto && pstart < pend; ++pfrom, ++pstart)
    *pfrom = *pstart;
  /* adjust for incomplete trailing chars */
  prev = (unsigned char*)sexp_string_utf8_prev(pfrom);
  if (sexp_utf8_initial_byte_count(*prev) > pfrom - prev) {
    for (p = prev; p < pfrom; ++p)
      *p = '\0';
    pstart -= pfrom - prev;
  }
  return sexp_make_fixnum(pstart - (unsigned char*)sexp_string_data(src));
}

sexp sexp_errno (sexp ctx, sexp self, sexp_sint_t n) {
#ifdef PLAN9
  return SEXP_FALSE;
#else
  return sexp_make_fixnum(errno);
#endif
}

sexp sexp_error_string (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
#ifdef PLAN9
  return SEXP_FALSE;
#else
  int err;
  if (x == SEXP_FALSE) {
    err = errno;
  } else {
    sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, x);
    err = sexp_unbox_fixnum(x);
  }
  return sexp_c_string(ctx, strerror(err), -1);
#endif
}

sexp sexp_update_free_vars (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  return sexp_free_vars(ctx, x, SEXP_NULL);
}

sexp sexp_setenv (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp value) {
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, name);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, value);
  return sexp_make_boolean(setenv(sexp_string_data(name), sexp_string_data(value), 1));
}

sexp sexp_unsetenv (sexp ctx, sexp self, sexp_sint_t n, sexp name) {
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, name);
  return sexp_make_boolean(unsetenv(sexp_string_data(name)));
}

sexp sexp_abort (sexp ctx, sexp self, sexp_sint_t n, sexp value) {
  sexp res = sexp_make_trampoline(ctx, SEXP_FALSE, value);
  sexp_exception_message(res) = SEXP_TRAMPOLINE;
  return res;
}

#define sexp_define_type(ctx, name, tag) \
  sexp_env_define(ctx, env, sexp_intern(ctx, name, -1), sexp_type_by_index(ctx, tag));

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  sexp_gc_var2(sym, str);
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_define_type(ctx, "Object", SEXP_OBJECT);
  sexp_define_type(ctx, "Number", SEXP_NUMBER);
  sexp_define_type(ctx, "Bignum", SEXP_BIGNUM);
  sexp_define_type(ctx, "Flonum", SEXP_FLONUM);
  sexp_define_type(ctx, "Integer", SEXP_FIXNUM);
#if SEXP_USE_RATIOS
  sexp_define_type(ctx, "Ratio", SEXP_RATIO);
#endif
#if SEXP_USE_COMPLEX
  sexp_define_type(ctx, "Complex", SEXP_COMPLEX);
#endif
  sexp_define_type(ctx, "Symbol", SEXP_SYMBOL);
  sexp_define_type(ctx, "Char", SEXP_CHAR);
  sexp_define_type(ctx, "Boolean", SEXP_BOOLEAN);
  sexp_define_type(ctx, "String", SEXP_STRING);
  sexp_define_type(ctx, "Byte-Vector", SEXP_BYTES);
  sexp_define_type(ctx, "Pair", SEXP_PAIR);
  sexp_define_type(ctx, "Vector", SEXP_VECTOR);
  sexp_define_type(ctx, "Input-Port", SEXP_IPORT);
  sexp_define_type(ctx, "Output-Port", SEXP_OPORT);
  sexp_define_type(ctx, "File-Descriptor", SEXP_FILENO);
  sexp_define_type(ctx, "Opcode", SEXP_OPCODE);
  sexp_define_type(ctx, "Procedure", SEXP_PROCEDURE);
  sexp_define_type(ctx, "Bytecode", SEXP_BYTECODE);
  sexp_define_type(ctx, "Env", SEXP_ENV);
  sexp_define_type(ctx, "Macro", SEXP_MACRO);
  sexp_define_type(ctx, "Lam", SEXP_LAMBDA);
  sexp_define_type(ctx, "Cnd", SEXP_CND);
  sexp_define_type(ctx, "Set", SEXP_SET);
  sexp_define_type(ctx, "Set-Syn", SEXP_SET_SYN);
  sexp_define_type(ctx, "Ref", SEXP_REF);
  sexp_define_type(ctx, "Seq", SEXP_SEQ);
  sexp_define_type(ctx, "Lit", SEXP_LIT);
  sexp_define_type(ctx, "Sc", SEXP_SYNCLO);
  sexp_define_type(ctx, "Context", SEXP_CONTEXT);
  sexp_define_type(ctx, "Exception", SEXP_EXCEPTION);
  sexp_define_type(ctx, "Core", SEXP_CORE);
  sexp_define_type_predicate(ctx, env, "environment?", SEXP_ENV);
  sexp_define_type_predicate(ctx, env, "bytecode?", SEXP_BYTECODE);
  sexp_define_type_predicate(ctx, env, "macro?", SEXP_MACRO);
  sexp_define_type_predicate(ctx, env, "syntactic-closure?", SEXP_SYNCLO);
  sexp_define_type_predicate(ctx, env, "lambda?", SEXP_LAMBDA);
  sexp_define_type_predicate(ctx, env, "cnd?", SEXP_CND);
  sexp_define_type_predicate(ctx, env, "set?", SEXP_SET);
  sexp_define_type_predicate(ctx, env, "ref?", SEXP_REF);
  sexp_define_type_predicate(ctx, env, "seq?", SEXP_SEQ);
  sexp_define_type_predicate(ctx, env, "lit?", SEXP_LIT);
  sexp_define_type_predicate(ctx, env, "opcode?", SEXP_OPCODE);
  sexp_define_type_predicate(ctx, env, "type?", SEXP_TYPE);
  sexp_define_type_predicate(ctx, env, "core?", SEXP_CORE);
  sexp_define_type_predicate(ctx, env, "context?", SEXP_CONTEXT);
  sexp_define_type_predicate(ctx, env, "file-descriptor?", SEXP_FILENO);
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 0, "syntactic-closure-env", NULL);
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 1, "syntactic-closure-vars", NULL);
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 2, "syntactic-closure-expr", NULL);
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 0, "lambda-name", "lambda-name-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 1, "lambda-params", "lambda-params-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 2, "lambda-body", "lambda-body-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 3, "lambda-defs", "lambda-defs-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 4, "lambda-locals", "lambda-locals-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 5, "lambda-flags", "lambda-flags-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 6, "lambda-free-vars", "lambda-free-vars-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 7, "lambda-set-vars", "lambda-set-vars-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 8, "lambda-return-type", "lambda-return-type-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 9, "lambda-param-types", "lambda-param-types-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 10, "lambda-source", "lambda-source-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 0, "cnd-test", "cnd-test-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 1, "cnd-pass", "cnd-pass-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 2, "cnd-fail", "cnd-fail-set!");
  sexp_define_accessors(ctx, env, SEXP_SET, 0, "set-var", "set-var-set!");
  sexp_define_accessors(ctx, env, SEXP_SET, 1, "set-value", "set-value-set!");
  sexp_define_accessors(ctx, env, SEXP_SET, 2, "set-source", "set-source-set!");
  sexp_define_accessors(ctx, env, SEXP_REF, 0, "ref-name", "ref-name-set!");
  sexp_define_accessors(ctx, env, SEXP_REF, 1, "ref-cell", "ref-cell-set!");
  sexp_define_accessors(ctx, env, SEXP_SEQ, 0, "seq-ls", "seq-ls-set!");
  sexp_define_accessors(ctx, env, SEXP_LIT, 0, "lit-value", "lit-value-set!");
  sexp_define_accessors(ctx, env, SEXP_BYTECODE, 2, "bytecode-name", "bytecode-name-set!");
  sexp_define_accessors(ctx, env, SEXP_BYTECODE, 3, "bytecode-literals", NULL);
  sexp_define_accessors(ctx, env, SEXP_BYTECODE, 4, "bytecode-source", NULL);
  sexp_define_accessors(ctx, env, SEXP_EXCEPTION, 1, "exception-message", NULL);
  sexp_define_accessors(ctx, env, SEXP_EXCEPTION, 4, "exception-source", NULL);
  sexp_define_accessors(ctx, env, SEXP_MACRO, 0, "macro-procedure", NULL);
  sexp_define_accessors(ctx, env, SEXP_MACRO, 1, "macro-env", NULL);
  sexp_define_accessors(ctx, env, SEXP_MACRO, 2, "macro-source", NULL);
  sexp_define_accessors(ctx, env, SEXP_MACRO, 3, "macro-aux", "macro-aux-set!");
  sexp_define_foreign(ctx, env, "procedure-code", 1, sexp_get_procedure_code);
  sexp_define_foreign(ctx, env, "procedure-vars", 1, sexp_get_procedure_vars);
  sexp_define_foreign(ctx, env, "procedure-arity", 1, sexp_get_procedure_arity);
  sexp_define_foreign(ctx, env, "procedure-variadic?", 1, sexp_get_procedure_variadic_p);
  sexp_define_foreign(ctx, env, "procedure-flags", 1, sexp_get_procedure_flags);
  sexp_define_foreign(ctx, env, "copy-lambda", 1, sexp_copy_lambda);
  sexp_define_foreign_opt(ctx, env, "make-lambda", 4, sexp_make_lambda_op, SEXP_NULL);
  sexp_define_foreign_opt(ctx, env, "make-cnd", 3, sexp_make_cnd_op, SEXP_VOID);
  sexp_define_foreign(ctx, env, "make-ref", 2, sexp_make_ref_op);
  sexp_define_foreign(ctx, env, "make-set", 2, sexp_make_set_op);
  sexp_define_foreign(ctx, env, "make-lit", 1, sexp_make_lit_op);
  sexp_define_foreign(ctx, env, "make-seq", 1, sexp_make_seq);
  sexp_define_foreign(ctx, env, "make-macro", 2, sexp_make_macro_op);
  sexp_define_foreign_opt(ctx, env, "analyze", 2, sexp_analyze_op, SEXP_FALSE);
  sexp_define_foreign(ctx, env, "optimize", 1, sexp_optimize);
  sexp_define_foreign(ctx, env, "extend-env", 3, sexp_extend_env_op);
  sexp_define_foreign_opt(ctx, env, "env-cell", 3, sexp_get_env_cell, SEXP_FALSE);
  sexp_define_foreign(ctx, env, "opcode-name", 1, sexp_get_opcode_name);
  sexp_define_foreign(ctx, env, "opcode-class", 1, sexp_get_opcode_class);
  sexp_define_foreign(ctx, env, "opcode-code", 1, sexp_get_opcode_code);
  sexp_define_foreign(ctx, env, "opcode-data", 1, sexp_get_opcode_data);
  sexp_define_foreign(ctx, env, "opcode-variadic?", 1, sexp_get_opcode_variadic_p);
  sexp_define_foreign(ctx, env, "opcode-num-params", 1, sexp_get_opcode_num_params);
  sexp_define_foreign(ctx, env, "opcode-return-type", 1, sexp_get_opcode_ret_type);
  sexp_define_foreign(ctx, env, "opcode-param-type", 2, sexp_get_opcode_param_type);
  sexp_define_foreign(ctx, env, "port-line", 1, sexp_get_port_line);
  sexp_define_foreign(ctx, env, "port-line-set!", 2, sexp_set_port_line);
  sexp_define_foreign(ctx, env, "port-source?", 1, sexp_get_port_sourcep);
  sexp_define_foreign(ctx, env, "port-source?-set!", 2, sexp_set_port_sourcep);
  sexp_define_foreign(ctx, env, "type-of", 1, sexp_type_of);
  sexp_define_foreign(ctx, env, "type-name", 1, sexp_type_name_op);
  sexp_define_foreign(ctx, env, "type-cpl", 1, sexp_type_cpl_op);
  sexp_define_foreign(ctx, env, "type-slots", 1, sexp_type_slots_op);
  sexp_define_foreign(ctx, env, "type-num-slots", 1, sexp_type_num_slots_op);
  sexp_define_foreign(ctx, env, "type-printer", 1, sexp_type_printer_op);
  sexp_define_foreign(ctx, env, "type-printer-set!", 2, sexp_type_printer_set_op);
  sexp_define_foreign(ctx, env, "env-parent-set!", 2, sexp_env_parent_set_op);
  sexp_define_foreign(ctx, env, "env-lambda", 1, sexp_env_lambda_op);
  sexp_define_foreign(ctx, env, "env-lambda-set!", 2, sexp_env_lambda_set_op);
  sexp_define_foreign(ctx, env, "env-syntactic?", 1, sexp_env_syntactic_op);
  sexp_define_foreign(ctx, env, "env-syntactic?-set!", 2, sexp_env_syntactic_set_op);
  sexp_define_foreign(ctx, env, "env-define!", 3, sexp_env_define_op);
  sexp_define_foreign(ctx, env, "env-push!", 3, sexp_env_push_op);
  sexp_define_foreign(ctx, env, "core-code", 1, sexp_core_code_op);
  sexp_define_foreign(ctx, env, "object-size", 1, sexp_object_size);
  sexp_define_foreign(ctx, env, "immutable?", 1, sexp_immutablep_op);
  sexp_define_foreign(ctx, env, "make-immutable!", 1, sexp_make_immutable_op);
  sexp_define_foreign_opt(ctx, env, "integer->immediate", 2, sexp_integer_to_immediate, SEXP_FALSE);
  sexp_define_foreign_opt(ctx, env, "object->integer", 1, sexp_object_to_integer, SEXP_FALSE);
  sexp_define_foreign(ctx, env, "gc", 0, sexp_gc_op);
  sexp_define_foreign(ctx, env, "gc-count", 0, sexp_gc_count_op);
  sexp_define_foreign(ctx, env, "gc-usecs", 0, sexp_gc_usecs_op);
#if SEXP_USE_GREEN_THREADS
  sexp_define_foreign(ctx, env, "%set-atomic!", 1, sexp_set_atomic);
#endif
  sexp_define_foreign(ctx, env, "thread-list", 0, sexp_thread_list);
  sexp_define_foreign_opt(ctx, env, "string-contains", 3, sexp_string_contains, sexp_make_string_cursor(0));
  sexp_define_foreign(ctx, env, "string-cursor-copy!", 5, sexp_string_cursor_copy);
  sexp_define_foreign(ctx, env, "errno", 0, sexp_errno);
  sexp_define_foreign_opt(ctx, env, "integer->error-string", 1, sexp_error_string, SEXP_FALSE);
  sexp_define_foreign(ctx, env, "update-free-vars!", 1, sexp_update_free_vars);
  sexp_define_foreign(ctx, env, "setenv", 2, sexp_setenv);
  sexp_define_foreign(ctx, env, "unsetenv", 1, sexp_unsetenv);
  sexp_define_foreign(ctx, env, "abort", 1, sexp_abort);
  sexp_gc_preserve2(ctx, sym, str);
  sym = sexp_intern(ctx, "chibi-version", -1);
  str = sexp_c_string(ctx, sexp_version, -1);
  sexp_immutablep(str) = 1;
  sexp_env_define(ctx, env, sym, str);
  sexp_gc_release2(ctx);
  return SEXP_VOID;
}
