/*  ast.c -- interface to the Abstract Syntax Tree            */
/*  Copyright (c) 2009-2010 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

#if ! SEXP_USE_BOEHM
extern sexp sexp_gc (sexp ctx, size_t *sum_freed);
#endif

static void sexp_define_type_predicate (sexp ctx, sexp env, char *cname, sexp_uint_t type) {
  sexp_gc_var2(name, op);
  sexp_gc_preserve2(ctx, name, op);
  name = sexp_c_string(ctx, cname, -1);
  op = sexp_make_type_predicate(ctx, name, sexp_make_fixnum(type));
  sexp_env_define(ctx, env, name=sexp_intern(ctx, cname, -1), op);
  sexp_gc_release2(ctx);
}

static void sexp_define_accessors (sexp ctx, sexp env, sexp_uint_t ctype,
                                   sexp_uint_t cindex, char* get, char *set) {
  sexp type, index;
  sexp_gc_var2(name, op);
  sexp_gc_preserve2(ctx, name, op);
  type = sexp_make_fixnum(ctype);
  index = sexp_make_fixnum(cindex);
  op = sexp_make_getter(ctx, name=sexp_c_string(ctx, get, -1), type, index);
  sexp_env_define(ctx, env, name=sexp_intern(ctx, get, -1), op);
  op = sexp_make_setter(ctx, name=sexp_c_string(ctx, set, -1), type, index);
  sexp_env_define(ctx, env, name=sexp_intern(ctx, set, -1), op);
  sexp_gc_release2(ctx);
}

static sexp sexp_get_env_cell (sexp ctx sexp_api_params(self, n), sexp env, sexp id) {
  sexp cell = sexp_env_cell(env, id, 0);
  while ((! cell) && sexp_synclop(id)) {
    env = sexp_synclo_env(id);
    id = sexp_synclo_expr(id);
  }
  return cell ? cell : SEXP_FALSE;
}

static sexp sexp_get_opcode_name (sexp ctx sexp_api_params(self, n), sexp op) {
  if (! sexp_opcodep(op))
    return sexp_type_exception(ctx, self, SEXP_OPCODE, op);
  else if (! sexp_opcode_name(op))
    return SEXP_FALSE;
  else
    return sexp_intern(ctx, sexp_opcode_name(op), -1);
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

static sexp sexp_get_opcode_ret_type (sexp ctx sexp_api_params(self, n), sexp op) {
  sexp res;
  if (! sexp_opcodep(op))
    return sexp_type_exception(ctx, self, SEXP_OPCODE, op);
  if (sexp_opcode_code(op) == SEXP_OP_RAISE)
    return sexp_list1(ctx, sexp_intern(ctx, "error", -1));
  res = sexp_opcode_return_type(op);
  if (sexp_fixnump(res))
    res = sexp_type_by_index(ctx, sexp_unbox_fixnum(res));
  return sexp_translate_opcode_type(ctx, res);
}

static sexp sexp_get_opcode_param_type (sexp ctx sexp_api_params(self, n), sexp op, sexp k) {
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
    if (sexp_vectorp(res)) {
      if (sexp_vector_length(res) > (sexp_unbox_fixnum(k)-2))
        res = sexp_vector_ref(res, sexp_fx_sub(k, SEXP_TWO));
      else
        res = sexp_type_by_index(ctx, SEXP_OBJECT);
    }
    break;
  }
  return sexp_translate_opcode_type(ctx, res);
}

static sexp sexp_get_opcode_num_params (sexp ctx sexp_api_params(self, n), sexp op) {
  if (! sexp_opcodep(op))
    return sexp_type_exception(ctx, self, SEXP_OPCODE, op);
  return sexp_make_fixnum(sexp_opcode_num_args(op));
}

static sexp sexp_get_opcode_variadic_p (sexp ctx sexp_api_params(self, n), sexp op) {
  if (! sexp_opcodep(op))
    return sexp_type_exception(ctx, self, SEXP_OPCODE, op);
  return sexp_make_boolean(sexp_opcode_variadic_p(op));
}

static sexp sexp_get_port_line (sexp ctx sexp_api_params(self, n), sexp p) {
  sexp_assert_type(ctx, sexp_portp, SEXP_IPORT, p);
  return sexp_make_fixnum(sexp_port_line(p));
}

static sexp sexp_set_port_line (sexp ctx sexp_api_params(self, n), sexp p, sexp i) {
  sexp_assert_type(ctx, sexp_portp, SEXP_IPORT, p);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, i);
  sexp_port_line(p) = sexp_unbox_fixnum(i);
  return SEXP_VOID;
}

static sexp sexp_type_of (sexp ctx sexp_api_params(self, n), sexp x) {
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

static sexp sexp_type_name_op (sexp ctx sexp_api_params(self, n), sexp t) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  return sexp_c_string(ctx, sexp_type_name(t), -1);
}

static sexp sexp_type_cpl_op (sexp ctx sexp_api_params(self, n), sexp t) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  return sexp_type_cpl(t);
}

static sexp sexp_type_slots_op (sexp ctx sexp_api_params(self, n), sexp t) {
  sexp_assert_type(ctx, sexp_typep, SEXP_TYPE, t);
  return sexp_type_slots(t);
}

static sexp sexp_object_size (sexp ctx sexp_api_params(self, n), sexp x) {
  sexp t;
  if ((! sexp_pointerp(x)) || (sexp_pointer_tag(x) >= sexp_context_num_types(ctx)))
    return SEXP_ZERO;
  t = sexp_object_type(ctx, x);
  return sexp_make_fixnum(sexp_type_size_of_object(t, x));
}

static sexp sexp_integer_to_immediate (sexp ctx sexp_api_params(self, n), sexp i, sexp dflt) {
  sexp x = (sexp)sexp_unbox_fixnum(i);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, i);
  if (sexp_pointerp(x))
    return dflt;
  return x;
}

static sexp sexp_analyze_op (sexp ctx sexp_api_params(self, n), sexp x, sexp e) {
  sexp ctx2 = ctx;
  if (sexp_envp(e)) {
    ctx2 = sexp_make_child_context(ctx, NULL);
    sexp_context_env(ctx2) = e;
  }
  return sexp_analyze(ctx2, x);
}

static sexp sexp_optimize (sexp ctx sexp_api_params(self, n), sexp x) {
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

static sexp sexp_gc_op (sexp ctx sexp_api_params(self, n)) {
  size_t sum_freed=0;
#if SEXP_USE_BOEHM
  GC_gcollect();
#else
  sexp_gc(ctx, &sum_freed);
#endif
  return sexp_make_unsigned_integer(ctx, sum_freed);
}


static sexp sexp_string_contains (sexp ctx sexp_api_params(self, n), sexp x, sexp y) {
  const char *res;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, x);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, y);
  res = strstr(sexp_string_data(x), sexp_string_data(y));
  return res ? sexp_make_fixnum(res-sexp_string_data(x)) : SEXP_FALSE;
}

#define sexp_define_type(ctx, name, tag) \
  sexp_env_define(ctx, env, sexp_intern(ctx, name, -1), sexp_type_by_index(ctx, tag));

sexp sexp_init_library (sexp ctx sexp_api_params(self, n), sexp env) {
  sexp_define_type(ctx, "<object>", SEXP_OBJECT);
  sexp_define_type(ctx, "<number>", SEXP_NUMBER);
  sexp_define_type(ctx, "<bignum>", SEXP_BIGNUM);
  sexp_define_type(ctx, "<flonum>", SEXP_FLONUM);
  sexp_define_type(ctx, "<integer>", SEXP_FIXNUM);
  sexp_define_type(ctx, "<symbol>", SEXP_SYMBOL);
  sexp_define_type(ctx, "<char>", SEXP_CHAR);
  sexp_define_type(ctx, "<boolean>", SEXP_BOOLEAN);
  sexp_define_type(ctx, "<string>", SEXP_STRING);
  sexp_define_type(ctx, "<byte-vector>", SEXP_BYTES);
  sexp_define_type(ctx, "<pair>", SEXP_PAIR);
  sexp_define_type(ctx, "<vector>", SEXP_VECTOR);
  sexp_define_type(ctx, "<opcode>", SEXP_OPCODE);
  sexp_define_type(ctx, "<procedure>", SEXP_PROCEDURE);
  sexp_define_type(ctx, "<bytecode>", SEXP_BYTECODE);
  sexp_define_type(ctx, "<env>", SEXP_ENV);
  sexp_define_type(ctx, "<macro>", SEXP_MACRO);
  sexp_define_type(ctx, "<lam>", SEXP_LAMBDA);
  sexp_define_type(ctx, "<cnd>", SEXP_CND);
  sexp_define_type(ctx, "<set>", SEXP_SET);
  sexp_define_type(ctx, "<ref>", SEXP_REF);
  sexp_define_type(ctx, "<seq>", SEXP_SEQ);
  sexp_define_type(ctx, "<lit>", SEXP_LIT);
  sexp_define_type(ctx, "<sc>", SEXP_SYNCLO);
  sexp_define_type(ctx, "<context>", SEXP_CONTEXT);
  sexp_define_type(ctx, "<exception>", SEXP_EXCEPTION);
  sexp_define_type_predicate(ctx, env, "environment?", SEXP_ENV);
  sexp_define_type_predicate(ctx, env, "bytecode?", SEXP_BYTECODE);
  sexp_define_type_predicate(ctx, env, "exception?", SEXP_EXCEPTION);
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
  sexp_define_type_predicate(ctx, env, "context?", SEXP_CONTEXT);
  sexp_define_type_predicate(ctx, env, "exception?", SEXP_EXCEPTION);
  sexp_define_accessors(ctx, env, SEXP_PAIR, 2, "pair-source", "pair-source-set!");
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 0, "syntactic-closure-env", "syntactic-closure-env-set!");
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 1, "syntactic-closure-vars", "syntactic-closure-vars-set!");
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 2, "syntactic-closure-expr", "syntactic-closure-expr-set!");
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
  sexp_define_accessors(ctx, env, SEXP_REF, 0, "ref-name", "ref-name-set!");
  sexp_define_accessors(ctx, env, SEXP_REF, 1, "ref-cell", "ref-cell-set!");
  sexp_define_accessors(ctx, env, SEXP_SEQ, 0, "seq-ls", "seq-ls-set!");
  sexp_define_accessors(ctx, env, SEXP_LIT, 0, "lit-value", "lit-value-set!");
  sexp_define_accessors(ctx, env, SEXP_PROCEDURE, 1, "procedure-code", "procedure-code-set!");
  sexp_define_accessors(ctx, env, SEXP_PROCEDURE, 2, "procedure-vars", "procedure-vars-set!");
  sexp_define_accessors(ctx, env, SEXP_BYTECODE, 1, "bytecode-name", "bytecode-name-set!");
  sexp_define_accessors(ctx, env, SEXP_BYTECODE, 2, "bytecode-literals", "bytecode-literals-set!");
  sexp_define_accessors(ctx, env, SEXP_BYTECODE, 3, "bytecode-source", "bytecode-source-set!");
  sexp_define_accessors(ctx, env, SEXP_EXCEPTION, 0, "exception-kind", "exception-kind-set!");
  sexp_define_accessors(ctx, env, SEXP_EXCEPTION, 1, "exception-message", "exception-message-set!");
  sexp_define_accessors(ctx, env, SEXP_EXCEPTION, 2, "exception-irritants", "exception-irritants-set!");
  sexp_define_accessors(ctx, env, SEXP_MACRO, 0, "macro-procedure", "macro-procedure-set!");
  sexp_define_accessors(ctx, env, SEXP_MACRO, 1, "macro-env", "macro-env-set!");
  sexp_define_accessors(ctx, env, SEXP_MACRO, 2, "macro-source", "macro-source-set!");
  sexp_define_foreign_opt(ctx, env, "analyze", 2, sexp_analyze_op, SEXP_FALSE);
  sexp_define_foreign(ctx, env, "extend-env", 2, sexp_extend_env);
  sexp_define_foreign(ctx, env, "env-cell", 2, sexp_get_env_cell);
  sexp_define_foreign(ctx, env, "opcode-name", 1, sexp_get_opcode_name);
  sexp_define_foreign(ctx, env, "opcode-variadic?", 1, sexp_get_opcode_variadic_p);
  sexp_define_foreign(ctx, env, "opcode-num-params", 1, sexp_get_opcode_num_params);
  sexp_define_foreign(ctx, env, "opcode-return-type", 1, sexp_get_opcode_ret_type);
  sexp_define_foreign(ctx, env, "opcode-param-type", 2, sexp_get_opcode_param_type);
  sexp_define_foreign(ctx, env, "port-line", 1, sexp_get_port_line);
  sexp_define_foreign(ctx, env, "port-line-set!", 2, sexp_set_port_line);
  sexp_define_foreign(ctx, env, "optimize", 1, sexp_optimize);
  sexp_define_foreign(ctx, env, "type-of", 1, sexp_type_of);
  sexp_define_foreign(ctx, env, "type-name", 1, sexp_type_name_op);
  sexp_define_foreign(ctx, env, "type-cpl", 1, sexp_type_cpl_op);
  sexp_define_foreign(ctx, env, "type-slots", 1, sexp_type_slots_op);
  sexp_define_foreign(ctx, env, "object-size", 1, sexp_object_size);
  sexp_define_foreign_opt(ctx, env, "integer->immediate", 2, sexp_integer_to_immediate, SEXP_FALSE);
  sexp_define_foreign(ctx, env, "gc", 0, sexp_gc_op);
  sexp_define_foreign(ctx, env, "string-contains", 2, sexp_string_contains);
  return SEXP_VOID;
}

