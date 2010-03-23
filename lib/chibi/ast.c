/*  ast.c -- interface to the Abstract Syntax Tree       */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include <chibi/eval.h>

static void sexp_define_type_predicate (sexp ctx, sexp env,
                                        char *cname, sexp_uint_t type) {
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

static sexp sexp_get_env_cell (sexp ctx, sexp env, sexp id) {
  sexp cell = sexp_env_cell(env, id);
  while ((! cell) && sexp_synclop(id)) {
    env = sexp_synclo_env(id);
    id = sexp_synclo_expr(id);
  }
  return cell ? cell : SEXP_FALSE;
}

static sexp sexp_get_opcode_name (sexp ctx, sexp op) {
  if (! sexp_opcodep(op))
    return sexp_type_exception(ctx, "not an opcode", op);
  else if (! sexp_opcode_name(op))
    return SEXP_FALSE;
  else
    return sexp_intern(ctx, sexp_opcode_name(op), -1);
}

sexp sexp_init_library (sexp ctx, sexp env) {
  sexp_define_type_predicate(ctx, env, "syntactic-closure?", SEXP_SYNCLO);
  sexp_define_type_predicate(ctx, env, "lambda?", SEXP_LAMBDA);
  sexp_define_type_predicate(ctx, env, "cnd?", SEXP_CND);
  sexp_define_type_predicate(ctx, env, "set?", SEXP_SET);
  sexp_define_type_predicate(ctx, env, "ref?", SEXP_REF);
  sexp_define_type_predicate(ctx, env, "seq?", SEXP_SEQ);
  sexp_define_type_predicate(ctx, env, "lit?", SEXP_LIT);
  sexp_define_type_predicate(ctx, env, "opcode?", SEXP_OPCODE);
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 0, "syntactic-closure-env", "syntactic-closure-env-set!");
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 1, "syntactic-closure-vars", "syntactic-closure-vars-set!");
  sexp_define_accessors(ctx, env, SEXP_SYNCLO, 2, "syntactic-closure-expr", "syntactic-closure-expr-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 0, "lambda-name", "lambda-name-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 1, "lambda-params", "lambda-params-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 2, "lambda-body", "lambda-body-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 3, "lambda-defs", "lambda-defs-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 0, "cnd-test", "cnd-test-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 1, "cnd-pass", "cnd-pass-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 2, "cnd-fail", "cnd-fail-set!");
  sexp_define_accessors(ctx, env, SEXP_SET, 0, "set-var", "set-var-set!");
  sexp_define_accessors(ctx, env, SEXP_SET, 1, "set-value", "set-value-set!");
  sexp_define_accessors(ctx, env, SEXP_REF, 0, "ref-name", "ref-name-set!");
  sexp_define_accessors(ctx, env, SEXP_REF, 1, "ref-cell", "ref-cell-set!");
  sexp_define_accessors(ctx, env, SEXP_SEQ, 0, "seq-ls", "seq-ls-set!");
  sexp_define_accessors(ctx, env, SEXP_LIT, 0, "lit-value", "lit-value-set!");
  sexp_define_foreign(ctx, env, "analyze", 1, sexp_analyze);
  sexp_define_foreign(ctx, env, "extend-env", 2, sexp_extend_env);
  sexp_define_foreign(ctx, env, "env-cell", 2, sexp_get_env_cell);
  sexp_define_foreign(ctx, env, "opcode-name", 1, sexp_get_opcode_name);
  return SEXP_VOID;
}

