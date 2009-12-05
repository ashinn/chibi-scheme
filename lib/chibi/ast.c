
#include <chibi/eval.h>

static void sexp_define_type_predicate (sexp ctx, sexp env,
                                        char *cname, sexp_uint_t type) {
  sexp_gc_var2(name, op);
  sexp_gc_preserve2(ctx, name, op);
  name = sexp_c_string(ctx, cname, -1);
  op = sexp_make_type_predicate(ctx, name, sexp_make_fixnum(type));
  sexp_env_define(ctx, env, name=sexp_intern(ctx, cname), op);
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
  sexp_env_define(ctx, env, name=sexp_intern(ctx, get), op);
  op = sexp_make_setter(ctx, name=sexp_c_string(ctx, set, -1), type, index);
  sexp_env_define(ctx, env, name=sexp_intern(ctx, set), op);
  sexp_gc_release2(ctx);
}

sexp sexp_init_library (sexp ctx, sexp env) {
  sexp_gc_var2(name, op);
  sexp_gc_preserve2(ctx, name, op);
  sexp_define_type_predicate(ctx, env, "lambda?", SEXP_LAMBDA);
  sexp_define_type_predicate(ctx, env, "cnd?", SEXP_CND);
  sexp_define_type_predicate(ctx, env, "set?", SEXP_SET);
  sexp_define_type_predicate(ctx, env, "ref?", SEXP_REF);
  sexp_define_type_predicate(ctx, env, "seq?", SEXP_SEQ);
  sexp_define_type_predicate(ctx, env, "lit?", SEXP_LIT);
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 0, "lambda-name", "lambda-name-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 1, "lambda-params", "lambda-params-set!");
  sexp_define_accessors(ctx, env, SEXP_LAMBDA, 7, "lambda-body", "lambda-body-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 0, "cnd-test", "cnd-test-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 1, "cnd-pass", "cnd-pass-set!");
  sexp_define_accessors(ctx, env, SEXP_CND, 2, "cnd-fail", "cnd-fail-set!");
  sexp_define_accessors(ctx, env, SEXP_SET, 0, "set-var", "set-var-set!");
  sexp_define_accessors(ctx, env, SEXP_SET, 1, "set-value", "set-value-set!");
  sexp_define_accessors(ctx, env, SEXP_REF, 0, "ref-name", "ref-name-set!");
  sexp_define_accessors(ctx, env, SEXP_REF, 1, "ref-cell", "ref-cell-set!");
  sexp_define_accessors(ctx, env, SEXP_SEQ, 0, "seq-ls", "seq-ls-set!");
  sexp_define_accessors(ctx, env, SEXP_LIT, 0, "list-value", "lit-value-set!");
  sexp_define_foreign(ctx, env, "analyze", 1, sexp_analyze);
  sexp_gc_release2(ctx);
  return SEXP_VOID;
}

