
#include "chibi/eval.h"

sexp sexp_apply_times_to_pi(sexp ctx, sexp self, sexp_sint_t n,
                            sexp fn, sexp times) {
  int i;
  sexp_gc_var1(tmp);
  sexp_assert_type(ctx, sexp_applicablep, SEXP_PROCEDURE, fn);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, times);
  sexp_gc_preserve1(ctx, tmp);
  for (i=0; i<sexp_unbox_fixnum(times); i++) {
    tmp = sexp_make_flonum(ctx, 3.14);
    tmp = sexp_apply(ctx, fn, tmp = sexp_list1(ctx, tmp));
  }
  sexp_gc_release1(ctx);
  return SEXP_VOID;
}

int main (int argc, char **argv) {
  sexp ctx, res;

  /* Create a context and load two modules with C types. */
  ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(ctx, NULL, stdin, stdout, stderr, 1);
  sexp_define_foreign(ctx, sexp_context_env(ctx),
                      "apply-times-to-pi", 2, sexp_apply_times_to_pi);
  sexp_eval_string(ctx, "(define counter 0)", -1, NULL);
  sexp_eval_string(ctx,
                   "(apply-times-to-pi"
                   " (lambda (pi) (set! counter (+ counter pi)))"
                   " 1000000)",
                   -1,
                   NULL);
  sexp_eval_string(ctx, "(begin (write (round counter)) (newline))", -1, NULL);

  sexp_destroy_context(ctx);

  return 0;
}
