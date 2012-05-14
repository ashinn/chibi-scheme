
#include "chibi/eval.h"

#define sexp_safe_pointer_tag(x) (sexp_pointerp(x) ? sexp_pointer_tag(x) : -1)

#define CHECK(expr) res = expr; check_exception(res, #expr)

void check_exception (sexp x, const char* expr) {
  if (sexp_exceptionp(x))
    fprintf(stderr, "Exception: %s => %s\n", expr,
            sexp_string_data(sexp_exception_message(x)));
}

int main (int argc, char **argv) {
  sexp ctx1, ctx2, res;

  /* Create a context and load two modules with C types. */
  ctx1 = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp_load_standard_env(ctx1, NULL, SEXP_SEVEN);
  CHECK(sexp_eval_string(ctx1, "(import (chibi net))", -1, NULL));
  CHECK(sexp_eval_string(ctx1, "(import (chibi time))", -1, NULL));

  /* Create another context and load the same modules in a different order. */
  ctx2 = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp_load_standard_env(ctx2, NULL, SEXP_SEVEN);
  CHECK(sexp_eval_string(ctx2, "(import (chibi time))", -1, NULL));
  CHECK(sexp_eval_string(ctx2, "(import (chibi net))", -1, NULL));

  /* Both instances of seconds->time should return the same type, */
  /* but with different tags. */
  CHECK(sexp_eval_string(ctx1, "(seconds->time 0)", -1, NULL));
  fprintf(stderr, "ctx1: %p (%d): %s\n", res, sexp_safe_pointer_tag(res),
          sexp_string_data(sexp_object_type_name(ctx1, res)));

  CHECK(sexp_eval_string(ctx2, "(seconds->time 0)", -1, NULL));
  fprintf(stderr, "ctx2: %p (%d): %s\n", res, sexp_safe_pointer_tag(res),
          sexp_string_data(sexp_object_type_name(ctx2, res)));

  /* Cleanup. */
  sexp_destroy_context(ctx1);
  sexp_destroy_context(ctx2);

  return 0;
}
