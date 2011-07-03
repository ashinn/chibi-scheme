/*  profile.c -- low-level utilities for VM profiling         */
/*  Copyright (c) 2011 Alex Shinn.  All rights reserved.      */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

static sexp sexp_increment_cdr (sexp ctx sexp_api_params(self, n), sexp pair) {
  sexp_assert_type(ctx, sexp_pairp, SEXP_PAIR, pair);
  sexp_cdr(pair) = sexp_make_fixnum(1 + sexp_unbox_fixnum(sexp_cdr(pair)));
  return SEXP_VOID;
}

sexp sexp_init_library (sexp ctx sexp_api_params(self, n), sexp env) {
  sexp_define_foreign(ctx, env, "increment-cdr!", 1, sexp_increment_cdr);
  return SEXP_VOID;
}
