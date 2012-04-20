/* time.c -- R7RS time routines                              */
/* Copyright (c) 2011-2012 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt       */

#include <time.h>
#include <chibi/eval.h>

/* TODO: Check a leap second table file at appropriate intervals. */
static time_t leap_seconds_since_epoch = 24;

static sexp sexp_current_second (sexp ctx, sexp self, sexp_sint_t n) {
  time_t res = time(NULL);
  return sexp_make_flonum(ctx, res + leap_seconds_since_epoch);
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return sexp_global(ctx, SEXP_G_ABI_ERROR);
  sexp_define_foreign(ctx, env, "current-second", 0, sexp_current_second);
  return SEXP_VOID;
}
