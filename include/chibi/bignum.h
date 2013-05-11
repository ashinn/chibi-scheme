/*  bignum.h -- header for bignum utilities                   */
/*  Copyright (c) 2009-2012 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#ifndef SEXP_BIGNUM_H
#define SEXP_BIGNUM_H

#include "chibi/eval.h"

#if (SEXP_64_BIT) && defined(__GNUC__)
typedef unsigned int uint128_t __attribute__((mode(TI)));
typedef int sint128_t __attribute__((mode(TI)));
typedef uint128_t sexp_luint_t;
typedef sint128_t sexp_lsint_t;
#else
typedef unsigned long long sexp_luint_t;
typedef long long sexp_lsint_t;
#endif

SEXP_API sexp_sint_t sexp_bignum_compare (sexp a, sexp b);
SEXP_API sexp sexp_compare (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_make_bignum (sexp ctx, sexp_uint_t len);
SEXP_API sexp sexp_copy_bignum (sexp ctx, sexp dst, sexp a, sexp_uint_t len);
SEXP_API sexp sexp_bignum_normalize (sexp a);
SEXP_API sexp_uint_t sexp_bignum_hi (sexp a);
SEXP_API sexp sexp_fixnum_to_bignum (sexp ctx, sexp a);
SEXP_API double sexp_bignum_to_double (sexp a);
SEXP_API sexp sexp_double_to_bignum (sexp ctx, double f);
SEXP_API sexp sexp_bignum_fxadd (sexp ctx, sexp a, sexp_uint_t b);
SEXP_API sexp sexp_bignum_fxmul (sexp ctx, sexp d, sexp a, sexp_uint_t b, int offset);
SEXP_API sexp_uint_t sexp_bignum_fxdiv (sexp ctx, sexp a, sexp_uint_t b, int offset);
SEXP_API sexp sexp_bignum_add (sexp ctx, sexp dst, sexp a, sexp b);
SEXP_API sexp sexp_bignum_sub (sexp ctx, sexp dst, sexp a, sexp b);
SEXP_API sexp sexp_bignum_mul (sexp ctx, sexp dst, sexp a, sexp b);
SEXP_API sexp sexp_bignum_div (sexp ctx, sexp dst, sexp a, sexp b);
SEXP_API sexp sexp_bignum_expt (sexp ctx, sexp n, sexp e);
SEXP_API sexp sexp_bignum_sqrt (sexp ctx, sexp a);
SEXP_API sexp sexp_add (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_sub (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_mul (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_div (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_quotient (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_remainder (sexp ctx, sexp a, sexp b);
#if SEXP_USE_RATIOS
SEXP_API sexp sexp_double_to_ratio (sexp ctx, double f);
SEXP_API double sexp_ratio_to_double (sexp rat);
SEXP_API sexp sexp_make_ratio (sexp ctx, sexp num, sexp den);
SEXP_API sexp sexp_ratio_normalize (sexp ctx, sexp rat, sexp in);
SEXP_API sexp sexp_ratio_round (sexp ctx, sexp a);
SEXP_API sexp sexp_ratio_trunc (sexp ctx, sexp a);
SEXP_API sexp sexp_ratio_floor (sexp ctx, sexp a);
SEXP_API sexp sexp_ratio_ceiling (sexp ctx, sexp a);
SEXP_API sexp sexp_ratio_compare (sexp ctx, sexp a, sexp b);
#endif
#if SEXP_USE_COMPLEX
SEXP_API sexp sexp_make_complex (sexp ctx, sexp real, sexp image);
SEXP_API sexp sexp_complex_normalize (sexp real);
SEXP_API sexp sexp_complex_math_error (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_sqrt (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_exp (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_expt (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_complex_log (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_sin (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_cos (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_tan (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_asin (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_acos (sexp ctx, sexp z);
SEXP_API sexp sexp_complex_atan (sexp ctx, sexp z);
#endif

#endif  /* ! SEXP_BIGNUM_H */

