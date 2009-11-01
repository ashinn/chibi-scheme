/*  bignum.h -- header for bignum utilities              */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifndef SEXP_BIGNUM_H
#define SEXP_BIGNUM_H

#if (SEXP_64_BIT)
typedef unsigned int uint128_t __attribute__((mode(TI)));
typedef int sint128_t __attribute__((mode(TI)));
typedef uint128_t sexp_luint_t;
typedef sint128_t sexp_lsint_t;
#else
typedef unsigned long long sexp_luint_t;
typedef long long sexp_lsint_t;
#endif

sexp_sint_t sexp_bignum_compare (sexp a, sexp b);
sexp sexp_compare (sexp ctx, sexp a, sexp b);
sexp sexp_make_bignum (sexp ctx, sexp_uint_t len);
sexp sexp_copy_bignum (sexp ctx, sexp dst, sexp a, sexp_uint_t len);
sexp sexp_fixnum_to_bignum (sexp ctx, sexp a);
double sexp_bignum_to_double (sexp a);
sexp sexp_double_to_bignum (sexp ctx, double f);
sexp sexp_bignum_fxadd (sexp ctx, sexp a, sexp_uint_t b);
sexp sexp_bignum_fxmul (sexp ctx, sexp d, sexp a, sexp_uint_t b, int offset);
sexp_uint_t sexp_bignum_fxdiv (sexp ctx, sexp a, sexp_uint_t b, int offset);
sexp sexp_bignum_add (sexp ctx, sexp dst, sexp a, sexp b);
sexp sexp_bignum_sub (sexp ctx, sexp dst, sexp a, sexp b);
sexp sexp_bignum_mul (sexp ctx, sexp dst, sexp a, sexp b);
sexp sexp_bignum_div (sexp ctx, sexp dst, sexp a, sexp b);
sexp sexp_bignum_expt (sexp ctx, sexp n, sexp e);
sexp sexp_add (sexp ctx, sexp a, sexp b);
sexp sexp_sub (sexp ctx, sexp a, sexp b);
sexp sexp_mul (sexp ctx, sexp a, sexp b);
sexp sexp_div (sexp ctx, sexp a, sexp b);
sexp sexp_quotient (sexp ctx, sexp a, sexp b);
sexp sexp_remainder (sexp ctx, sexp a, sexp b);

#endif  /* ! SEXP_BIGNUM_H */

