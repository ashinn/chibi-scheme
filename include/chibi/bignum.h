/*  bignum.h -- header for bignum utilities                   */
/*  Copyright (c) 2009-2020 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#ifndef SEXP_BIGNUM_H
#define SEXP_BIGNUM_H

#include "chibi/eval.h"

#if SEXP_USE_CUSTOM_LONG_LONGS
#ifdef PLAN9
#include <ape/stdint.h>
#else
#include <stdint.h>
#endif
typedef struct
{
  uint64_t hi;
  uint64_t lo;
} sexp_luint_t;
typedef struct
{
  int64_t  hi;
  uint64_t lo;
} sexp_lsint_t;
#elif SEXP_64_BIT
typedef unsigned int uint128_t __attribute__((mode(TI)));
typedef int sint128_t __attribute__((mode(TI)));
typedef uint128_t sexp_luint_t;
typedef sint128_t sexp_lsint_t;
#else
typedef unsigned long long sexp_luint_t;
typedef long long sexp_lsint_t;
#endif

#if !SEXP_USE_CUSTOM_LONG_LONGS

#define sexp_lsint_fits_sint(x) ((sexp_sint_t)x == x)
#define sexp_luint_fits_uint(x) ((sexp_uint_t)x == x)
#define lsint_from_sint(v) ((sexp_lsint_t)v)
#define luint_from_uint(v) ((sexp_luint_t)v)
#define lsint_to_sint(v) ((sexp_sint_t)v)
#define luint_to_uint(v) ((sexp_uint_t)v)
#define lsint_to_sint_hi(v) ((sexp_sint_t) ((v) >> (8*sizeof(sexp_sint_t))))
#define luint_to_uint_hi(v) ((sexp_uint_t) ((v) >> (8*sizeof(sexp_uint_t))))
#define lsint_negate(v) (-((sexp_lsint_t)v))
#define luint_eq(a, b) (((sexp_luint_t)a)==((sexp_luint_t)b))
#define luint_lt(a, b) (((sexp_luint_t)a)<((sexp_luint_t)b))
#define lsint_lt_0(a) (((sexp_lsint_t)a)<0)
#define luint_shl(a, shift) (((sexp_luint_t)a)<<(shift))
#define luint_shr(a, shift) (((sexp_luint_t)a)>>(shift))
#define luint_add(a, b) (((sexp_luint_t)a)+((sexp_luint_t)b))
#define luint_add_uint(a, b) (((sexp_luint_t)a)+((sexp_uint_t)b))
#define luint_sub(a, b) (((sexp_luint_t)a)-((sexp_luint_t)b))
#define luint_mul_uint(a, b) (((sexp_luint_t)a)*((sexp_uint_t)b))
#define lsint_mul_sint(a, b) (((sexp_lsint_t)a)*((sexp_sint_t)b))
#define luint_div(a, b) (((sexp_luint_t)a)/((sexp_luint_t)b))
#define luint_div_uint(a, b) (((sexp_luint_t)a)/((sexp_luint_t)b))
#define luint_and(a, b) (((sexp_luint_t)a)&((sexp_luint_t)b))
#define luint_is_fixnum(x) (((sexp_luint_t)x)<=SEXP_MAX_FIXNUM)
#define lsint_is_fixnum(x) ((SEXP_MIN_FIXNUM <= ((sexp_lsint_t)x)) && (((sexp_lsint_t)x) <= SEXP_MAX_FIXNUM))

#else

static inline int lsint_lt_0(sexp_lsint_t a) {
  return a.hi < 0;
}

static inline int sexp_lsint_fits_sint(sexp_lsint_t x) {
  return x.hi == (((int64_t)x.lo)>>63) && ((sexp_sint_t)x.lo == x.lo);
}
static inline int sexp_luint_fits_uint(sexp_luint_t x) {
  return x.hi == 0 && ((sexp_uint_t)x.lo == x.lo);
}

static inline sexp_luint_t luint_from_lsint(sexp_lsint_t v) {
  sexp_luint_t result;
  result.hi = v.hi;
  result.lo = v.lo;
  return result;
}

static inline sexp_lsint_t lsint_from_luint(sexp_luint_t v) {
  sexp_lsint_t result;
  result.hi = v.hi;
  result.lo = v.lo;
  return result;
}

static inline sexp_lsint_t lsint_from_sint(sexp_sint_t v) {
  sexp_lsint_t result;
  result.hi = v >> 63;
  result.lo = v;
  return result;
}

static inline sexp_luint_t luint_from_uint(sexp_uint_t v) {
  sexp_luint_t result;
  result.hi = 0;
  result.lo = v;
  return result;
}

static inline sexp_sint_t lsint_to_sint(sexp_lsint_t v) {
  return v.lo;
}

static inline sexp_uint_t luint_to_uint(sexp_luint_t v) {
  return v.lo;
}

static inline sexp_sint_t lsint_to_sint_hi(sexp_lsint_t v) {
#if SEXP_64_BIT
  return v.hi;
#else
  return v.lo >> 32;
#endif
}

static inline sexp_uint_t luint_to_uint_hi(sexp_luint_t v) {
#if SEXP_64_BIT
  return v.hi;
#else
  return v.lo >> 32;
#endif
}

static inline sexp_lsint_t lsint_negate(sexp_lsint_t v) {
  sexp_luint_t a;
  a.hi = ~v.hi;
  a.lo = ~v.lo;

  uint64_t aLoLo = a.lo & 0xFFFFFFFF;
  uint64_t aLoHi = a.lo >> 32;
  uint64_t aHiLo = a.hi & 0xFFFFFFFF;
  uint64_t aHiHi = a.hi >> 32;

  uint64_t carry;
  uint64_t sumLoLo = aLoLo + 1;
  carry = sumLoLo >> 32;
  uint64_t resultLoLo = sumLoLo & 0xFFFFFFFF;

  uint64_t sumLoHi = aLoHi + carry;
  uint64_t resultLoHi = sumLoHi & 0xFFFFFFFF;
  carry = sumLoHi >> 32;

  uint64_t sumHiLo = aHiLo + carry;
  uint64_t resultHiLo = sumHiLo & 0xFFFFFFFF;
  carry = sumHiLo >> 32;

  uint64_t sumHiHi = aHiHi + carry;
  uint64_t resultHiHi = sumHiHi & 0xFFFFFFFF;
  /* carry = sumHiHi >> 32; */

  sexp_lsint_t result;
  result.hi = (resultHiHi << 32) | resultHiLo;
  result.lo = (resultLoHi << 32) | resultLoLo;
  return result;
}

static inline int luint_eq(sexp_luint_t a, sexp_luint_t b) {
  return (a.hi == b.hi) && (a.lo == b.lo);
}

static inline int luint_lt(sexp_luint_t a, sexp_luint_t b) {
  if (a.hi < b.hi)
    return 1;
  else if (a.hi > b.hi)
    return 0;
  else
    return a.lo < b.lo;
}

static inline sexp_luint_t luint_shl(sexp_luint_t v, size_t shift) {
  if (shift == 0)
    return v;
  sexp_luint_t result;
  if (shift >= 64) {
    result.hi = v.lo << (shift - 64);
    result.lo = 0;
  } else {
    result.hi = (v.hi << shift) | (v.lo >> (64-shift));
    result.lo = v.lo << shift;
  }
  return result;
}

static inline sexp_luint_t luint_shr(sexp_luint_t v, size_t shift) {
  if (shift == 0)
    return v;
  sexp_luint_t result;
  if (shift >= 64) {
    result.hi = 0;
    result.lo = v.hi >> (shift - 64);
  } else {
    result.hi = v.hi >> shift;
    result.lo = (v.lo >> shift) | (v.hi << (64-shift));
  }
  return result;
}

static inline sexp_luint_t luint_add(sexp_luint_t a, sexp_luint_t b) {
  uint64_t aLoLo = a.lo & 0xFFFFFFFF;
  uint64_t aLoHi = a.lo >> 32;
  uint64_t aHiLo = a.hi & 0xFFFFFFFF;
  uint64_t aHiHi = a.hi >> 32;
  uint64_t bLoLo = b.lo & 0xFFFFFFFF;
  uint64_t bLoHi = b.lo >> 32;
  uint64_t bHiLo = b.hi & 0xFFFFFFFF;
  uint64_t bHiHi = b.hi >> 32;

  uint64_t carry;
  uint64_t sumLoLo = (aLoLo + bLoLo);
  carry = sumLoLo >> 32;
  uint64_t resultLoLo = sumLoLo & 0xFFFFFFFF;

  uint64_t sumLoHi = (aLoHi + bLoHi) + carry;
  uint64_t resultLoHi = sumLoHi & 0xFFFFFFFF;
  carry = sumLoHi >> 32;

  uint64_t sumHiLo = (aHiLo + bHiLo) + carry;
  uint64_t resultHiLo = sumHiLo & 0xFFFFFFFF;
  carry = sumHiLo >> 32;

  uint64_t sumHiHi = (aHiHi + bHiHi) + carry;
  uint64_t resultHiHi = sumHiHi & 0xFFFFFFFF;
  /* carry = sumHiHi >> 32; */

  sexp_luint_t result;
  result.hi = (resultHiHi << 32) | resultHiLo;
  result.lo = (resultLoHi << 32) | resultLoLo;
  return result;
}

static inline sexp_luint_t luint_add_uint(sexp_luint_t a, sexp_uint_t b) {
  uint64_t aLoLo = a.lo & 0xFFFFFFFF;
  uint64_t aLoHi = a.lo >> 32;
  uint64_t aHiLo = a.hi & 0xFFFFFFFF;
  uint64_t aHiHi = a.hi >> 32;
  uint64_t bLoLo = b & 0xFFFFFFFF;
  uint64_t bLoHi = b >> 32;

  uint64_t carry;
  uint64_t sumLoLo = (aLoLo + bLoLo);
  carry = sumLoLo >> 32;
  uint64_t resultLoLo = sumLoLo & 0xFFFFFFFF;

  uint64_t sumLoHi = (aLoHi + bLoHi) + carry;
  uint64_t resultLoHi = sumLoHi & 0xFFFFFFFF;
  carry = sumLoHi >> 32;

  uint64_t sumHiLo = aHiLo + carry;
  uint64_t resultHiLo = sumHiLo & 0xFFFFFFFF;
  carry = sumHiLo >> 32;

  uint64_t sumHiHi = aHiHi + carry;
  uint64_t resultHiHi = sumHiHi & 0xFFFFFFFF;
  /* carry = sumHiHi >> 32; */

  sexp_luint_t result;
  result.hi = (resultHiHi << 32) | resultHiLo;
  result.lo = (resultLoHi << 32) | resultLoLo;
  return result;
}

static inline sexp_luint_t luint_sub(sexp_luint_t a, sexp_luint_t b) {
  sexp_luint_t negB;
  negB.hi = ~b.hi;
  negB.lo = ~b.lo;
  return luint_add(a, luint_add_uint(negB, 1));
}

static inline sexp_luint_t luint_mul_uint(sexp_luint_t a, sexp_uint_t b) {
  uint64_t aLoLo = a.lo & 0xFFFFFFFF;
  uint64_t aLoHi = a.lo >> 32;
  uint64_t aHiLo = a.hi & 0xFFFFFFFF;
  uint64_t aHiHi = a.hi >> 32;

  uint64_t bLo = b & 0xFFFFFFFF;
  uint64_t bHi = b >> 32;

  sexp_luint_t resultBLo, resultBHi;
  {
    sexp_luint_t prodLoLo;
    prodLoLo.hi = 0;
    prodLoLo.lo = aLoLo * bLo;

    sexp_luint_t prodLoHi;
    prodLoHi.hi = (aLoHi * bLo) >> 32;
    prodLoHi.lo = (aLoHi * bLo) << 32;

    sexp_luint_t prodHiLo;
    prodHiLo.hi = aHiLo * bLo;
    prodHiLo.lo = 0;

    sexp_luint_t prodHiHi;
    prodHiHi.hi = (aHiHi * bLo) << 32;
    prodHiHi.lo = 0;

    resultBLo = luint_add(luint_add(luint_add(prodLoLo, prodLoHi), prodHiLo), prodHiHi);
  }
  {
    sexp_luint_t prodLoLo;
    prodLoLo.hi = 0;
    prodLoLo.lo = aLoLo * bHi;

    sexp_luint_t prodLoHi;
    prodLoHi.hi = (aLoHi * bHi) >> 32;
    prodLoHi.lo = (aLoHi * bHi) << 32;

    sexp_luint_t prodHiLo;
    prodHiLo.hi = aHiLo * bHi;
    prodHiLo.lo = 0;

    sexp_luint_t prodHiHi;
    prodHiHi.hi = (aHiHi * bHi) << 32;
    prodHiHi.lo = 0;

    resultBHi = luint_add(luint_add(luint_add(prodLoLo, prodLoHi), prodHiLo), prodHiHi);
  }

  sexp_luint_t result = luint_add(resultBLo, luint_shl(resultBHi, 32));

  return result;
}

static inline sexp_lsint_t lsint_mul_sint(sexp_lsint_t a, sexp_sint_t b) {
  if (lsint_lt_0(a)) {
    sexp_luint_t minusA = luint_from_lsint(lsint_negate(a));
    if (b < 0)
      return lsint_from_luint(luint_mul_uint(minusA, (sexp_uint_t)-b));
    else
      return lsint_negate(lsint_from_luint(luint_mul_uint(minusA, (sexp_uint_t)b)));
  } else {
    if (b < 0)
      return lsint_negate(lsint_from_luint(luint_mul_uint(luint_from_lsint(a), (sexp_uint_t)-b)));
    else
      return lsint_from_luint(luint_mul_uint(luint_from_lsint(a), (sexp_uint_t)b));
  }
}

static inline sexp_luint_t luint_div(sexp_luint_t a, sexp_luint_t b) {
  if (luint_lt(a, b))
    return luint_from_uint(0);
  else if (luint_eq(a, b))
    return luint_from_uint(1);

  sexp_luint_t quotient = luint_from_uint(0);
  sexp_luint_t remainder = luint_from_uint(0);

  for (int i = 0; i < 128; i++) {
    quotient = luint_shl(quotient, 1);

    remainder = luint_shl(remainder, 1);
    remainder.lo |= (a.hi >> 63) & 1;
    a = luint_shl(a, 1);

    if (!(luint_lt(remainder, b))) {
      remainder = luint_sub(remainder, b);
      quotient.lo |= 1;
    }
  }

  return quotient;
}

static inline sexp_luint_t luint_div_uint(sexp_luint_t a, sexp_uint_t b) {
  return luint_div(a, luint_from_uint(b));
}

static inline sexp_luint_t luint_and(sexp_luint_t a, sexp_luint_t b) {
  sexp_luint_t result;
  result.hi = a.hi & b.hi;
  result.lo = a.lo & b.lo;
  return result;
}

static inline int luint_is_fixnum(sexp_luint_t x) {
  return (x.hi == 0) && (x.lo <= SEXP_MAX_FIXNUM);
}

static inline int lsint_is_fixnum(sexp_lsint_t x) {
  if (x.hi > 0)
      return 0;
  else if (x.hi == 0)
      return x.lo <= SEXP_MAX_FIXNUM;
  else if (x.hi == -1)
      return SEXP_MIN_FIXNUM <= x.lo;
  else return 0;
}

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
SEXP_API double sexp_to_double (sexp ctx, sexp x);
SEXP_API sexp sexp_bignum_fxadd (sexp ctx, sexp a, sexp_uint_t b);
SEXP_API sexp sexp_bignum_fxsub (sexp ctx, sexp a, sexp_uint_t b);
SEXP_API sexp sexp_bignum_fxmul (sexp ctx, sexp d, sexp a, sexp_uint_t b, int offset);
SEXP_API sexp_uint_t sexp_bignum_fxdiv (sexp ctx, sexp a, sexp_uint_t b, int offset);
SEXP_API sexp sexp_bignum_add (sexp ctx, sexp dst, sexp a, sexp b);
SEXP_API sexp sexp_bignum_sub (sexp ctx, sexp dst, sexp a, sexp b);
SEXP_API sexp sexp_bignum_mul (sexp ctx, sexp dst, sexp a, sexp b);
SEXP_API sexp sexp_bignum_div (sexp ctx, sexp dst, sexp a, sexp b);
SEXP_API sexp sexp_bignum_expt (sexp ctx, sexp n, sexp e);
SEXP_API sexp sexp_bignum_sqrt (sexp ctx, sexp a, sexp* rem);
SEXP_API sexp sexp_add (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_sub (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_mul (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_div (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_quotient (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_remainder (sexp ctx, sexp a, sexp b);
#if SEXP_USE_RATIOS
SEXP_API sexp sexp_double_to_ratio (sexp ctx, double f);
SEXP_API sexp sexp_double_to_ratio_2 (sexp ctx, double f);
SEXP_API double sexp_ratio_to_double (sexp ctx, sexp rat);
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

