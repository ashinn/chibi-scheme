/*  bignum.c -- bignum support                                */
/*  Copyright (c) 2009-2012 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/sexp.h"

#if SEXP_USE_BIGNUMS

#define SEXP_INIT_BIGNUM_SIZE 2

static int digit_value (int c) {
  return (((c)<='9') ? ((c) - '0') : ((sexp_toupper(c) - 'A') + 10));
}

static int hex_digit (int n) {
  return ((n<=9) ? ('0' + n) : ('A' + n - 10));
}

sexp sexp_make_bignum (sexp ctx, sexp_uint_t len) {
  sexp_uint_t size = sexp_sizeof(bignum) + len*sizeof(sexp_uint_t);
  sexp res = sexp_alloc_tagged(ctx, size, SEXP_BIGNUM);
  sexp_bignum_length(res) = len;
  sexp_bignum_sign(res) = 1;
  return res;
}

sexp sexp_fixnum_to_bignum (sexp ctx, sexp a) {
  sexp res = sexp_make_bignum(ctx, 1);
  sexp_bignum_data(res)[0] = sexp_unbox_fixnum(sexp_fx_abs(a));
  sexp_bignum_sign(res) = sexp_fx_sign(a);
  return res;
}

sexp sexp_make_integer (sexp ctx, sexp_lsint_t x) {
  sexp res;
  if ((SEXP_MIN_FIXNUM <= x) && (x <= SEXP_MAX_FIXNUM)) {
    res = sexp_make_fixnum(x);
  } else {
    res = sexp_make_bignum(ctx, 1);
    if (x < 0) {
      sexp_bignum_sign(res) = -1;
      sexp_bignum_data(res)[0] = -x;
    } else {
      sexp_bignum_sign(res) = 1;
      sexp_bignum_data(res)[0] = x;
    }
  }
  return res;
}

sexp sexp_make_unsigned_integer (sexp ctx, sexp_luint_t x) {
  sexp res;
  if (x <= SEXP_MAX_FIXNUM) {
    res = sexp_make_fixnum(x);
  } else {
    res = sexp_make_bignum(ctx, 1);
    sexp_bignum_sign(res) = 1;
    sexp_bignum_data(res)[0] = x;
  }
  return res;
}

#define double_trunc_10s_digit(f) (trunc((f)/10.0)*10.0)
#define double_10s_digit(f) ((f)-double_trunc_10s_digit(f))

sexp sexp_double_to_bignum (sexp ctx, double f) {
  int sign;
  sexp_gc_var3(res, scale, tmp);
  sexp_gc_preserve3(ctx, res, scale, tmp);
  res = sexp_fixnum_to_bignum(ctx, SEXP_ZERO);
  scale = sexp_fixnum_to_bignum(ctx, SEXP_ONE);
  sign = (f < 0 ? -1 : 1);
  for (f=fabs(f); f >= 1.0; f=trunc(f/10)) {
    tmp = sexp_bignum_fxmul(ctx, NULL, scale, double_10s_digit(f), 0);
    res = sexp_bignum_add(ctx, res, res, tmp);
    scale = sexp_bignum_fxmul(ctx, NULL, scale, 10, 0);
  }
  sexp_bignum_sign(res) = sign;
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_copy_bignum (sexp ctx, sexp dst, sexp a, sexp_uint_t len0) {
  sexp_uint_t len = (len0 > 0) ? len0 : sexp_bignum_length(a), size;
  size = sexp_sizeof(bignum) + len*sizeof(sexp_uint_t);
  if (! dst || sexp_bignum_length(dst) < len) {
    dst = sexp_alloc_tagged(ctx, size, SEXP_BIGNUM);
    memmove(dst, a, size);
    sexp_bignum_length(dst) = len;
  } else {
    memset(dst->value.bignum.data, 0,
           sexp_bignum_length(dst)*sizeof(sexp_uint_t));
    memmove(dst->value.bignum.data, a->value.bignum.data,
            sexp_bignum_length(a)*sizeof(sexp_uint_t));
  }
  return dst;
}

int sexp_bignum_zerop (sexp a) {
  int i;
  sexp_uint_t *data = sexp_bignum_data(a);
  for (i=sexp_bignum_length(a)-1; i>=0; i--)
    if (data[i])
      return 0;
  return 1;
}

sexp_uint_t sexp_bignum_hi (sexp a) {
  sexp_uint_t i=sexp_bignum_length(a)-1;
  while ((i>0) && ! sexp_bignum_data(a)[i])
    i--;
  return i+1;
}

sexp_sint_t sexp_bignum_compare_abs (sexp a, sexp b) {
  int ai=sexp_bignum_hi(a), bi=sexp_bignum_hi(b);
  sexp_uint_t *adata=sexp_bignum_data(a), *bdata=sexp_bignum_data(b);
  if (ai != bi)
    return ai - bi;
  for (--ai; ai >= 0; ai--) {
    if (adata[ai] > bdata[ai])
      return 1;
    else if (adata[ai] < bdata[ai])
      return -1;
  }
  return 0;
}

sexp_sint_t sexp_bignum_compare (sexp a, sexp b) {
  if (sexp_bignum_sign(a) != sexp_bignum_sign(b))
    return sexp_bignum_sign(a);
  return sexp_bignum_compare_abs(a, b);
}

sexp sexp_bignum_normalize (sexp a) {
  sexp_uint_t *data;
  if ((! sexp_bignump(a)) || (sexp_bignum_hi(a)>1))
    return a;
  data = sexp_bignum_data(a);
  if ((data[0] > SEXP_MAX_FIXNUM)
      && ! ((sexp_bignum_sign(a) == -1) && (data[0] == SEXP_MAX_FIXNUM+1)))
    return a;
  return sexp_make_fixnum((sexp_sint_t)data[0] * sexp_bignum_sign(a));
}

double sexp_bignum_to_double (sexp a) {
  double res = 0;
  sexp_sint_t i;
  sexp_uint_t *data=sexp_bignum_data(a);
  for (i=sexp_bignum_hi(a)-1; i>=0; i--)
    res = res * ((double)SEXP_UINT_T_MAX+1) + data[i];
  return res * sexp_bignum_sign(a);
}

sexp sexp_bignum_fxadd (sexp ctx, sexp a, sexp_uint_t b) {
  sexp_uint_t len=sexp_bignum_hi(a), *data=sexp_bignum_data(a),
    carry=b, i=0, n;
  do { n = data[i];
       data[i] += carry;
       carry = (n > (SEXP_UINT_T_MAX - carry));
  } while (++i<len && carry);
  if (carry) {
    a = sexp_copy_bignum(ctx, NULL, a, len+1);
    sexp_bignum_data(a)[len] = 1;
  }
  return a;
}

sexp sexp_bignum_fxsub (sexp ctx, sexp a, sexp_uint_t b) {
  sexp_uint_t *data=sexp_bignum_data(a), borrow, i=0, n;
  for (borrow=b; borrow; i++) {
    n = data[i];
    data[i] -= borrow;
    borrow = (n < borrow);
  }
  return a;
}

sexp sexp_bignum_fxmul (sexp ctx, sexp d, sexp a, sexp_uint_t b, int offset) {
  sexp_uint_t len=sexp_bignum_length(a), *data, *adata=sexp_bignum_data(a),
    carry=0, i;
  sexp_luint_t n;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  if ((! d) || (sexp_bignum_length(d)+offset < len))
    d = tmp = sexp_make_bignum(ctx, len);
  data = sexp_bignum_data(d);
  for (i=0; i<len; i++) {
    n = (sexp_luint_t)adata[i]*b + carry;
    data[i+offset] = (sexp_uint_t)n;
    carry = n >> (sizeof(sexp_uint_t)*8);
  }
  if (carry) {
    if (sexp_bignum_length(d)+offset <= len)
      d = sexp_copy_bignum(ctx, NULL, d, len+offset+1);
    sexp_bignum_data(d)[len+offset] = carry;
  }
  sexp_gc_release1(ctx);
  return d;
}

sexp_uint_t sexp_bignum_fxdiv (sexp ctx, sexp a, sexp_uint_t b, int offset) {
  sexp_uint_t len=sexp_bignum_hi(a), *data=sexp_bignum_data(a), q, r=0;
  int i;
  sexp_luint_t n = 0;
  for (i=len-1; i>=offset; i--) {
    n = (n << sizeof(sexp_uint_t)*8) + data[i];
    q = n / b;
    r = n - (sexp_luint_t)q * b;
    data[i] = q;
    n = r;
  }
  return r;
}

sexp sexp_read_bignum (sexp ctx, sexp in, sexp_uint_t init,
                       signed char sign, sexp_uint_t base) {
  int c, digit;
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_make_bignum(ctx, SEXP_INIT_BIGNUM_SIZE);
  sexp_bignum_sign(res) = sign;
  sexp_bignum_data(res)[0] = init;
  for (c=sexp_read_char(ctx, in); sexp_isxdigit(c); c=sexp_read_char(ctx, in)) {
    digit = digit_value(c);
    if ((digit < 0) || (digit >= base))
      break;
    res = sexp_bignum_fxmul(ctx, res, res, base, 0);
    res = sexp_bignum_fxadd(ctx, res, digit);
  }
  if (c=='.' || c=='e' || c=='E') {
    if (base != 10) {
      res = sexp_read_error(ctx, "found non-base 10 float", SEXP_NULL, in);
    } else {
      if (c!='.') sexp_push_char(ctx, c, in); /* push the e back */
      res = sexp_read_float_tail(ctx, in, sexp_bignum_to_double(res), (sign==-1));
    }
#if SEXP_USE_RATIOS
  } else if (c=='/') {
    res = sexp_bignum_normalize(res);
    res = sexp_make_ratio(ctx, res, SEXP_ONE);
    sexp_ratio_denominator(res) = sexp_read_number(ctx, in, 10);
    res = sexp_ratio_normalize(ctx, res, in);
#endif
#if SEXP_USE_COMPLEX
  } else if (c=='i' || c=='i' || c=='+' || c=='-') {
    sexp_push_char(ctx, c, in);
    res = sexp_bignum_normalize(res);
    res = sexp_read_complex_tail(ctx, in, res);
#endif
  } else if ((c!=EOF) && ! sexp_is_separator(c)) {
    res = sexp_read_error(ctx, "invalid numeric syntax",
                          sexp_make_character(c), in);
  } else {
    sexp_push_char(ctx, c, in);
  }
  sexp_gc_release1(ctx);
  return sexp_bignum_normalize(res);
}

static int log2i(int v) {
  int i;
  for (i = 0; i < sizeof(v)*8; i++)
    if ((1<<(i+1)) > v)
      break;
  return i;
}

sexp sexp_write_bignum (sexp ctx, sexp a, sexp out, sexp_uint_t base) {
  int i, str_len, lg_base = log2i(base);
  char *data;
  sexp_gc_var2(b, str);
  sexp_gc_preserve2(ctx, b, str);
  b = sexp_copy_bignum(ctx, NULL, a, 0);
  sexp_bignum_sign(b) = 1;
  i = str_len = (sexp_bignum_length(b)*sizeof(sexp_uint_t)*8 + lg_base - 1)
    / lg_base + 1;
  str = sexp_make_string(ctx, sexp_make_fixnum(str_len),
                         sexp_make_character(' '));
  data = sexp_string_data(str);
  while (! sexp_bignum_zerop(b))
    data[--i] = hex_digit(sexp_bignum_fxdiv(ctx, b, base, 0));
  if (i == str_len)
    data[--i] = '0';
  else if (sexp_bignum_sign(a) == -1)
    data[--i] = '-';
  sexp_write_string(ctx, data + i, out);
  sexp_gc_release2(ctx);
  return SEXP_VOID;
}

/****************** bignum arithmetic *************************/

sexp sexp_bignum_add_fixnum (sexp ctx, sexp a, sexp b) {
  sexp_gc_var1(c);
  sexp_gc_preserve1(ctx, c);
  c = sexp_copy_bignum(ctx, NULL, a, 0);
  if (sexp_bignum_sign(c) == sexp_fx_sign(b))
    c = sexp_bignum_fxadd(ctx, c, sexp_unbox_fixnum(sexp_fx_abs(b)));
  else
    c = sexp_bignum_fxsub(ctx, c, sexp_unbox_fixnum(sexp_fx_abs(b)));
  sexp_gc_release1(ctx);
  return c;
}

sexp sexp_bignum_sub_digits (sexp ctx, sexp dst, sexp a, sexp b) {
  sexp_uint_t alen=sexp_bignum_hi(a), blen=sexp_bignum_hi(b),
    borrow=0, i, *adata, *bdata, *cdata;
  sexp_gc_var1(c);
  if ((alen < blen) || ((alen == blen) && (sexp_bignum_compare_abs(a, b) < 0)))
    return sexp_bignum_sub_digits(ctx, dst, b, a);
  sexp_gc_preserve1(ctx, c);
  c = ((dst && sexp_bignum_hi(dst) >= alen)
       ? dst : sexp_copy_bignum(ctx, NULL, a, 0));
  adata = sexp_bignum_data(a);
  bdata = sexp_bignum_data(b);
  cdata = sexp_bignum_data(c);
  for (i=0; i<blen; i++) {
    cdata[i] = adata[i] - bdata[i] - borrow;
    borrow = (adata[i] < bdata[i] ? 1 : 0);
  }
  for ( ; borrow && (i<alen); i++) {
    borrow = (cdata[i] == 0 ? 1 : 0);
    cdata[i]--;
  }
  sexp_gc_release1(ctx);
  return c;
}

sexp sexp_bignum_add_digits (sexp ctx, sexp dst, sexp a, sexp b) {
  sexp_uint_t alen=sexp_bignum_hi(a), blen=sexp_bignum_hi(b),
    carry=0, i, n, *adata, *bdata, *cdata;
  sexp_gc_var1(c);
  if (alen < blen) return sexp_bignum_add_digits(ctx, dst, b, a);
  sexp_gc_preserve1(ctx, c);
  c = ((dst && sexp_bignum_hi(dst) >= alen)
       ? dst : sexp_copy_bignum(ctx, NULL, a, 0));
  adata = sexp_bignum_data(a);
  bdata = sexp_bignum_data(b);
  cdata = sexp_bignum_data(c);
  for (i=0; i<blen; i++) {
    n = adata[i];
    cdata[i] = n + bdata[i] + carry;
    carry = (n > (SEXP_UINT_T_MAX - bdata[i]) ? 1 : 0);
  }
  for ( ; carry && (i<alen); i++) {
    carry = (cdata[i] == SEXP_UINT_T_MAX-1 ? 1 : 0);
    cdata[i]++;
  }
  if (carry) {
    c = sexp_copy_bignum(ctx, NULL, c, alen+1);
    sexp_bignum_data(c)[alen] = 1;
  }
  sexp_gc_release1(ctx);
  return c;
}

sexp sexp_bignum_add (sexp ctx, sexp dst, sexp a, sexp b) {
  sexp res;
  if (sexp_bignum_sign(a) == sexp_bignum_sign(b)) {
    res = sexp_bignum_add_digits(ctx, dst, a, b);
    sexp_bignum_sign(res) = sexp_bignum_sign(a);
  } else {
    res = sexp_bignum_sub_digits(ctx, dst, a, b);
    sexp_bignum_sign(res)
      = sexp_bignum_sign(sexp_bignum_compare_abs(a, b) >= 0 ? a : b);
  }
  return res;
}

sexp sexp_bignum_sub (sexp ctx, sexp dst, sexp a, sexp b) {
  sexp res;
  if (sexp_bignum_sign(a) == sexp_bignum_sign(b)) {
    res = sexp_bignum_sub_digits(ctx, dst, a, b);
    sexp_bignum_sign(res)
      = (sexp_bignum_compare_abs(a, b) >= 0 ? sexp_bignum_sign(a)
         : -sexp_bignum_sign(a));
  } else {
    res = sexp_bignum_add_digits(ctx, dst, a, b);
    sexp_bignum_sign(res) = sexp_bignum_sign(a);
  }
  return res;
}

sexp sexp_bignum_mul (sexp ctx, sexp dst, sexp a, sexp b) {
  sexp_uint_t alen=sexp_bignum_hi(a), blen=sexp_bignum_hi(b), i,
    *bdata=sexp_bignum_data(b);
  sexp_gc_var2(c, d);
  if (alen < blen) return sexp_bignum_mul(ctx, dst, b, a);
  sexp_gc_preserve2(ctx, c, d);
  c = (dst ? dst : sexp_make_bignum(ctx, alen+blen+1));
  d = sexp_make_bignum(ctx, alen+blen+1);
  for (i=0; i<blen; i++) {
    d = sexp_bignum_fxmul(ctx, d, a, bdata[i], i);
    c = sexp_bignum_add_digits(ctx, NULL, c, d);
    sexp_bignum_data(d)[i] = 0;
  }
  sexp_bignum_sign(c) = sexp_bignum_sign(a) * sexp_bignum_sign(b);
  sexp_gc_release2(ctx);
  return c;
}

static sexp sexp_bignum_double (sexp ctx, sexp a) {
  return sexp_bignum_fxmul(ctx, NULL, a, 2, 0);
}

static sexp quot_step (sexp ctx, sexp *rem, sexp a, sexp b, sexp k, sexp i) {
  sexp res;
  sexp_gc_var5(x, prod, diff, k2, i2);
  if (sexp_bignum_compare(k, a) > 0) {
    *rem = a;
    return sexp_fixnum_to_bignum(ctx, SEXP_ZERO);
  }
  sexp_gc_preserve5(ctx, x, prod, diff, k2, i2);
  k2 = sexp_bignum_double(ctx, k);
  i2 = sexp_bignum_double(ctx, i);
  x = quot_step(ctx, rem, a, b, k2, i2);
  prod = sexp_bignum_mul(ctx, NULL, x, b);
  diff = sexp_bignum_sub_digits(ctx, NULL, a, prod);
  if (sexp_bignum_compare(diff, k) >= 0) {
    *rem = sexp_bignum_sub_digits(ctx, NULL, diff, k);
    res = sexp_bignum_add_digits(ctx, NULL, x, i);
  } else {
    *rem = diff;
    res = x;
  }
  sexp_gc_release5(ctx);
  return res;
}

sexp sexp_bignum_quot_rem (sexp ctx, sexp *rem, sexp a, sexp b) {
  sexp res;
  sexp_gc_var4(k, i, a1, b1);
  sexp_gc_preserve4(ctx, k, i, a1, b1);
  a1 = sexp_copy_bignum(ctx, NULL, a, 0);
  sexp_bignum_sign(a1) = 1;
  b1 = sexp_copy_bignum(ctx, NULL, b, 0);
  sexp_bignum_sign(b1) = 1;
  k = sexp_copy_bignum(ctx, NULL, b1, 0);
  i = sexp_fixnum_to_bignum(ctx, SEXP_ONE);
  res = quot_step(ctx, rem, a1, b1, k, i);
  sexp_bignum_sign(res) = sexp_bignum_sign(a) * sexp_bignum_sign(b);
  if (sexp_bignum_sign(a) < 0) {
    sexp_negate_exact(*rem);
  }
  sexp_gc_release4(ctx);
  return res;
}

sexp sexp_bignum_quotient (sexp ctx, sexp a, sexp b) {
  sexp res;
  sexp_gc_var1(rem);
  sexp_gc_preserve1(ctx, rem);
  res = sexp_bignum_quot_rem(ctx, &rem, a, b);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_bignum_remainder (sexp ctx, sexp a, sexp b) {
  sexp_gc_var1(rem);
  sexp_gc_preserve1(ctx, rem);
  sexp_bignum_quot_rem(ctx, &rem, a, b); /* discard quotient */
  sexp_gc_release1(ctx);
  return rem;
}

sexp sexp_bignum_expt (sexp ctx, sexp a, sexp b) {
  sexp_sint_t e = sexp_unbox_fixnum(sexp_fx_abs(b));
  sexp_gc_var2(res, acc);
  sexp_gc_preserve2(ctx, res, acc);
  res = sexp_fixnum_to_bignum(ctx, SEXP_ONE);
  acc = sexp_copy_bignum(ctx, NULL, a, 0);
  for (; e; e>>=1, acc=sexp_bignum_mul(ctx, NULL, acc, acc))
    if (e & 1)
      res = sexp_bignum_mul(ctx, NULL, res, acc);
  sexp_gc_release2(ctx);
  return sexp_bignum_normalize(res);
}

/************************ ratios ******************************/

#if SEXP_USE_RATIOS

double sexp_ratio_to_double (sexp rat) {
  sexp num = sexp_ratio_numerator(rat), den = sexp_ratio_denominator(rat);
  return (sexp_bignump(num) ? sexp_bignum_to_double(num)
          : sexp_fixnum_to_double(num))
    / (sexp_bignump(den) ? sexp_bignum_to_double(den)
       : sexp_fixnum_to_double(den));
}

sexp sexp_double_to_ratio (sexp ctx, double f) {
  int sign, i;
  sexp_gc_var3(res, whole, scale);
  if (f == trunc(f))
    return sexp_bignum_normalize(sexp_double_to_bignum(ctx, f));
  sexp_gc_preserve3(ctx, res, whole, scale);
  whole = sexp_double_to_bignum(ctx, trunc(f));
  res = sexp_fixnum_to_bignum(ctx, SEXP_ZERO);
  scale = SEXP_ONE;
  sign = (f < 0 ? -1 : 1);
  for (i=0, f=fabs(f-trunc(f)); f != trunc(f) && i < 15; i++) {
    res = sexp_bignum_fxmul(ctx, NULL, res, 10, 0);
    f = f * 10;
    res = sexp_bignum_fxadd(ctx, res, double_10s_digit(f));
    f = f - trunc(f);
    scale = sexp_mul(ctx, scale, SEXP_TEN);
  }
  sexp_bignum_sign(res) = sign;
  res = sexp_bignum_normalize(res);
  scale = sexp_bignum_normalize(scale);
  res = sexp_make_ratio(ctx, res, scale);
  res = sexp_ratio_normalize(ctx, res, SEXP_FALSE);
  res = sexp_add(ctx, res, whole);
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_ratio_add (sexp ctx, sexp a, sexp b) {
  sexp_gc_var3(res, num, den);
  sexp_gc_preserve3(ctx, res, num, den);
  num = sexp_mul(ctx, sexp_ratio_numerator(a), sexp_ratio_denominator(b));
  den = sexp_mul(ctx, sexp_ratio_numerator(b), sexp_ratio_denominator(a));
  num = sexp_add(ctx, num, den);
  den = sexp_mul(ctx, sexp_ratio_denominator(a), sexp_ratio_denominator(b));
  res = sexp_make_ratio(ctx, num, den);
  sexp_gc_release3(ctx);
  return sexp_ratio_normalize(ctx, res, SEXP_FALSE);
}

sexp sexp_ratio_mul (sexp ctx, sexp a, sexp b) {
  sexp_gc_var3(res, num, den);
  sexp_gc_preserve3(ctx, res, num, den);
  num = sexp_mul(ctx, sexp_ratio_numerator(a), sexp_ratio_numerator(b));
  den = sexp_mul(ctx, sexp_ratio_denominator(a), sexp_ratio_denominator(b));
  res = sexp_make_ratio(ctx, num, den);
  sexp_gc_release3(ctx);
  return sexp_ratio_normalize(ctx, res, SEXP_FALSE);
}

sexp sexp_ratio_div (sexp ctx, sexp a, sexp b) {
  sexp_gc_var3(res, num, den);
  sexp_gc_preserve3(ctx, res, num, den);
  num = sexp_mul(ctx, sexp_ratio_numerator(a), sexp_ratio_denominator(b));
  den = sexp_mul(ctx, sexp_ratio_denominator(a), sexp_ratio_numerator(b));
  res = sexp_make_ratio(ctx, num, den);
  sexp_gc_release3(ctx);
  return sexp_ratio_normalize(ctx, res, SEXP_FALSE);
}

sexp sexp_ratio_compare (sexp ctx, sexp a, sexp b) {
  sexp_gc_var2(a2, b2);
  sexp_gc_preserve2(ctx, a2, b2);
  a2 = sexp_mul(ctx, sexp_ratio_numerator(a), sexp_ratio_denominator(b));
  b2 = sexp_mul(ctx, sexp_ratio_numerator(b), sexp_ratio_denominator(a));
  a2 = sexp_compare(ctx, a2, b2);
  sexp_gc_release2(ctx);
  return a2;
}

sexp sexp_ratio_round (sexp ctx, sexp a) {
  sexp_gc_var2(q, r);
  sexp_gc_preserve2(ctx, q, r);
  q = sexp_quotient(ctx, sexp_ratio_numerator(a), sexp_ratio_denominator(a));
  if ((sexp_ratio_denominator(a) == SEXP_TWO) && sexp_oddp(q)) {
    q = sexp_add(ctx, q, (sexp_positivep(q) ? SEXP_ONE : SEXP_NEG_ONE));
  } else {
    r = sexp_remainder(ctx, sexp_ratio_numerator(a), sexp_ratio_denominator(a));
    r = sexp_mul(ctx, r, SEXP_TWO);
    if (sexp_negativep(r)) {sexp_negate(r);}
    if (sexp_unbox_fixnum(sexp_compare(ctx, r, sexp_ratio_denominator(a))) > 0)
      q = sexp_add(ctx, q, (sexp_positivep(q) ? SEXP_ONE : SEXP_NEG_ONE));
  }
  sexp_gc_release2(ctx);
  return q;
}

sexp sexp_ratio_trunc (sexp ctx, sexp a) {
  return sexp_quotient(ctx, sexp_ratio_numerator(a), sexp_ratio_denominator(a));
}

sexp sexp_ratio_floor (sexp ctx, sexp a) {
  sexp_gc_var1(q);
  sexp_gc_preserve1(ctx, q);
  q = sexp_quotient(ctx, sexp_ratio_numerator(a), sexp_ratio_denominator(a));
  if (sexp_negativep(sexp_ratio_numerator(a)))
    q = sexp_add(ctx, q, SEXP_NEG_ONE);
  sexp_gc_release1(ctx);
  return q;
}

sexp sexp_ratio_ceiling (sexp ctx, sexp a) {
  sexp_gc_var1(q);
  sexp_gc_preserve1(ctx, q);
  q = sexp_quotient(ctx, sexp_ratio_numerator(a), sexp_ratio_denominator(a));
  if (sexp_positivep(sexp_ratio_numerator(a)))
    q = sexp_add(ctx, q, SEXP_ONE);
  sexp_gc_release1(ctx);
  return q;
}

#endif

/************************ complex numbers ****************************/

#if SEXP_USE_COMPLEX

sexp sexp_complex_add (sexp ctx, sexp a, sexp b) {
  sexp_gc_var3(res, real, imag);
  sexp_gc_preserve3(ctx, res, real, imag);
  real = sexp_add(ctx, sexp_complex_real(a), sexp_complex_real(b));
  imag = sexp_add(ctx, sexp_complex_imag(a), sexp_complex_imag(b));
  res = sexp_make_complex(ctx, real, imag);
  sexp_gc_release3(ctx);
  return sexp_complex_normalize(res);
}

sexp sexp_complex_sub (sexp ctx, sexp a, sexp b) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  tmp = sexp_make_complex(ctx, sexp_complex_real(b), sexp_complex_imag(b));
  sexp_negate(sexp_complex_real(tmp));
  sexp_negate(sexp_complex_imag(tmp));
  res = sexp_complex_add(ctx, a, tmp);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_complex_mul (sexp ctx, sexp a, sexp b) {
  sexp_gc_var3(res, real, imag);
  sexp_gc_preserve3(ctx, res, real, imag);
  real = sexp_mul(ctx, sexp_complex_real(a), sexp_complex_real(b));
  res = sexp_mul(ctx, sexp_complex_imag(a), sexp_complex_imag(b));
  real = sexp_sub(ctx, real, res);
  imag = sexp_mul(ctx, sexp_complex_real(a), sexp_complex_imag(b));
  res = sexp_mul(ctx, sexp_complex_imag(a), sexp_complex_real(b));
  imag = sexp_add(ctx, imag, res);
  res = sexp_make_complex(ctx, real, imag);
  sexp_gc_release3(ctx);
  return sexp_complex_normalize(res);
}

/* (a + bi)    (ac + bd)      (bc - ad)    */
/* -------- = -----------  + ----------- i */
/* (c + di)   (c^2 + d^2)    (c^2 + d^2)   */

sexp sexp_complex_div (sexp ctx, sexp a, sexp b) {
  sexp_gc_var4(res, real, imag, denom);
  sexp_gc_preserve4(ctx, res, real, imag, denom);
  /* c^2 + d^2 */
  denom = sexp_mul(ctx, sexp_complex_real(b), sexp_complex_real(b));
  res = sexp_mul(ctx, sexp_complex_imag(b), sexp_complex_imag(b));
  denom = sexp_add(ctx, denom, res);
  /* ac + bd */
  real = sexp_mul(ctx, sexp_complex_real(a), sexp_complex_real(b));
  res = sexp_mul(ctx, sexp_complex_imag(a), sexp_complex_imag(b));
  real = sexp_add(ctx, real, res);
  real = sexp_div(ctx, real, denom);
  /* bc - ad */
  imag = sexp_mul(ctx, sexp_complex_imag(a), sexp_complex_real(b));
  res = sexp_mul(ctx, sexp_complex_real(a), sexp_complex_imag(b));
  imag = sexp_sub(ctx, imag, res);
  imag = sexp_div(ctx, imag, denom);
  res = sexp_make_complex(ctx, real, imag);
  sexp_gc_release4(ctx);
  return sexp_complex_normalize(res);
}

static double sexp_to_double (sexp x) {
  if (sexp_flonump(x))
    return sexp_flonum_value(x);
  else if (sexp_fixnump(x))
    return sexp_fixnum_to_double(x);
  else if (sexp_bignump(x))
    return sexp_bignum_to_double(x);
#if SEXP_USE_RATIOS
  else if (sexp_ratiop(x))
    return sexp_ratio_to_double(x);
#endif
  else
    return 0.0;
}

static sexp sexp_to_complex (sexp ctx, sexp x) {
#if SEXP_USE_RATIOS
  sexp_gc_var1(tmp);
#endif
  if (sexp_flonump(x) || sexp_fixnump(x) || sexp_bignump(x)) {
    return sexp_make_complex(ctx, x, SEXP_ZERO);
#if SEXP_USE_RATIOS
  } else if (sexp_ratiop(x)) {
    sexp_gc_preserve1(ctx, tmp);
    tmp = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
    sexp_complex_real(tmp) = sexp_make_flonum(ctx, sexp_to_double(x));
    sexp_gc_release1(ctx);
    return tmp;
#endif
  } else {
    return x;
  }
}

sexp sexp_complex_exp (sexp ctx, sexp z) {
  double e2x = exp(sexp_to_double(sexp_complex_real(z))),
    y = sexp_to_double(sexp_complex_imag(z));
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
  sexp_complex_real(res) = sexp_make_flonum(ctx, e2x*cos(y));
  sexp_complex_imag(res) = sexp_make_flonum(ctx, e2x*sin(y));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_complex_log (sexp ctx, sexp z) {
  double x = sexp_to_double(sexp_complex_real(z)),
    y = sexp_to_double(sexp_complex_imag(z));
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
  sexp_complex_real(res) = sexp_make_flonum(ctx, log(sqrt(x*x + y*y)));
  sexp_complex_imag(res) = sexp_make_flonum(ctx, atan2(y, x));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_complex_expt (sexp ctx, sexp a, sexp b) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_to_complex(ctx, a);
  res = sexp_complex_log(ctx, res);
  res = sexp_mul(ctx, b, res);
  res = sexp_complex_exp(ctx, res);
  sexp_gc_release1(ctx);
  return res;
}

#if SEXP_USE_MATH

sexp sexp_complex_sqrt (sexp ctx, sexp z) {
  double x = sexp_to_double(sexp_complex_real(z)),
    y = sexp_to_double(sexp_complex_imag(z)), r;
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  r = sqrt(x*x + y*y);
  res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
  sexp_complex_real(res) = sexp_make_flonum(ctx, sqrt((x+r)/2));
  sexp_complex_imag(res) = sexp_make_flonum(ctx, (y<0?-1:1)*sqrt((-x+r)/2));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_complex_sin (sexp ctx, sexp z) {
  double x = sexp_to_double(sexp_complex_real(z)),
    y = sexp_to_double(sexp_complex_imag(z));
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
  sexp_complex_real(res) = sexp_make_flonum(ctx, sin(x)*cosh(y));
  sexp_complex_imag(res) = sexp_make_flonum(ctx, cos(x)*sinh(y));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_complex_cos (sexp ctx, sexp z) {
  double x = sexp_to_double(sexp_complex_real(z)),
    y = sexp_to_double(sexp_complex_imag(z));
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
  sexp_complex_real(res) = sexp_make_flonum(ctx, cos(x)*cosh(y));
  sexp_complex_imag(res) = sexp_make_flonum(ctx, -sin(x)*sinh(y));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_complex_tan (sexp ctx, sexp z) {
  sexp res;
  sexp_gc_var2(sin, cos);
  sexp_gc_preserve2(ctx, sin, cos);
  sin = sexp_complex_sin(ctx, z);
  cos = sexp_complex_cos(ctx, z);
  res = sexp_complex_div(ctx, sin, cos);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_complex_asin (sexp ctx, sexp z) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  res = sexp_complex_mul(ctx, z, z);
  tmp = sexp_make_complex(ctx, SEXP_ONE, SEXP_ZERO);
  res = sexp_complex_sub(ctx, tmp, res);
  res = sexp_complex_sqrt(ctx, res);
  /* tmp = iz */
  sexp_complex_real(tmp) = sexp_complex_imag(z);
  sexp_negate(sexp_complex_real(tmp));
  sexp_complex_imag(tmp) = sexp_complex_real(z);
  res = sexp_complex_add(ctx, tmp, res);
  tmp = sexp_complex_log(ctx, res);
  /* res = -i*tmp */
  sexp_complex_real(res) = sexp_complex_imag(tmp);
  sexp_complex_imag(res) = sexp_complex_real(tmp);
  sexp_negate(sexp_complex_imag(res));
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_complex_acos (sexp ctx, sexp z) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  res = sexp_complex_asin(ctx, z);
  tmp = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
  sexp_complex_real(tmp) = sexp_make_flonum(ctx, acos(-1)/2);
  res = sexp_sub(ctx, tmp, res);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_complex_atan (sexp ctx, sexp z) {
  sexp_gc_var3(res, tmp1, tmp2);
  sexp_gc_preserve3(ctx, res, tmp1, tmp2);
  tmp1 = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ONE);
  tmp1 = sexp_complex_mul(ctx, z, tmp1);
  res = sexp_make_complex(ctx, SEXP_ONE, SEXP_ZERO);
  res = sexp_complex_sub(ctx, res, tmp1);
  res = sexp_complex_log(ctx, res);
  tmp2 = sexp_make_complex(ctx, SEXP_ONE, SEXP_ZERO);
  tmp2 = sexp_complex_add(ctx, tmp2, tmp1);
  tmp2 = sexp_complex_log(ctx, tmp2);
  res = sexp_complex_sub(ctx, res, tmp2);
  tmp1 = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ONE);
  sexp_complex_imag(tmp1) = sexp_make_flonum(ctx, 0.5);
  res = sexp_complex_mul(ctx, res, tmp1);
  sexp_gc_release3(ctx);
  return res;
}

#endif
#endif

/****************** generic arithmetic ************************/

enum sexp_number_types {
  SEXP_NUM_NOT = 0,
  SEXP_NUM_FIX,
  SEXP_NUM_FLO,
  SEXP_NUM_BIG,
#if SEXP_USE_RATIOS
  SEXP_NUM_RAT,
#endif
#if SEXP_USE_COMPLEX
  SEXP_NUM_CPX,
#endif
};

enum sexp_number_combs {
  SEXP_NUM_NOT_NOT = 0,
  SEXP_NUM_NOT_FIX,
  SEXP_NUM_NOT_FLO,
  SEXP_NUM_NOT_BIG,
#if SEXP_USE_RATIOS
  SEXP_NUM_NOT_RAT,
#endif
#if SEXP_USE_COMPLEX
  SEXP_NUM_NOT_CPX,
#endif
  SEXP_NUM_FIX_NOT,
  SEXP_NUM_FIX_FIX,
  SEXP_NUM_FIX_FLO,
  SEXP_NUM_FIX_BIG,
#if SEXP_USE_RATIOS
  SEXP_NUM_FIX_RAT,
#endif
#if SEXP_USE_COMPLEX
  SEXP_NUM_FIX_CPX,
#endif
  SEXP_NUM_FLO_NOT,
  SEXP_NUM_FLO_FIX,
  SEXP_NUM_FLO_FLO,
  SEXP_NUM_FLO_BIG,
#if SEXP_USE_RATIOS
  SEXP_NUM_FLO_RAT,
#endif
#if SEXP_USE_COMPLEX
  SEXP_NUM_FLO_CPX,
#endif
  SEXP_NUM_BIG_NOT,
  SEXP_NUM_BIG_FIX,
  SEXP_NUM_BIG_FLO,
  SEXP_NUM_BIG_BIG,
#if SEXP_USE_RATIOS
  SEXP_NUM_BIG_RAT,
#endif
#if SEXP_USE_COMPLEX
  SEXP_NUM_BIG_CPX,
#endif
#if SEXP_USE_RATIOS
  SEXP_NUM_RAT_NOT,
  SEXP_NUM_RAT_FIX,
  SEXP_NUM_RAT_FLO,
  SEXP_NUM_RAT_BIG,
  SEXP_NUM_RAT_RAT,
#if SEXP_USE_COMPLEX
  SEXP_NUM_RAT_CPX,
#endif
#endif
#if SEXP_USE_COMPLEX
  SEXP_NUM_CPX_NOT,
  SEXP_NUM_CPX_FIX,
  SEXP_NUM_CPX_FLO,
  SEXP_NUM_CPX_BIG,
#if SEXP_USE_RATIOS
  SEXP_NUM_CPX_RAT,
#endif
  SEXP_NUM_CPX_CPX,
#endif
};

static int sexp_number_types[] =
#if SEXP_USE_RATIOS && SEXP_USE_COMPLEX
  {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 4, 5, 0, 0};
#else
#if SEXP_USE_RATIOS || SEXP_USE_COMPLEX
  {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 0, 0};
#else
  {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 0, 0, 0, 0};
#endif
#endif

#define SEXP_NUM_NUMBER_TYPES (4 + SEXP_USE_RATIOS + SEXP_USE_COMPLEX)

static int sexp_number_type (sexp a) {
  return sexp_pointerp(a) ?
    (sexp_pointer_tag(a)<(sizeof(sexp_number_types)/sizeof(sexp_number_types[0]))
     ? sexp_number_types[sexp_pointer_tag(a)] : 0)
#if SEXP_USE_IMMEDIATE_FLONUMS
    : sexp_flonump(a) ? 2
#endif
    : sexp_fixnump(a);
}

sexp sexp_add (sexp ctx, sexp a, sexp b) {
  sexp_sint_t sum;
  int at=sexp_number_type(a), bt=sexp_number_type(b), t;
  sexp r=SEXP_VOID;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  if (at > bt) {r=a; a=b; b=r; t=at; at=bt; bt=t;}
  switch ((at * SEXP_NUM_NUMBER_TYPES) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
#if SEXP_USE_RATIOS
  case SEXP_NUM_NOT_RAT:
#endif
#if SEXP_USE_COMPLEX
  case SEXP_NUM_NOT_CPX:
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_NUMBER, a);
    break;
  case SEXP_NUM_FIX_FIX:
    sum = sexp_unbox_fixnum(a) + sexp_unbox_fixnum(b);
    if ((sum < SEXP_MIN_FIXNUM) || (sum > SEXP_MAX_FIXNUM))
      r = sexp_add(ctx, tmp=sexp_fixnum_to_bignum(ctx, a), b);
    else
      r = sexp_make_fixnum(sum);
    break;
  case SEXP_NUM_FIX_FLO:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(a)+sexp_flonum_value(b));
    break;
  case SEXP_NUM_FIX_BIG:
    r = sexp_bignum_normalize(sexp_bignum_add_fixnum(ctx, b, a));
    break;
  case SEXP_NUM_FLO_FLO:
    r = sexp_fp_add(ctx, a, b);
    break;
  case SEXP_NUM_FLO_BIG:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) + sexp_bignum_to_double(b));
    break;
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_normalize(sexp_bignum_add(ctx, NULL, b, a));
    break;
#if SEXP_USE_RATIOS
  case SEXP_NUM_FLO_RAT:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) + sexp_ratio_to_double(b));
    break;
  case SEXP_NUM_FIX_RAT:
  case SEXP_NUM_BIG_RAT:
    a = tmp = sexp_make_ratio(ctx, a, SEXP_ONE);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_RAT_RAT:
    r = sexp_ratio_add(ctx, a, b);
    break;
#endif
#if SEXP_USE_COMPLEX
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_CPX:
#endif
  case SEXP_NUM_FLO_CPX:
  case SEXP_NUM_FIX_CPX:
  case SEXP_NUM_BIG_CPX:
    a = tmp = sexp_make_complex(ctx, a, SEXP_ZERO);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_CPX_CPX:
    r = sexp_complex_add(ctx, a, b);
    break;
#endif
  }
  sexp_gc_release1(ctx);
  return r;
}

sexp sexp_sub (sexp ctx, sexp a, sexp b) {
#if SEXP_USE_FLONUMS
  int negatep=0;
#endif
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  sexp r=SEXP_VOID;
  sexp_gc_var2(tmp1, tmp2);
  sexp_gc_preserve2(ctx, tmp1, tmp2);
  switch ((at * SEXP_NUM_NUMBER_TYPES) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
#if SEXP_USE_RATIOS
  case SEXP_NUM_NOT_RAT:
#endif
#if SEXP_USE_COMPLEX
  case SEXP_NUM_NOT_CPX:
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_NUMBER, a);
    break;
  case SEXP_NUM_FIX_NOT: case SEXP_NUM_FLO_NOT: case SEXP_NUM_BIG_NOT:
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_NOT:
#endif
#if SEXP_USE_COMPLEX
  case SEXP_NUM_CPX_NOT:
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_NUMBER, b);
    break;
  case SEXP_NUM_FIX_FIX:
    r = sexp_fx_sub(a, b);      /* VM catches this case */
    break;
  case SEXP_NUM_FIX_FLO:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(a)-sexp_flonum_value(b));
    break;
  case SEXP_NUM_FIX_BIG:
    tmp1 = sexp_fixnum_to_bignum(ctx, a);
    r = sexp_bignum_sub(ctx, NULL, b, tmp1);
    sexp_negate_exact(r);
    r = sexp_bignum_normalize(r);
    break;
  case SEXP_NUM_FLO_FIX:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) - sexp_fixnum_to_double(b));
    break;
  case SEXP_NUM_FLO_FLO:
    r = sexp_fp_sub(ctx, a, b);
    break;
  case SEXP_NUM_FLO_BIG:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) - sexp_bignum_to_double(b));
    break;
  case SEXP_NUM_BIG_FIX:
    tmp1 = sexp_fixnum_to_bignum(ctx, b);
    r = sexp_bignum_normalize(sexp_bignum_sub(ctx, NULL, a, tmp1));
    break;
  case SEXP_NUM_BIG_FLO:
    r = sexp_make_flonum(ctx, sexp_bignum_to_double(a) - sexp_flonum_value(b));
    break;
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_normalize(sexp_bignum_sub(ctx, NULL, a, b));
    break;
#if SEXP_USE_RATIOS
  case SEXP_NUM_FLO_RAT:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) - sexp_ratio_to_double(b));
    break;
  case SEXP_NUM_RAT_FLO:
    r = sexp_make_flonum(ctx, sexp_ratio_to_double(a) - sexp_flonum_value(b));
    break;
  case SEXP_NUM_RAT_FIX:
  case SEXP_NUM_RAT_BIG:
    tmp1 = a; a = b; b = tmp1;
    negatep = 1;
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_FIX_RAT:
  case SEXP_NUM_BIG_RAT:
    a = tmp1 = sexp_make_ratio(ctx, a, SEXP_ONE);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_RAT_RAT:
    tmp2 = sexp_make_ratio(ctx, sexp_ratio_numerator(b), sexp_ratio_denominator(b));
    sexp_negate_exact(sexp_ratio_numerator(tmp2));
    r = sexp_ratio_add(ctx, a, tmp2);
    if (negatep) {
      if (sexp_ratiop(r)) {
        sexp_negate_exact(sexp_ratio_numerator(r));
      } else {
        sexp_negate_exact(r);
      }
    }
    break;
#endif
#if SEXP_USE_COMPLEX
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_CPX:
    a = tmp1 = sexp_make_flonum(ctx, sexp_ratio_to_double(a));
    goto complex_sub;
  case SEXP_NUM_CPX_RAT:
    b = tmp1 = sexp_make_flonum(ctx, sexp_ratio_to_double(b));
    /* ... FALLTHROUGH ... */
#endif
  case SEXP_NUM_CPX_FLO:
  case SEXP_NUM_CPX_FIX:
  case SEXP_NUM_CPX_BIG:
    tmp1 = a; a = b; b = tmp1;
    negatep = 1;
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_FLO_CPX:
  case SEXP_NUM_FIX_CPX:
  case SEXP_NUM_BIG_CPX:
    a = tmp1 = sexp_make_complex(ctx, a, SEXP_ZERO);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_CPX_CPX:
#if SEXP_USE_RATIOS
  complex_sub:
#endif
    r = sexp_complex_sub(ctx, a, b);
    if (negatep) {
      if (sexp_complexp(r)) {
        sexp_negate(sexp_complex_real(r));
        sexp_negate(sexp_complex_imag(r));
      } else {
        sexp_negate(r);
      }
    }
    break;
#endif
  }
  sexp_gc_release2(ctx);
  return r;
}

sexp sexp_mul (sexp ctx, sexp a, sexp b) {
  sexp_lsint_t prod;
  int at=sexp_number_type(a), bt=sexp_number_type(b), t;
  sexp r=SEXP_VOID;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  if (at > bt) {r=a; a=b; b=r; t=at; at=bt; bt=t;}
  switch ((at * SEXP_NUM_NUMBER_TYPES) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
#if SEXP_USE_RATIOS
  case SEXP_NUM_NOT_RAT:
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_NUMBER, a);
    break;
  case SEXP_NUM_FIX_FIX:
    prod = (sexp_lsint_t)sexp_unbox_fixnum(a) * sexp_unbox_fixnum(b);
    if ((prod < SEXP_MIN_FIXNUM) || (prod > SEXP_MAX_FIXNUM))
      r = sexp_mul(ctx, tmp=sexp_fixnum_to_bignum(ctx, a), b);
    else
      r = sexp_make_fixnum(prod);
    break;
  case SEXP_NUM_FIX_FLO:
    r = (a==SEXP_ZERO ? a : sexp_make_flonum(ctx, sexp_fixnum_to_double(a)*sexp_flonum_value(b)));
    break;
  case SEXP_NUM_FIX_BIG:
    r = sexp_bignum_fxmul(ctx, NULL, b, sexp_unbox_fixnum(sexp_fx_abs(a)), 0);
    sexp_bignum_sign(r) = sexp_fx_sign(a) * sexp_bignum_sign(b);
    break;
  case SEXP_NUM_FLO_FLO:
    r = sexp_fp_mul(ctx, a, b);
    break;
  case SEXP_NUM_FLO_BIG:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) * sexp_bignum_to_double(b));
    break;
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_mul(ctx, NULL, a, b);
    break;
#if SEXP_USE_RATIOS
  case SEXP_NUM_FLO_RAT:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) * sexp_ratio_to_double(b));
    break;
  case SEXP_NUM_FIX_RAT:
  case SEXP_NUM_BIG_RAT:
    a = tmp = sexp_make_ratio(ctx, a, SEXP_ONE);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_RAT_RAT:
    r = sexp_ratio_mul(ctx, a, b);
    break;
#endif
#if SEXP_USE_COMPLEX
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_CPX:
#endif
  case SEXP_NUM_FLO_CPX:
  case SEXP_NUM_FIX_CPX:
  case SEXP_NUM_BIG_CPX:
    a = tmp = sexp_make_complex(ctx, a, SEXP_ZERO);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_CPX_CPX:
    r = sexp_complex_mul(ctx, a, b);
    break;
#endif
  }
  sexp_gc_release1(ctx);
  return r;
}

sexp sexp_div (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
#if ! SEXP_USE_RATIOS
  double f;
#endif
  sexp r=SEXP_VOID;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  switch ((at * SEXP_NUM_NUMBER_TYPES) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
#if SEXP_USE_RATIOS
  case SEXP_NUM_NOT_RAT:
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_NUMBER, a);
    break;
  case SEXP_NUM_FIX_NOT: case SEXP_NUM_FLO_NOT: case SEXP_NUM_BIG_NOT:
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_NOT:
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_NUMBER, b);
    break;
  case SEXP_NUM_FIX_FIX:
#if SEXP_USE_RATIOS
    tmp = sexp_make_ratio(ctx, a, b);
    r = sexp_ratio_normalize(ctx, tmp, SEXP_FALSE);
#else
    f = sexp_fixnum_to_double(a) / sexp_fixnum_to_double(b);
    r = ((f == trunc(f)) ? sexp_make_fixnum((sexp_sint_t)f)
         : sexp_make_flonum(ctx, f));
#endif
    break;
  case SEXP_NUM_FIX_FLO:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(a)/sexp_flonum_value(b));
    break;
  case SEXP_NUM_FIX_BIG:
#if SEXP_USE_RATIOS
    tmp = sexp_make_ratio(ctx, a, b);
    r = sexp_ratio_normalize(ctx, tmp, SEXP_FALSE);
#else
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(a)/sexp_bignum_to_double(b)); 
#endif
    break;
  case SEXP_NUM_FLO_FIX:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a)/sexp_fixnum_to_double(b));
    break;
  case SEXP_NUM_FLO_FLO:
    r = sexp_fp_div(ctx, a, b);
    break;
  case SEXP_NUM_FLO_BIG:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) / sexp_bignum_to_double(b));
    break;
  case SEXP_NUM_BIG_FIX:
#if SEXP_USE_RATIOS
    tmp = sexp_make_ratio(ctx, a, b);
    r = sexp_ratio_normalize(ctx, tmp, SEXP_FALSE);
    break;
#else
    b = tmp = sexp_fixnum_to_bignum(ctx, b);
#endif
    /* ... FALLTHROUGH if ! SEXP_USE_RATIOS ... */
  case SEXP_NUM_BIG_BIG:
#if SEXP_USE_RATIOS
    tmp = sexp_make_ratio(ctx, a, b);
    r = sexp_ratio_normalize(ctx, tmp, SEXP_FALSE);
#else
    r = sexp_bignum_quot_rem(ctx, &tmp, a, b);
    if (sexp_bignum_normalize(tmp) != SEXP_ZERO)
      r = sexp_make_flonum(ctx, sexp_bignum_to_double(a)
                           / sexp_bignum_to_double(b));
    else
      r = sexp_bignum_normalize(r);
#endif
    break;
  case SEXP_NUM_BIG_FLO:
    r = sexp_make_flonum(ctx, sexp_bignum_to_double(a) / sexp_flonum_value(b));
    break;
#if SEXP_USE_RATIOS
  case SEXP_NUM_FLO_RAT:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) / sexp_ratio_to_double(b));
    break;
  case SEXP_NUM_RAT_FLO:
    r = sexp_make_flonum(ctx, sexp_ratio_to_double(a) / sexp_flonum_value(b));
    break;
  case SEXP_NUM_RAT_FIX:
  case SEXP_NUM_RAT_BIG:
    b = tmp = sexp_make_ratio(ctx, b, SEXP_ONE);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_FIX_RAT:
  case SEXP_NUM_BIG_RAT:
    if (!sexp_ratiop(a))
      a = tmp = sexp_make_ratio(ctx, a, SEXP_ONE);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_RAT_RAT:
    r = sexp_ratio_div(ctx, a, b);
    break;
#endif
#if SEXP_USE_COMPLEX
#if SEXP_USE_RATIOS
  case SEXP_NUM_CPX_RAT:
    b = tmp = sexp_make_flonum(ctx, sexp_ratio_to_double(b));
    /* ... FALLTHROUGH ... */
#endif
  case SEXP_NUM_CPX_FLO:
  case SEXP_NUM_CPX_FIX:
  case SEXP_NUM_CPX_BIG:
    b = tmp = sexp_make_complex(ctx, b, SEXP_ZERO);
    /* ... FALLTHROUGH ... */
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_CPX:
    if (sexp_ratiop(a))
      a = tmp = sexp_make_flonum(ctx, sexp_ratio_to_double(a));
    /* ... FALLTHROUGH ... */
#endif
  case SEXP_NUM_FLO_CPX:
  case SEXP_NUM_FIX_CPX:
  case SEXP_NUM_BIG_CPX:
    if (!sexp_complexp(a))
      a = tmp = sexp_make_complex(ctx, a, SEXP_ZERO);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_CPX_CPX:
    r = sexp_complex_div(ctx, a, b);
    break;
#endif
  }
  sexp_gc_release1(ctx);
  return r;
}

sexp sexp_to_inexact (sexp ctx, sexp a) {
  if (sexp_fixnump(a)) return sexp_fixnum_to_flonum(ctx, a);
  if (sexp_bignump(a)) return sexp_make_flonum(ctx, sexp_bignum_to_double(a));
  return a;
}

sexp sexp_quotient (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  sexp r=SEXP_VOID;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  switch ((at * SEXP_NUM_NUMBER_TYPES) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
    r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, a);
    break;
  case SEXP_NUM_FIX_NOT: case SEXP_NUM_FLO_NOT: case SEXP_NUM_BIG_NOT:
    r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, b);
    break;
  case SEXP_NUM_FLO_FIX: case SEXP_NUM_FLO_FLO: case SEXP_NUM_FLO_BIG:
#if SEXP_USE_RATIOS
  case SEXP_NUM_FLO_RAT:
#endif
    if (sexp_flonum_value(a) != trunc(sexp_flonum_value(a))) {
      r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, a);
    } else {
      tmp = sexp_bignum_normalize(sexp_double_to_bignum(ctx, sexp_flonum_value(a)));
      tmp = sexp_quotient(ctx, tmp, b);
      r = sexp_to_inexact(ctx, tmp);
    }
    break;
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_FIX: case SEXP_NUM_RAT_BIG: case SEXP_NUM_RAT_RAT:
#endif
#if SEXP_USE_COMPLEX
  case SEXP_NUM_FLO_CPX: case SEXP_NUM_CPX_FIX: case SEXP_NUM_CPX_FLO:
  case SEXP_NUM_CPX_BIG: case SEXP_NUM_CPX_CPX:
#if SEXP_USE_RATIOS
  case SEXP_NUM_CPX_RAT:
#endif
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, a);
    break;
  case SEXP_NUM_FIX_FLO: case SEXP_NUM_BIG_FLO:
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_FLO:
#endif
    if (sexp_flonum_value(b) != trunc(sexp_flonum_value(b))) {
      r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, b);
    } else {
      tmp = sexp_bignum_normalize(sexp_double_to_bignum(ctx, sexp_flonum_value(b)));
      tmp = sexp_quotient(ctx, a, tmp);
      r = sexp_to_inexact(ctx, tmp);
    }
    break;
#if SEXP_USE_RATIOS
  case SEXP_NUM_FIX_RAT: case SEXP_NUM_BIG_RAT:
#endif
#if SEXP_USE_COMPLEX
  case SEXP_NUM_FIX_CPX: case SEXP_NUM_BIG_CPX:
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, b);
    break;
  case SEXP_NUM_FIX_FIX:
    r = sexp_fx_div(a, b);
    break;
  case SEXP_NUM_FIX_BIG:
    r = SEXP_ZERO;
    break;
  case SEXP_NUM_BIG_FIX:
    b = tmp = sexp_fixnum_to_bignum(ctx, b);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_normalize(sexp_bignum_quotient(ctx, a, b));
    break;
  }
  sexp_gc_release1(ctx);
  return r;
}

sexp sexp_remainder (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  sexp r=SEXP_VOID;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  switch ((at * SEXP_NUM_NUMBER_TYPES) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
    r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, a);
    break;
  case SEXP_NUM_FIX_NOT: case SEXP_NUM_FLO_NOT: case SEXP_NUM_BIG_NOT:
    r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, b);
    break;
  case SEXP_NUM_FLO_FIX: case SEXP_NUM_FLO_FLO: case SEXP_NUM_FLO_BIG:
#if SEXP_USE_RATIOS
  case SEXP_NUM_FLO_RAT:
#endif
    if (sexp_flonum_value(a) != trunc(sexp_flonum_value(a))) {
      r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, a);
    } else {
      tmp = sexp_bignum_normalize(sexp_double_to_bignum(ctx, sexp_flonum_value(a)));
      tmp = sexp_remainder(ctx, tmp, b);
      r = sexp_to_inexact(ctx, tmp);
    }
    break;
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_FIX: case SEXP_NUM_RAT_BIG: case SEXP_NUM_RAT_RAT:
#endif
#if SEXP_USE_COMPLEX
  case SEXP_NUM_FLO_CPX: case SEXP_NUM_CPX_FIX: case SEXP_NUM_CPX_FLO:
  case SEXP_NUM_CPX_BIG: case SEXP_NUM_CPX_CPX:
#if SEXP_USE_RATIOS
  case SEXP_NUM_CPX_RAT:
#endif
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, a);
    break;
  case SEXP_NUM_FIX_FLO: case SEXP_NUM_BIG_FLO:
#if SEXP_USE_RATIOS
  case SEXP_NUM_RAT_FLO:
#endif
    if (sexp_flonum_value(b) != trunc(sexp_flonum_value(b))) {
      r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, b);
    } else {
      tmp = sexp_bignum_normalize(sexp_double_to_bignum(ctx, sexp_flonum_value(b)));
      tmp = sexp_remainder(ctx, a, tmp);
      r = sexp_to_inexact(ctx, tmp);
    }
    break;
#if SEXP_USE_RATIOS
  case SEXP_NUM_FIX_RAT: case SEXP_NUM_BIG_RAT:
#endif
#if SEXP_USE_COMPLEX
  case SEXP_NUM_FIX_CPX: case SEXP_NUM_BIG_CPX:
#endif
    r = sexp_type_exception(ctx, NULL, SEXP_FIXNUM, b);
    break;
  case SEXP_NUM_FIX_FIX:
    r = sexp_fx_rem(a, b);
    break;
  case SEXP_NUM_FIX_BIG:
    r = a;
    break;
  case SEXP_NUM_BIG_FIX:
    b = tmp = sexp_fixnum_to_bignum(ctx, b);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_normalize(sexp_bignum_remainder(ctx, a, b));
    break;
  }
  sexp_gc_release1(ctx);
  return r;
}

sexp sexp_compare (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  sexp r=SEXP_VOID;
  double f, g;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);
  if (at > bt) {
    r = sexp_compare(ctx, b, a);
    sexp_negate(r);
  } else {
    switch ((at * SEXP_NUM_NUMBER_TYPES) + bt) {
    case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
    case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
#if SEXP_USE_COMPLEX
    case SEXP_NUM_CPX_CPX: case SEXP_NUM_CPX_FIX:
    case SEXP_NUM_CPX_FLO: case SEXP_NUM_CPX_BIG:
#if SEXP_USE_RATIOS
    case SEXP_NUM_CPX_RAT:
#endif
#endif
      r = sexp_type_exception(ctx, NULL, SEXP_NUMBER, a);
      break;
    case SEXP_NUM_FIX_FIX:
      r = sexp_make_fixnum(sexp_unbox_fixnum(a) - sexp_unbox_fixnum(b));
      break;
    case SEXP_NUM_FIX_FLO:
      f = sexp_fixnum_to_double(a);
      g = sexp_flonum_value(b);
      r = sexp_make_fixnum(f < g ? -1 : f == g ? 0 : 1);
      break;
    case SEXP_NUM_FIX_BIG:
      r = sexp_make_fixnum(sexp_bignum_sign(b) < 0 ? 1 : -1);
      break;
    case SEXP_NUM_FLO_FLO:
      f = sexp_flonum_value(a);
      g = sexp_flonum_value(b);
      r = sexp_make_fixnum(f < g ? -1 : f == g ? 0 : 1);
      break;
    case SEXP_NUM_FLO_BIG:
      f = sexp_flonum_value(a);
      if (isinf(f)) {
        r = f > 0 ? SEXP_ONE : SEXP_NEG_ONE;
        break;
      } else if (isnan(f)) {
        r = sexp_xtype_exception(ctx, NULL, "can't compare NaN", a);
        break;
      } else {
        a = tmp = sexp_double_to_bignum(ctx, f);
      }
      /* ... FALLTHROUGH ... */
    case SEXP_NUM_BIG_BIG:
      r = sexp_make_fixnum(sexp_bignum_compare(a, b));
      break;
#if SEXP_USE_RATIOS
    case SEXP_NUM_FLO_RAT:
      f = sexp_flonum_value(a);
      if (isinf(f)) {
        r = f > 0 ? SEXP_ONE : SEXP_NEG_ONE;
      } else if (isnan(f)) {
        r = sexp_xtype_exception(ctx, NULL, "can't compare NaN", a);
      } else {
        g = sexp_ratio_to_double(b);
        r = sexp_make_fixnum(f < g ? -1 : f == g ? 0 : 1);
      }
      break;
    case SEXP_NUM_FIX_RAT:
    case SEXP_NUM_BIG_RAT:
      a = tmp = sexp_make_ratio(ctx, a, SEXP_ONE);
      /* ... FALLTHROUGH ... */
    case SEXP_NUM_RAT_RAT:
      r = sexp_ratio_compare(ctx, a, b);
      break;
#endif
    }
  }
  sexp_gc_release1(ctx);
  return r;
}

#endif
