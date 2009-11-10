/*  bignum.c -- bignum support                           */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#define SEXP_INIT_BIGNUM_SIZE 2

#define sexp_negate(x)                                  \
  if (sexp_bignump(x))                                  \
    sexp_bignum_sign(x) = -sexp_bignum_sign(x);         \
  else if (sexp_fixnump(x))                            \
    x = sexp_fx_neg(x);

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

sexp sexp_make_integer (sexp ctx, sexp_sint_t x) {
  sexp res;
  if ((SEXP_MIN_FIXNUM < x) && (x < SEXP_MAX_FIXNUM)) {
    res = sexp_make_fixnum(x);
  } else {
    res = sexp_make_bignum(ctx, 1);
    sexp_bignum_sign(res) = (x < 0 ? -1 : 1);
    sexp_bignum_data(res)[0] = x * sexp_bignum_sign(res);
  }
  return res;
}

#define double_trunc_10s_digit(f) (trunc((f)/10.0)*10.0)
#define double_10s_digit(f) ((f)-double_trunc_10s_digit(f))

sexp sexp_double_to_bignum (sexp ctx, double f) {
  int sign;
  sexp_gc_var3(res, scale, tmp);
  sexp_gc_preserve3(ctx, res, scale, tmp);
  res = sexp_fixnum_to_bignum(ctx, sexp_make_fixnum(0));
  scale = sexp_fixnum_to_bignum(ctx, sexp_make_fixnum(1));
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
    memcpy(dst, a, size);
    sexp_bignum_length(dst) = len;
  } else {
    memset(dst->value.bignum.data, 0,
           sexp_bignum_length(dst)*sizeof(sexp_uint_t));
    memcpy(dst->value.bignum.data, a->value.bignum.data,
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

static sexp_uint_t sexp_bignum_hi (sexp a) {
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
  sexp_uint_t i, *data=sexp_bignum_data(a);
  for (i=0; i<sexp_bignum_length(a); i++)
    res = res * ((double)SEXP_UINT_T_MAX+1) + data[i];
  return res;
}

sexp sexp_bignum_fxadd (sexp ctx, sexp a, sexp_uint_t b) {
  sexp_uint_t len=sexp_bignum_hi(a), *data=sexp_bignum_data(a),
    carry=b, i, n;
  for (i=0; i<len; i++) {
    n = data[i];
    data[i] += carry;
    if (n > (SEXP_UINT_T_MAX - carry)) {
      carry = 1;
    } else {
      carry = 0;
      break;
    }
  }
  if (carry) {
    a = sexp_copy_bignum(ctx, NULL, a, len+1);
    sexp_bignum_data(a)[len] = 1;
  }
  return a;
}

sexp sexp_bignum_fxsub (sexp ctx, sexp a, sexp_uint_t b) {
  sexp_uint_t *data=sexp_bignum_data(a), borrow=b, i=0, n;
  for (borrow=b; borrow; i++) {
    n = data[i];
    data[i] -= borrow;
    borrow = ((n < borrow) ? 1 : 0);
  }
  return a;
}

sexp sexp_bignum_fxmul (sexp ctx, sexp d, sexp a, sexp_uint_t b, int offset) {
  sexp_uint_t len=sexp_bignum_length(a), *data, *adata=sexp_bignum_data(a),
    carry=0, i;
  sexp_luint_t n;
  if ((! d) || (sexp_bignum_length(d)+offset < len))
    d = sexp_make_bignum(ctx, len);
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
  return d;
}

sexp_uint_t sexp_bignum_fxdiv (sexp ctx, sexp a, sexp_uint_t b, int offset) {
  sexp_uint_t len=sexp_bignum_hi(a), *data=sexp_bignum_data(a), q, r;
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
                       char sign, sexp_uint_t base) {
  int c, digit;
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_make_bignum(ctx, SEXP_INIT_BIGNUM_SIZE);
  sexp_bignum_sign(res) = sign;
  sexp_bignum_data(res)[0] = init;
  for (c=sexp_read_char(ctx, in); isxdigit(c); c=sexp_read_char(ctx, in)) {
    digit = digit_value(c);
    if ((digit < 0) || (digit >= base))
      break;
    res = sexp_bignum_fxmul(ctx, res, res, base, 0);
    res = sexp_bignum_fxadd(ctx, res, digit);
  }
  if (c=='.' || c=='e' || c=='E') {
    if (base != 10)
      res = sexp_read_error(ctx, "found non-base 10 float", SEXP_NULL, in);
    if (c!='.') sexp_push_char(ctx, c, in);
    res = sexp_read_float_tail(ctx, in, sexp_bignum_to_double(res), (sign==-1));
  } else if ((c!=EOF) && ! is_separator(c)) {
    res = sexp_read_error(ctx, "invalid numeric syntax",
                          sexp_make_character(c), in);
  }
  sexp_push_char(ctx, c, in);
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
    return sexp_fixnum_to_bignum(ctx, sexp_make_fixnum(0));
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
  i = sexp_fixnum_to_bignum(ctx, sexp_make_fixnum(1));
  res = quot_step(ctx, rem, a1, b1, k, i);
  sexp_bignum_sign(res) = sexp_bignum_sign(a) * sexp_bignum_sign(b);
  if (sexp_bignum_sign(a) < 0) {
    sexp_negate(*rem);
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
  sexp rem;
  sexp_bignum_quot_rem(ctx, &rem, a, b); /* discard quotient */
  return rem;
}

sexp sexp_bignum_expt (sexp ctx, sexp a, sexp b) {
  sexp_sint_t e = sexp_unbox_fixnum(sexp_fx_abs(b));
  sexp_gc_var2(res, acc);
  sexp_gc_preserve2(ctx, res, acc);
  res = sexp_fixnum_to_bignum(ctx, sexp_make_fixnum(1));
  acc = sexp_copy_bignum(ctx, NULL, a, 0);
  for (; e; e>>=1, acc=sexp_bignum_mul(ctx, NULL, acc, acc))
    if (e & 1)
      res = sexp_bignum_mul(ctx, NULL, res, acc);
  sexp_gc_release2(ctx);
  return res;
}

/****************** generic arithmetic ************************/

enum sexp_number_types {
  SEXP_NUM_NOT = 0,
  SEXP_NUM_FIX,
  SEXP_NUM_FLO,
  SEXP_NUM_BIG
};

enum sexp_number_combs {
  SEXP_NUM_NOT_NOT = 0,
  SEXP_NUM_NOT_FIX,
  SEXP_NUM_NOT_FLO,
  SEXP_NUM_NOT_BIG,
  SEXP_NUM_FIX_NOT,
  SEXP_NUM_FIX_FIX,
  SEXP_NUM_FIX_FLO,
  SEXP_NUM_FIX_BIG,
  SEXP_NUM_FLO_NOT,
  SEXP_NUM_FLO_FIX,
  SEXP_NUM_FLO_FLO,
  SEXP_NUM_FLO_BIG,
  SEXP_NUM_BIG_NOT,
  SEXP_NUM_BIG_FIX,
  SEXP_NUM_BIG_FLO,
  SEXP_NUM_BIG_BIG
};

static int sexp_number_types[] =
  {0, 0, 1, 0, 0, 0, 0, 0, 0, 2, 3, 0, 0, 0, 0, 0};

static int sexp_number_type (sexp a) {
  return sexp_pointerp(a) ? sexp_number_types[sexp_pointer_tag(a)&1111]
    : sexp_fixnump(a);
}

sexp sexp_add (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b), t;
  sexp r;
  if (at > bt) {r=a; a=b; b=r; t=at; at=bt; bt=t;}
  switch ((at << 2) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
    r = sexp_type_exception(ctx, "+: not a number", a);
    break;
  case SEXP_NUM_FIX_FIX:
    r = sexp_fx_add(a, b);      /* XXXX check overflow */
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
  }
  return r;
}

sexp sexp_sub (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  sexp r;
  switch ((at << 2) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
    r = sexp_type_exception(ctx, "-: not a number", a);
    break;
  case SEXP_NUM_FIX_NOT: case SEXP_NUM_FLO_NOT: case SEXP_NUM_BIG_NOT:
    r = sexp_type_exception(ctx, "-: not a number", b);
    break;
  case SEXP_NUM_FIX_FIX:
    r = sexp_fx_sub(a, b);      /* XXXX check overflow */
    break;
  case SEXP_NUM_FIX_FLO:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(a)-sexp_flonum_value(b));
    break;
  case SEXP_NUM_FIX_BIG:
    r = sexp_bignum_sub(ctx, NULL, b, sexp_fixnum_to_bignum(ctx, a));
    sexp_negate(r);
    r = sexp_bignum_normalize(r);
    break;
  case SEXP_NUM_FLO_FIX:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(b)+sexp_flonum_value(a));
    break;
  case SEXP_NUM_FLO_FLO:
    r = sexp_fp_sub(ctx, a, b);
    break;
  case SEXP_NUM_FLO_BIG:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) - sexp_bignum_to_double(b));
    break;
  case SEXP_NUM_BIG_FIX:
    r = sexp_bignum_normalize(sexp_bignum_sub(ctx, NULL, a, sexp_fixnum_to_bignum(ctx, b)));
    break;
  case SEXP_NUM_BIG_FLO:
    r = sexp_make_flonum(ctx, sexp_flonum_value(b) - sexp_bignum_to_double(a));
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_normalize(sexp_bignum_sub(ctx, NULL, a, b));
    break;
  }
  return r;
}

sexp sexp_mul (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b), t;
  sexp r;
  if (at > bt) {r=a; a=b; b=r; t=at; at=bt; bt=t;}
  switch ((at << 2) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
    r = sexp_type_exception(ctx, "*: not a number", a);
    break;
  case SEXP_NUM_FIX_FIX:
    r = sexp_fx_mul(a, b);
    break;
  case SEXP_NUM_FIX_FLO:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(a)*sexp_flonum_value(b));
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
  }
  return r;
}

sexp sexp_div (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  double f;
  sexp r, rem;
  switch ((at << 2) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
    r = sexp_type_exception(ctx, "/: not a number", a);
    break;
  case SEXP_NUM_FIX_NOT: case SEXP_NUM_FLO_NOT: case SEXP_NUM_BIG_NOT:
    r = sexp_type_exception(ctx, "/: not a number", b);
    break;
  case SEXP_NUM_FIX_FIX:
    f = sexp_fixnum_to_double(a) / sexp_fixnum_to_double(b);
    r = ((f == trunc(f)) ? sexp_make_fixnum((sexp_sint_t)f)
         : sexp_make_flonum(ctx, f));
    break;
  case SEXP_NUM_FIX_FLO:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(a)/sexp_flonum_value(b));
    break;
  case SEXP_NUM_FIX_BIG:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(a)/sexp_bignum_to_double(b));
    break;
  case SEXP_NUM_FLO_FIX:
    r = sexp_make_flonum(ctx, sexp_fixnum_to_double(b)/sexp_flonum_value(a));
    break;
  case SEXP_NUM_FLO_FLO:
    r = sexp_fp_div(ctx, a, b);
    break;
  case SEXP_NUM_FLO_BIG:
    r = sexp_make_flonum(ctx, sexp_flonum_value(a) / sexp_bignum_to_double(b));
    break;
  case SEXP_NUM_BIG_FIX:
    b = sexp_fixnum_to_bignum(ctx, b);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_quot_rem(ctx, &rem, a, b);
    if (sexp_bignum_normalize(rem) != sexp_make_fixnum(0))
      r = sexp_make_flonum(ctx, sexp_bignum_to_double(a)
                           / sexp_fixnum_to_double(b));
    else
      r = sexp_bignum_normalize(r);
    break;
  case SEXP_NUM_BIG_FLO:
    r = sexp_make_flonum(ctx, sexp_bignum_to_double(a) / sexp_flonum_value(b));
    break;
  }
  return r;
}

sexp sexp_quotient (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  sexp r;
  switch ((at << 2) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
    r = sexp_type_exception(ctx, "quotient: not a number", a);
    break;
  case SEXP_NUM_FIX_NOT: case SEXP_NUM_FLO_NOT: case SEXP_NUM_BIG_NOT:
    r = sexp_type_exception(ctx, "quotient: not a number", b);
    break;
  case SEXP_NUM_FLO_FIX: case SEXP_NUM_FLO_FLO: case SEXP_NUM_FLO_BIG:
    r = sexp_type_exception(ctx, "quotient: can't take quotient of inexact", a);
    break;
  case SEXP_NUM_FIX_FLO: case SEXP_NUM_BIG_FLO:
    r = sexp_type_exception(ctx, "quotient: can't take quotient of inexact", b);
    break;
  case SEXP_NUM_FIX_FIX:
    r = sexp_fx_div(a, b);
    break;
  case SEXP_NUM_FIX_BIG:
    r = sexp_make_fixnum(0);
    break;
  case SEXP_NUM_BIG_FIX:
    b = sexp_fixnum_to_bignum(ctx, b);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_normalize(sexp_bignum_quotient(ctx, a, b));
    break;
  }
  return r;
}

sexp sexp_remainder (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  sexp r;
  switch ((at << 2) + bt) {
  case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
  case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
    r = sexp_type_exception(ctx, "remainder: not a number", a);
    break;
  case SEXP_NUM_FIX_NOT: case SEXP_NUM_FLO_NOT: case SEXP_NUM_BIG_NOT:
    r = sexp_type_exception(ctx, "remainder: not a number", b);
    break;
  case SEXP_NUM_FLO_FIX: case SEXP_NUM_FLO_FLO: case SEXP_NUM_FLO_BIG:
    r = sexp_type_exception(ctx, "remainder: can't take quotient of inexact", a);
    break;
  case SEXP_NUM_FIX_FLO: case SEXP_NUM_BIG_FLO:
    r = sexp_type_exception(ctx, "remainder: can't take quotient of inexact", b);
    break;
  case SEXP_NUM_FIX_FIX:
    r = sexp_fx_rem(a, b);
    break;
  case SEXP_NUM_FIX_BIG:
    r = a;
    break;
  case SEXP_NUM_BIG_FIX:
    b = sexp_fixnum_to_bignum(ctx, b);
    /* ... FALLTHROUGH ... */
  case SEXP_NUM_BIG_BIG:
    r = sexp_bignum_normalize(sexp_bignum_remainder(ctx, a, b));
    break;
  }
  return r;
}

sexp sexp_compare (sexp ctx, sexp a, sexp b) {
  int at=sexp_number_type(a), bt=sexp_number_type(b);
  sexp r;
  double f;
  if (at > bt) {
    r = sexp_compare(ctx, b, a);
    sexp_negate(r);
  } else {
    switch ((at << 2) + bt) {
    case SEXP_NUM_NOT_NOT: case SEXP_NUM_NOT_FIX:
    case SEXP_NUM_NOT_FLO: case SEXP_NUM_NOT_BIG:
      r = sexp_type_exception(ctx, "compare: not a number", a);
      break;
    case SEXP_NUM_FIX_FIX:
      r = sexp_make_fixnum(sexp_unbox_fixnum(a) - sexp_unbox_fixnum(b));
      break;
    case SEXP_NUM_FIX_FLO:
      f = sexp_fixnum_to_double(a) - sexp_flonum_value(b);
      r = sexp_make_fixnum(f > 0.0 ? 1 : f == 0.0 ? 0 : -1);
      break;
    case SEXP_NUM_FIX_BIG:
      r = sexp_make_fixnum(-1);
      break;
    case SEXP_NUM_FLO_FLO:
      f = sexp_flonum_value(a) - sexp_flonum_value(b);
      r = sexp_make_fixnum(f > 0.0 ? 1 : f == 0.0 ? 0 : -1);
      break;
    case SEXP_NUM_FLO_BIG:
      f = sexp_flonum_value(a) - sexp_bignum_to_double(b);
      r = sexp_make_fixnum(f > 0.0 ? 1 : f == 0.0 ? 0 : -1);
      break;
    case SEXP_NUM_BIG_BIG:
      r = sexp_make_fixnum(sexp_bignum_compare(a, b));
      break;
    }
  }
  return r;
}

