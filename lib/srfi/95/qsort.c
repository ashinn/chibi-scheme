/*  qsort.c -- object comparison & sort implementation        */
/*  Copyright (c) 2009-2015 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"

#ifdef __cplusplus
namespace {
#endif

#if SEXP_USE_HUFF_SYMS
#if SEXP_USE_STATIC_LIBS
#include "chibi/sexp-hufftabdefs.h"
#else
#include "chibi/sexp-hufftabs.h"
#endif
#endif

#ifdef __cplusplus
}
#endif

#define swap(tmp_var, a, b) (tmp_var=a, a=b, b=tmp_var)

static sexp sexp_vector_copy_to_list (sexp ctx, sexp vec, sexp seq) {
  sexp_sint_t i;
  sexp ls, *data=sexp_vector_data(vec);
  for (i=0, ls=seq; sexp_pairp(ls); i++, ls=sexp_cdr(ls))
    sexp_car(ls) = data[i];
  return seq;
}

static sexp sexp_vector_nreverse (sexp ctx, sexp vec) {
  int i, j;
  sexp tmp, *data=sexp_vector_data(vec);
  for (i=0, j=sexp_vector_length(vec)-1; i<j; i++, j--)
    swap(tmp, data[i], data[j]);
  return vec;
}

static int sexp_basic_comparator (sexp op) {
  if (sexp_not(op))
    return 1;
  if (! sexp_opcodep(op))
    return 0;
  if (sexp_opcode_class(op) == SEXP_OPC_ARITHMETIC_CMP)
    return 1;
  return 0;
}

#if SEXP_USE_HUFF_SYMS
static int sexp_isymbol_compare (sexp ctx, sexp a, sexp b) {
  int res, res2, tmp;
  sexp_uint_t c = ((sexp_uint_t)a)>>SEXP_IMMEDIATE_BITS, d = ((sexp_uint_t)b)>>SEXP_IMMEDIATE_BITS;
  while (c && d) {
#include "chibi/sexp-unhuff.h"
#define c d
#define res res2
#include "chibi/sexp-unhuff.h"
#undef c
#undef res
    if ((tmp=res-res2) != 0)
      return tmp;
  }
  return c ? 1 : d ? -1 : 0;
}
#endif

#if SEXP_USE_RATIOS
#define sexp_non_immediate_ordered_numberp(x) \
  (sexp_flonump(x) || sexp_bignump(x) || sexp_ratiop(x))
#elif SEXP_USE_BIGNUMS && SEXP_USE_FLONUMS
#define sexp_non_immediate_ordered_numberp(x) \
  (sexp_flonump(x) || sexp_bignump(x))
#elif SEXP_USE_BIGNUMS
#define sexp_non_immediate_ordered_numberp(x) (sexp_bignump(x))
#elif SEXP_USE_FLONUMS
#define sexp_non_immediate_ordered_numberp(x) (sexp_flonump(x))
#else
#define sexp_non_immediate_ordered_numberp(x) 0
#endif

static int sexp_object_compare (sexp ctx, sexp a, sexp b) {
  int res;
  if (a == b)
    return 0;
  if (sexp_pointerp(a)) {
    if (sexp_pointerp(b)) {
      if (sexp_pointer_tag(a) == sexp_pointer_tag(b)) {
        switch (sexp_pointer_tag(a)) {
#if SEXP_USE_FLONUMS
        case SEXP_FLONUM:
          res = sexp_flonum_value(a) > sexp_flonum_value(b) ? 1 :
                sexp_flonum_value(a) < sexp_flonum_value(b) ? -1 : 0;
          break;
#endif
#if SEXP_USE_BIGNUMS
        case SEXP_BIGNUM:
          res = sexp_bignum_compare(a, b);
          break;
#endif
#if SEXP_USE_RATIOS
        case SEXP_RATIO:
          res = sexp_unbox_fixnum(sexp_ratio_compare(ctx, a, b));
          break;
#endif
#if SEXP_USE_COMPLEX
        case SEXP_COMPLEX:
          res = sexp_object_compare(ctx, sexp_complex_real(a), sexp_complex_real(b));
          if (res==0) res = sexp_object_compare(ctx, sexp_complex_imag(a), sexp_complex_imag(b));
          break;
#endif
        case SEXP_STRING:
          res = strcmp(sexp_string_data(a), sexp_string_data(b));
          break;
        case SEXP_SYMBOL:
          res = strcmp(sexp_lsymbol_data(a), sexp_lsymbol_data(b));
          break;
        /* TODO: consider recursively traversing containers.  requires */
        /* cycle detection. */
        /* case SEXP_PAIR: */
        /*   res = sexp_object_compare(ctx, sexp_car(a), sexp_car(b)); */
        /*   if (res==0) res = sexp_object_compare(ctx, sexp_cdr(a), sexp_cdr(b)); */
        /*   break; */
        default:
          res = 0;
          break;
        }
      } else if (sexp_non_immediate_ordered_numberp(a) &&
                 sexp_non_immediate_ordered_numberp(b)) {
        res = sexp_unbox_fixnum(sexp_compare(ctx, a, b));
      } else {
        res = sexp_pointer_tag(a) - sexp_pointer_tag(b);
      }
#if SEXP_USE_BIGNUMS || SEXP_USE_FLONUMS
    } else if (sexp_fixnump(b) && sexp_non_immediate_ordered_numberp(a)) {
      res = sexp_unbox_fixnum(sexp_compare(ctx, a, b));
#endif
#if SEXP_USE_HUFF_SYMS
    } else if (sexp_isymbolp(b) && sexp_lsymbolp(a)) {
      res = strcmp(sexp_lsymbol_data(a),
		   sexp_string_data(sexp_write_to_string(ctx, b)));
#endif
    } else {
      res = 1;
    }
  } else if (sexp_pointerp(b)) {
#if SEXP_USE_BIGNUMS || SEXP_USE_FLONUMS
    if (sexp_fixnump(a) && sexp_non_immediate_ordered_numberp(b))
      res = sexp_unbox_fixnum(sexp_compare(ctx, a, b));
    else
#endif
#if SEXP_USE_HUFF_SYMS
    if (sexp_isymbolp(a) && sexp_lsymbolp(b))
      res = strcmp(sexp_string_data(sexp_write_to_string(ctx, a)),
		   sexp_lsymbol_data(b));
    else
#endif
      res = -1;
  } else {
#if SEXP_USE_HUFF_SYMS
    if (sexp_isymbolp(a) && sexp_isymbolp(b))
      return sexp_isymbol_compare(ctx, a, b);
    else
#endif
      res = (sexp_sint_t)a - (sexp_sint_t)b;
  }
  return res;
}

sexp sexp_object_compare_op (sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
  return sexp_make_fixnum(sexp_object_compare(ctx, a, b));
}

/* fast path when using general object-cmp comparator with no key */
/* TODO: include another fast path when the key is a fixed offset */
static void sexp_merge_sort (sexp ctx, sexp *vec, sexp *scratch, sexp_sint_t lo, sexp_sint_t hi) {
  sexp_sint_t mid, i, j, k;
  sexp tmp;
  switch (hi - lo) {
  case 0:
    scratch[lo] = vec[lo];
    break;
  case 2:
    if (sexp_object_compare(ctx, vec[hi], vec[hi-1]) < 0)
      swap(tmp, vec[hi], vec[hi-1]);
    /* ... FALLTHROUGH ... */
  case 1:
    if (sexp_object_compare(ctx, vec[lo+1], vec[lo]) < 0) {
      swap(tmp, vec[lo+1], vec[lo]);
      if (hi - lo > 1) {
        if (sexp_object_compare(ctx, vec[lo+2], vec[lo+1]) < 0)
          swap(tmp, vec[lo+2], vec[lo+1]);
      }
    }
    break;
  default:                      /* at least 4 elements */
    mid = (hi+lo)/2;
    sexp_merge_sort(ctx, vec, scratch, lo, mid);
    sexp_merge_sort(ctx, vec, scratch, mid+1, hi);
    for (k=lo, i = lo, j = mid+1; k <= hi; ++k) {
      if (i > mid) {
        scratch[k] = vec[j++];
      } else if (j > hi) {
        scratch[k] = vec[i++];
      } else {
        if (sexp_object_compare(ctx, vec[j], vec[i]) < 0) {
          scratch[k] = vec[j++];
        } else {
          scratch[k] = vec[i++];
        }
      }
    }
    memcpy(vec + lo, scratch + lo, (hi-lo+1) * sizeof(sexp));
  }
}

#define if_is_less(i, j)                                                \
  a = (sexp_truep(key) ? (sexp_car(args1) = vec[i], sexp_apply(ctx, key, args1)) : vec[i]);       \
  if (sexp_exceptionp(a)) {res=a; goto done;}                           \
  b = (sexp_truep(key) ? (sexp_car(args1) = vec[j], sexp_apply(ctx, key, args1)) : vec[j]); \
  if (sexp_exceptionp(b)) {res=b; goto done;}                           \
  sexp_car(args2) = a;                                                  \
  sexp_car(args1) = b;                                                  \
  res = sexp_apply(ctx, less, args2);                                   \
  if (sexp_exceptionp(res)) goto done;                                  \
  if (sexp_truep(res))

static sexp sexp_merge_sort_less (sexp ctx, sexp *vec, sexp *scratch,
                                  sexp_sint_t lo, sexp_sint_t hi,
                                  sexp less, sexp key) {
  sexp_sint_t mid, i, j, k;
  sexp args1;
  sexp_gc_var5(a, b, tmp, args2, res);
  sexp_gc_preserve5(ctx, a, b, tmp, args2, res);
  args2 = sexp_list2(ctx, SEXP_VOID, SEXP_VOID);
  args1 = sexp_cdr(args2);
  switch (hi - lo) {
  case 0:
    res = SEXP_VOID;
    scratch[lo] = vec[lo];
    break;
  case 2:
    if_is_less (hi, hi-1)
      swap(tmp, vec[hi], vec[hi-1]);
    /* ... FALLTHROUGH ... */
  case 1:
    if_is_less (lo+1, lo) {
      swap(tmp, vec[lo+1], vec[lo]);
      if (hi - lo > 1) {
        if_is_less (lo+2, lo+1)
          swap(tmp, vec[lo+2], vec[lo+1]);
      }
    }
    break;
  default:                      /* at least 4 elements */
    mid = (hi+lo)/2;
    res = sexp_merge_sort_less(ctx, vec, scratch, lo, mid, less, key);
    if (sexp_exceptionp(res))
      goto done;
    res = sexp_merge_sort_less(ctx, vec, scratch, mid+1, hi, less, key);
    if (sexp_exceptionp(res))
      goto done;
    for (k=lo, i = lo, j = mid+1; k <= hi; ++k) {
      if (i > mid) {
        scratch[k] = vec[j++];
      } else if (j > hi) {
        scratch[k] = vec[i++];
      } else {
        if_is_less (j, i) {
          scratch[k] = vec[j++];
        } else {
          scratch[k] = vec[i++];
        }
      }
    }
    memcpy(vec + lo, scratch + lo, (hi-lo+1) * sizeof(sexp));
  }
 done:
  sexp_gc_release5(ctx);
  return res;
}

sexp sexp_sort_x (sexp ctx, sexp self, sexp_sint_t n, sexp seq,
                         sexp less, sexp key) {
  sexp_sint_t len;
  sexp res;
  sexp_gc_var2(vec, scratch);

  if (sexp_nullp(seq)) return seq;

  sexp_gc_preserve2(ctx, vec, scratch);

  vec = (sexp_truep(sexp_listp(ctx, seq)) ? sexp_list_to_vector(ctx, seq) : seq);

  if (! sexp_vectorp(vec)) {
    res = sexp_type_exception(ctx, self, SEXP_VECTOR, vec);
  } else {
    scratch = sexp_make_vector(ctx, sexp_make_fixnum(sexp_vector_length(vec)), SEXP_VOID);
    len = sexp_vector_length(vec);
    if (sexp_not(key) && sexp_basic_comparator(less)) {
      sexp_merge_sort(ctx, sexp_vector_data(vec), sexp_vector_data(scratch),
                      0, len-1);
      if (sexp_opcodep(less) && sexp_opcode_inverse(less))
        sexp_vector_nreverse(ctx, vec);
      res = vec;
    } else if (! (sexp_procedurep(less) || sexp_opcodep(less))) {
      res = sexp_type_exception(ctx, self, SEXP_PROCEDURE, less);
    } else if (! (sexp_procedurep(key) || sexp_opcodep(key) || sexp_not(key))) {
      res = sexp_type_exception(ctx, self, SEXP_PROCEDURE, key);
    } else {
      res = sexp_merge_sort_less(ctx, sexp_vector_data(vec),
                                 sexp_vector_data(scratch),
                                 0, len-1, less, key);
    }
  }

  if (sexp_pairp(seq) && ! sexp_exceptionp(res))
    res = sexp_vector_copy_to_list(ctx, vec, seq);

  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_define_foreign(ctx, env, "object-cmp", 2, sexp_object_compare_op);
  sexp_define_foreign_opt(ctx, env, "sort!", 3, sexp_sort_x, SEXP_FALSE);
  return SEXP_VOID;
}
