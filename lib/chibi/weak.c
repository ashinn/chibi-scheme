/*  weak.c -- weak pointers and ephemerons                    */
/*  Copyright (c) 2010-2011 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

static int sexp_ephemeron_id, sexp_weak_vector_id;

#define sexp_ephemeronp(x) sexp_check_tag(x, sexp_ephemeron_id)
#define sexp_ephemeron_key(x) sexp_slot_ref(x, 0)
#define sexp_ephemeron_value(x) sexp_slot_ref(x, 1)

#define sexp_weak_vector_p(x) sexp_check_tag(x, sexp_weak_vector_id)

sexp sexp_make_ephemeron (sexp ctx, sexp self, sexp_sint_t n, sexp key, sexp value) {
  sexp res = sexp_alloc_type(ctx, pair, sexp_ephemeron_id);
  if (! sexp_exceptionp(res)) {
    sexp_ephemeron_key(res) = key;
    sexp_ephemeron_value(res) = value;
  }
  return res;
}

sexp sexp_ephemeron_brokenp_op (sexp ctx, sexp self, sexp_sint_t n, sexp eph) {
  return sexp_make_boolean(sexp_brokenp(eph));
}

sexp sexp_make_weak_vector (sexp ctx, sexp self, sexp_sint_t n, sexp len) {
  sexp vec, *x;
  int i, clen = sexp_unbox_fixnum(len);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, len);
  vec = sexp_alloc_tagged(ctx, sexp_sizeof(vector) + clen*sizeof(sexp),
                          SEXP_VECTOR);
  if (sexp_exceptionp(vec)) return vec;
  x = sexp_vector_data(vec);
  for (i=0; i<clen; i++)
    x[i] = SEXP_VOID;
  sexp_vector_length(vec) = clen;
  return vec;
}

sexp sexp_weak_vector_length (sexp ctx, sexp self, sexp_sint_t n, sexp v) {
  sexp_assert_type(ctx, sexp_weak_vector_p, sexp_weak_vector_id, v);
  return sexp_make_fixnum(sexp_vector_length(v));
}

sexp sexp_weak_vector_ref (sexp ctx, sexp self, sexp_sint_t n, sexp v, sexp k) {
  sexp_assert_type(ctx, sexp_weak_vector_p, sexp_weak_vector_id, v);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, k);
  return sexp_vector_ref(v, k);
}

sexp sexp_weak_vector_set (sexp ctx, sexp self, sexp_sint_t n, sexp v, sexp k, sexp x) {
  sexp_assert_type(ctx, sexp_weak_vector_p, sexp_weak_vector_id, v);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, k);
  sexp_vector_set(v, k, x);
  return SEXP_VOID;
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  sexp v;
  sexp_gc_var3(name, t, op);
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_gc_preserve3(ctx, name, t, op);

  name = sexp_c_string(ctx, "Ephemeron", -1);
  t = sexp_register_simple_type(ctx, name, SEXP_FALSE, SEXP_TWO);
  sexp_ephemeron_id = sexp_type_tag(t);
  sexp_type_field_len_base(t) = 0;
  sexp_type_weak_base(t) = sexp_type_field_base(t);
  sexp_type_weak_len_base(t) = 1;
  sexp_type_weak_len_extra(t) = 1;

  op = sexp_make_type_predicate(ctx, name=sexp_c_string(ctx,"ephemeron?",-1), t);
  sexp_env_define(ctx, env, name=sexp_intern(ctx, "ephemeron?", -1), op);
  op = sexp_make_getter(ctx, name=sexp_c_string(ctx, "ephemeron-key", -1), t, SEXP_ZERO);
  sexp_env_define(ctx, env, name=sexp_intern(ctx, "ephemeron-key", -1), op);
  op = sexp_make_getter(ctx, name=sexp_c_string(ctx, "ephemeron-value", -1), t, SEXP_ONE);
  sexp_env_define(ctx, env, name=sexp_intern(ctx, "ephemeron-value", -1), op);
  sexp_define_foreign(ctx, env, "make-ephemeron", 2, sexp_make_ephemeron);
  sexp_define_foreign(ctx, env, "ephemeron-broken?", 1, sexp_ephemeron_brokenp_op);

  name = sexp_c_string(ctx, "Weak-Vector", -1);
  t = sexp_register_simple_type(ctx, name, SEXP_FALSE, SEXP_ZERO);
  v = sexp_type_by_index(ctx, SEXP_VECTOR);
  sexp_weak_vector_id = sexp_type_tag(t);
  sexp_type_weak_base(t) = sexp_type_field_base(v);
  sexp_type_weak_len_off(t) = sexp_type_field_len_off(v);
  sexp_type_weak_len_scale(t) = sexp_type_field_len_scale(v);

  op = sexp_make_type_predicate(ctx, name=sexp_c_string(ctx,"weak-vector?",-1), t);
  sexp_env_define(ctx, env, name=sexp_intern(ctx, "weak-vector?", -1), op);
  sexp_define_foreign(ctx, env, "make-weak-vector", 1, sexp_make_weak_vector);
  sexp_define_foreign(ctx, env, "weak-vector-length", 2, sexp_weak_vector_length);
  sexp_define_foreign(ctx, env, "weak-vector-ref", 2, sexp_weak_vector_ref);
  sexp_define_foreign(ctx, env, "weak-vector-set!", 3, sexp_weak_vector_set);

  sexp_gc_release3(ctx);
  return SEXP_VOID;
}

