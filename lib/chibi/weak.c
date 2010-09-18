/*  weak.c -- weak pointers and ephemerons               */
/*  Copyright (c) 2010 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include <chibi/eval.h>

static int sexp_ephemeron_id;

#define sexp_ephemeron_key(x) sexp_slot_ref(x, 0)
#define sexp_ephemeron_value(x) sexp_slot_ref(x, 1)

sexp sexp_make_ephemeron (sexp ctx sexp_api_params(self, n), sexp key, sexp value) {
  sexp res = sexp_alloc_type(ctx, pair, sexp_ephemeron_id);
  if (! sexp_exceptionp(res)) {
    sexp_ephemeron_key(res) = key;
    sexp_ephemeron_value(res) = value;
  }
  return res;
}

sexp sexp_ephemeron_brokenp_op (sexp ctx sexp_api_params(self, n), sexp eph) {
  return sexp_make_boolean(sexp_brokenp(eph));
}

sexp sexp_init_library (sexp ctx sexp_api_params(self, n), sexp env) {
  sexp_gc_var3(name, t, op);
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

  sexp_gc_release3(ctx);
  return SEXP_VOID;
}

