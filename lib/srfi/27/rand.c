/*  rand.c -- rand_r/random_r interface                       */
/*  Copyright (c) 2009-2014 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <time.h>
#include <chibi/eval.h>

#define SEXP_RANDOM_STATE_SIZE 128

#define ZERO sexp_make_fixnum(0)
#define ONE  sexp_make_fixnum(1)
#define STATE_SIZE sexp_make_fixnum(SEXP_RANDOM_STATE_SIZE)

#define sexp_random_source_p(x) sexp_check_tag(x, rs_type_id)

#define sexp_random_state(x) (sexp_slot_ref((x), 0))
#define sexp_random_data(x)  ((sexp_random_t*)(&sexp_slot_ref((x), 1)))

#define sexp_sizeof_random (sexp_sizeof_header + sizeof(sexp_random_t) + sizeof(sexp))

static sexp_uint_t rs_type_id = 0;
static sexp default_random_source;

#ifdef __GNU_LIBRARY__

typedef struct random_data sexp_random_t;

#define sexp_random_init(rs, seed)                                      \
  initstate_r(seed,                                                     \
              sexp_bytes_data(sexp_random_state(rs)),                   \
              SEXP_RANDOM_STATE_SIZE,                                   \
              sexp_random_data(rs))

#define sexp_call_random(rs, dst) random_r(sexp_random_data(rs), &dst)
#define sexp_seed_random(n, rs) srandom_r(n, sexp_random_data(rs))

#else

typedef unsigned int sexp_random_t;

#define sexp_random_init(rs, seed) *sexp_random_data(rs) = (seed)

#define sexp_call_random(rs, dst) ((dst) = rand_r(sexp_random_data(rs)))
#define sexp_seed_random(n, rs) *sexp_random_data(rs) = (n)

#endif

static sexp sexp_rs_random_integer (sexp ctx, sexp self, sexp_sint_t n, sexp rs, sexp bound) {
  sexp res;
  sexp_int32_t m;
#if SEXP_USE_BIGNUMS
  sexp_uint32_t mod;
  sexp_int32_t hi, len, i, *data;
#endif
  if (! sexp_random_source_p(rs))
    res = sexp_type_exception(ctx, self, rs_type_id, rs);
  if (sexp_fixnump(bound)) {
    sexp_call_random(rs, m);
    res = sexp_make_fixnum(m % sexp_unbox_fixnum(bound));
#if SEXP_USE_BIGNUMS
  } else if (sexp_bignump(bound)) {
    hi = sexp_bignum_hi(bound);
    len = hi * (sizeof(sexp_uint_t) / sizeof(sexp_int32_t));
    res = sexp_make_bignum(ctx, hi + 1);
    data = (sexp_int32_t*) sexp_bignum_data(res);
    for (i=0; i<len-1; i++) {
      sexp_call_random(rs, m);
      data[i] = m;
    }
    /* Scan down, modding bigits > bound to < bound, and stop as */
    /* soon as we are sure the result is within bound. */
    for (i = hi-1; i >= 0; i--) {
      mod = sexp_bignum_data(bound)[i];
      if (mod && sexp_bignum_data(res)[i] > mod) {
        sexp_bignum_data(res)[i] %= mod;
      }
      if (sexp_bignum_data(res)[i] != mod) {
        break;
      }
    }
#endif
  } else {
    res = sexp_type_exception(ctx, self, SEXP_FIXNUM, bound);
  }
  return res;
}

static sexp sexp_random_integer (sexp ctx, sexp self, sexp_sint_t n, sexp bound) {
  return sexp_rs_random_integer(ctx, self, n, default_random_source, bound);
}

static sexp sexp_rs_random_real (sexp ctx, sexp self, sexp_sint_t n, sexp rs) {
  sexp_int32_t res;
  if (! sexp_random_source_p(rs))
    return sexp_type_exception(ctx, self, rs_type_id, rs);
  sexp_call_random(rs, res);
  return sexp_make_flonum(ctx, (double)res / (double)RAND_MAX);
}

static sexp sexp_random_real (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_rs_random_real(ctx, self, n, default_random_source);
}

#if SEXP_BSD || defined(__CYGWIN__)

static sexp sexp_make_random_source (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res;
  res = sexp_alloc_tagged(ctx, sexp_sizeof_random, rs_type_id);
  *sexp_random_data(res) = 1;
  return res;
}

static sexp sexp_random_source_state_ref (sexp ctx, sexp self, sexp_sint_t n, sexp rs) {
  if (! sexp_random_source_p(rs))
    return sexp_type_exception(ctx, self, rs_type_id, rs);
  else
    return sexp_make_integer(ctx, *sexp_random_data(rs));
}

static sexp sexp_random_source_state_set (sexp ctx, sexp self, sexp_sint_t n, sexp rs, sexp state) {
  if (! sexp_random_source_p(rs))
    return sexp_type_exception(ctx, self, rs_type_id, rs);
  else if (sexp_fixnump(state))
    *sexp_random_data(rs) = sexp_unbox_fixnum(state);
#if SEXP_USE_BIGNUMS
  else if (sexp_bignump(state))
    *sexp_random_data(rs)
      = sexp_bignum_data(state)[0]*sexp_bignum_sign(state);
#endif
  else
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, state);
  return SEXP_VOID;
}

#else

static sexp sexp_make_random_source (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res;
  sexp_gc_var1(state);
  sexp_gc_preserve1(ctx, state);
  state = sexp_make_bytes(ctx, STATE_SIZE, SEXP_UNDEF);
  res = sexp_alloc_tagged(ctx, sexp_sizeof_random, rs_type_id);
  if (sexp_exceptionp(res)) return res;
  sexp_random_state(res) = state;
  sexp_random_init(res, 1);
  sexp_gc_release1(ctx);
  return res;
}

static sexp sexp_random_source_state_ref (sexp ctx, sexp self, sexp_sint_t n, sexp rs) {
  if (! sexp_random_source_p(rs))
    return sexp_type_exception(ctx, self, rs_type_id, rs);
  else
    return sexp_subbytes(ctx, sexp_random_state(rs), ZERO, STATE_SIZE);
}

static sexp sexp_random_source_state_set (sexp ctx, sexp self, sexp_sint_t n, sexp rs, sexp state) {
  if (! sexp_random_source_p(rs))
    return sexp_type_exception(ctx, self, rs_type_id, rs);
  else if (! (sexp_bytesp(state)
              && (sexp_bytes_length(state) == SEXP_RANDOM_STATE_SIZE)))
    return sexp_type_exception(ctx, self, SEXP_BYTES, state);
  sexp_random_state(rs) = sexp_subbytes(ctx, state, ZERO, STATE_SIZE);
  sexp_random_init(rs, 1);
  return SEXP_VOID;
}

#endif

static sexp sexp_random_source_randomize (sexp ctx, sexp self, sexp_sint_t n, sexp rs) {
  if (! sexp_random_source_p(rs))
    return sexp_type_exception(ctx, self, rs_type_id, rs);
  sexp_seed_random(time(NULL), rs);
  return SEXP_VOID;
}

static sexp sexp_random_source_pseudo_randomize (sexp ctx, sexp self, sexp_sint_t n, sexp rs, sexp seed1, sexp seed2) {
  if (! sexp_random_source_p(rs))
    return sexp_type_exception(ctx, self, rs_type_id, rs);
  if (! sexp_fixnump(seed1))
    return sexp_type_exception(ctx, self, rs_type_id, seed1);
  if (! sexp_fixnump(seed2))
    return sexp_type_exception(ctx, self, rs_type_id, seed2);
  sexp_seed_random(sexp_unbox_fixnum(seed1) ^ sexp_unbox_fixnum(seed2), rs);
  return SEXP_VOID;
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  sexp_gc_var2(name, op);
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_gc_preserve2(ctx, name, op);

  name = sexp_c_string(ctx, "random-source", -1);
  op = sexp_register_type(ctx, name, SEXP_FALSE, SEXP_FALSE,
                          sexp_make_fixnum(sexp_offsetof_slot0),
                          ONE, ONE, ZERO, ZERO,
                          sexp_make_fixnum(sexp_sizeof_random), ZERO,
                          ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, NULL, NULL);
  if (sexp_exceptionp(op))
    return op;
  rs_type_id = sexp_type_tag(op);

  name = sexp_c_string(ctx, "random-source?", -1);
  op = sexp_make_type_predicate(ctx, name, sexp_make_fixnum(rs_type_id));
  name = sexp_intern(ctx, "random-source?", -1);
  sexp_env_define(ctx, env, name, op);

  sexp_define_foreign(ctx, env, "make-random-source", 0, sexp_make_random_source);
  sexp_define_foreign(ctx, env, "%random-integer", 2, sexp_rs_random_integer);
  sexp_define_foreign(ctx, env, "random-integer", 1, sexp_random_integer);
  sexp_define_foreign(ctx, env, "%random-real", 1, sexp_rs_random_real);
  sexp_define_foreign(ctx, env, "random-real", 0, sexp_random_real);
  sexp_define_foreign(ctx, env, "random-source-state-ref", 1, sexp_random_source_state_ref);
  sexp_define_foreign(ctx, env, "random-source-state-set!", 2, sexp_random_source_state_set);
  sexp_define_foreign(ctx, env, "random-source-randomize!", 1, sexp_random_source_randomize);
  sexp_define_foreign(ctx, env, "random-source-pseudo-randomize!", 3, sexp_random_source_pseudo_randomize);

  default_random_source = op = sexp_make_random_source(ctx, NULL, 0);
  name = sexp_intern(ctx, "default-random-source", -1);
  sexp_env_define(ctx, env, name, default_random_source);
  sexp_random_source_randomize(ctx, NULL, 0, default_random_source);

  sexp_gc_release2(ctx);
  return SEXP_VOID;
}

