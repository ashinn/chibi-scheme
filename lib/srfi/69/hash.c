
#include <chibi/eval.h>

#define HASH_DEPTH 5
#define HASH_BOUND sexp_make_fixnum(SEXP_MAX_FIXNUM)

#define FNV_PRIME 16777619
#define FNV_OFFSET_BASIS 2166136261uL

#define sexp_hash_table_buckets(x)  sexp_slot_ref(x, 0)
#define sexp_hash_table_size(x)     sexp_slot_ref(x, 1)
#define sexp_hash_table_hash_fn(x)  sexp_slot_ref(x, 2)
#define sexp_hash_table_eq_fn(x)    sexp_slot_ref(x, 3)

#define sexp_hash_resize_check(n, len) (((n)*3) > ((len)>>2))

static sexp_uint_t string_hash (char *str, sexp_uint_t bound) {
  sexp_uint_t acc = FNV_OFFSET_BASIS;
  while (*str) {acc *= FNV_PRIME; acc ^= *str++;}
  return acc % bound;
}

static sexp sexp_string_hash (sexp ctx, sexp str, sexp bound) {
  return sexp_make_fixnum(string_hash(sexp_string_data(str),
                                      sexp_unbox_fixnum(bound)));
}

static sexp_uint_t string_ci_hash (char *str, sexp_uint_t bound) {
  sexp_uint_t acc = FNV_OFFSET_BASIS;
  while (*str) {acc *= FNV_PRIME; acc ^= tolower(*str++);}
  return acc % bound;
}

static sexp sexp_string_ci_hash (sexp ctx, sexp str, sexp bound) {
  return sexp_make_fixnum(string_ci_hash(sexp_string_data(str),
                                         sexp_unbox_fixnum(bound)));
}

static sexp_uint_t hash_one (sexp obj, sexp_uint_t bound, sexp_sint_t depth) {
  sexp_uint_t acc = FNV_OFFSET_BASIS, size;
  sexp_sint_t i, len;
  sexp t, *p;
  char *p0;
 loop:
#if SEXP_USE_FLONUMS
  if (sexp_flonump(obj))
    acc ^= (sexp_sint_t) sexp_flonum_value(obj);
  else
#endif
  if (sexp_pointerp(obj)) {
    if (depth) {
      t = &(sexp_type_specs[sexp_pointer_tag(obj)]);
      p = (sexp*) (((char*)obj) + sexp_type_field_base(t));
      p0 = ((char*)obj) + offsetof(struct sexp_struct, value);
      if ((sexp)p == obj) p=(sexp*)p0;
      /* hash trailing non-object data */
      size = sexp_type_size_of_object(t, obj)-offsetof(struct sexp_struct, value);
      p0 = ((char*)p + sexp_type_num_slots_of_object(t,obj)*sizeof(sexp));
      if (((char*)obj + size) > p0)
        for (i=0; i<size; i++) {acc *= FNV_PRIME; acc ^= p0[i];}
      /* hash eq-object slots */
      len = sexp_type_num_eq_slots_of_object(t, obj);
      if (len > 0) {
        depth--;
        for (i=0; i<len-1; i++) {
          acc *= FNV_PRIME;
          acc ^= hash_one(p[i], 0, depth);
        }
        /* tail-recurse on the last value */
        obj = p[len-1]; goto loop;
      }
    } else {
      acc ^= sexp_pointer_tag(obj);
    }
  } else {
    acc ^= (sexp_uint_t)obj;
  }
  return (bound ? acc % bound : acc);
}

static sexp_uint_t hash (sexp obj, sexp_uint_t bound) {
  return hash_one(obj, bound, HASH_DEPTH);
}

static sexp sexp_hash (sexp ctx, sexp obj, sexp bound) {
  return sexp_make_fixnum(hash(obj, sexp_unbox_fixnum(bound)));
}

static sexp sexp_hash_by_identity (sexp ctx, sexp obj, sexp bound) {
  return sexp_make_fixnum((sexp_uint_t)obj % sexp_unbox_fixnum(bound));
}

static sexp sexp_get_bucket (sexp ctx, sexp ht, sexp obj) {
  sexp_gc_var1(args);
  sexp buckets = sexp_hash_table_buckets(ht), hash_fn, res;
  sexp_uint_t len = sexp_vector_length(buckets);
  hash_fn = sexp_hash_table_hash_fn(ht);
  if (hash_fn == sexp_make_fixnum(1))
    res = sexp_hash_by_identity(ctx, obj, sexp_make_fixnum(len));
  else if (hash_fn == sexp_make_fixnum(2))
    res = sexp_hash(ctx, obj, sexp_make_fixnum(len));
  else {
    sexp_gc_preserve1(ctx, args);
    args = sexp_list2(ctx, obj, sexp_make_fixnum(len));
    res = sexp_apply(ctx, hash_fn, args);
    if (sexp_exceptionp(res)) {
      args = sexp_eval_string(ctx, "(current-error-port)", sexp_context_env(ctx));
      sexp_print_exception(ctx, res, args);
      res = sexp_make_fixnum(0);
    }
    sexp_gc_release1(ctx);
  }
  return res;
}

static sexp sexp_scan_bucket (sexp ctx, sexp ls, sexp obj, sexp eq_fn) {
  sexp_gc_var1(res);
  sexp p;
  res = SEXP_FALSE;
  if ((eq_fn == sexp_make_fixnum(1))
      || ((eq_fn == sexp_make_fixnum(2))
          && (sexp_pointerp(obj) ?
              (sexp_pointer_tag(obj) == SEXP_SYMBOL) : ! sexp_fixnump(obj)))) {
    for (p=ls; sexp_pairp(p); p=sexp_cdr(p)) {
      if (sexp_caar(p) == obj) {
        res = p;
        break;
      }
    }
  } else if (eq_fn == sexp_make_fixnum(2)) {
    for (p=ls; sexp_pairp(p); p=sexp_cdr(p)) {
      if (sexp_truep(sexp_equalp(ctx, sexp_caar(p), obj))) {
        res = p;
        break;
      }
    }
  } else {
    sexp_gc_preserve1(ctx, res);
    for (p=ls; sexp_pairp(p); p=sexp_cdr(p)) {
      res = sexp_list2(ctx, sexp_caar(p), obj);
      if (sexp_truep(sexp_apply(ctx, eq_fn, res))) {
        res = p;
        break;
      } else {
        res = SEXP_FALSE;
      }
    }
    sexp_gc_release1(ctx);
  }
  return res;
}

/* static sexp sexp_regrow_hash_table (sexp ctx, sexp ht) { */
/* } */

static sexp sexp_hash_table_cell (sexp ctx, sexp ht, sexp obj, sexp createp) {
  sexp_gc_var1(res);
  sexp_uint_t size;
  sexp buckets=sexp_hash_table_buckets(ht), eq_fn=sexp_hash_table_eq_fn(ht),
    i=sexp_get_bucket(ctx, ht, obj);
  res = sexp_scan_bucket(ctx, sexp_vector_ref(buckets, i), obj, eq_fn);
  if (sexp_truep(res)) {
    res = sexp_car(res);
  } else if (sexp_truep(createp)) {
    sexp_gc_preserve1(ctx, res);
    size = sexp_unbox_fixnum(sexp_hash_table_size(ht));
    /* if (sexp_hash_resize_check(size, sexp_vector_length(buckets))) { */
    /*   sexp_regrow_hash_table(ctx, ht); */
    /*   buckets = sexp_hash_table_buckets(ht); */
    /*   i = sexp_get_bucket(ctx, ht, obj); */
    /* } */
    res = sexp_cons(ctx, obj, createp);
    sexp_vector_set(buckets, i, sexp_cons(ctx, res, sexp_vector_ref(buckets, i)));
    sexp_hash_table_size(ht) = sexp_make_fixnum(size+1);
    sexp_gc_release1(ctx);
  }
  return res;
}

static sexp sexp_hash_table_delete (sexp ctx, sexp ht, sexp obj) {
  sexp buckets=sexp_hash_table_buckets(ht), eq_fn=sexp_hash_table_eq_fn(ht),
    i=sexp_get_bucket(ctx, ht, obj), p, res;
  res = sexp_scan_bucket(ctx, sexp_vector_ref(buckets, i), obj, eq_fn);
  if (sexp_pairp(res)) {
    sexp_hash_table_size(ht)
      = sexp_fx_sub(sexp_hash_table_size(ht), sexp_make_fixnum(1));
    if (res == sexp_vector_ref(buckets, i)) {
      sexp_vector_set(buckets, i, sexp_cdr(res));
    } else {
      for (p=sexp_vector_ref(buckets, i); sexp_cdr(p)!=res; p=sexp_cdr(p))
        ;
      sexp_cdr(p) = sexp_cdr(res);
    }
  }
  return SEXP_VOID;
}

sexp sexp_init_library (sexp ctx, sexp env) {

  sexp_define_foreign_opt(ctx, env, "string-hash", 2, sexp_string_hash, HASH_BOUND);
  sexp_define_foreign_opt(ctx, env, "string-ci-hash", 2, sexp_string_ci_hash, HASH_BOUND);
  sexp_define_foreign_opt(ctx, env, "hash", 2, sexp_hash, HASH_BOUND);
  sexp_define_foreign_opt(ctx, env, "hash-by-identity", 2, sexp_hash_by_identity, HASH_BOUND);
  sexp_define_foreign(ctx, env, "hash-table-cell", 3, sexp_hash_table_cell);
  sexp_define_foreign(ctx, env, "hash-table-delete!", 2, sexp_hash_table_delete);

  return SEXP_VOID;
}

