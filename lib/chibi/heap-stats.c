/* heap-stats.c -- count or dump heap objects                */
/* Copyright (c) 2009-2011 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

#define SEXP_HEAP_VECTOR_DEPTH 1

#if ! SEXP_USE_BOEHM

extern sexp sexp_gc (sexp ctx, size_t *sum_freed);
extern sexp_uint_t sexp_allocated_bytes (sexp ctx, sexp x);

static void sexp_print_simple (sexp ctx, sexp x, sexp out, int depth) {
  int i;
  if ((!sexp_pointerp(x)) || sexp_symbolp(x) || sexp_stringp(x)
      || sexp_flonump(x) || sexp_bignump(x)) {
    sexp_write(ctx, x, out);
  } else if (depth <= 0) {
    goto print_name;
  } else if (sexp_synclop(x)) {
    sexp_write_string(ctx, "#<sc ", out);
    sexp_print_simple(ctx, sexp_synclo_expr(x), out, depth);
    sexp_write_string(ctx, ">", out);
  } else if (sexp_pairp(x)) {
    sexp_write_char(ctx, '(', out);
    sexp_print_simple(ctx, sexp_car(x), out, depth-1);
    sexp_write_string(ctx, " . ", out);
    sexp_print_simple(ctx, sexp_cdr(x), out, depth-1);
    sexp_write_char(ctx, ')', out);
  } else if (sexp_vectorp(x)) {
    sexp_write_string(ctx, "#(", out);
    for (i=0; i<SEXP_HEAP_VECTOR_DEPTH && i<sexp_vector_length(x); i++) {
      if (i>0)
        sexp_write_char(ctx, ' ', out);
      sexp_print_simple(ctx, sexp_vector_ref(x, i), out, depth-1);
    }
    if (i<sexp_vector_length(x))
      sexp_write_string(ctx, " ...", out);
    sexp_write_char(ctx, ')', out);
  } else {
  print_name:
    sexp_write_string(ctx, "#<", out);
    sexp_write(ctx, sexp_object_type_name(ctx, x), out);
    sexp_write_string(ctx, ">", out);
  }
}

static sexp sexp_heap_walk (sexp ctx, int depth, int printp) {
  size_t freed;
  sexp_uint_t stats[256], hi_type=0, i;
  sexp_heap h = sexp_context_heap(ctx);
  sexp p, out=SEXP_FALSE;
  sexp_free_list q, r;
  char *end;
  sexp_gc_var3(res, tmp, name);

  if (printp)
    out = sexp_parameter_ref(ctx,
                             sexp_env_ref(sexp_context_env(ctx),
                                          sexp_global(ctx,SEXP_G_CUR_OUT_SYMBOL),
                                          SEXP_FALSE));

  /* run gc once to remove unused variables */
  sexp_gc(ctx, &freed);

  /* initialize stats */
  for (i=0; i<256; i++) stats[i]=0;

  /* loop over each heap chunk */
  for ( ; h; h=h->next) {
    p = (sexp) (h->data + sexp_heap_align(sexp_sizeof(pair)));
    q = h->free_list;
    end = (char*)h->data + h->size;
    while (((char*)p) < end) {
      /* find the preceding and succeeding free list pointers */
      for (r=q->next; r && ((char*)r<(char*)p); q=r, r=r->next)
        ;
      if ((char*)r == (char*)p) { /* this is a free block, skip */
        p = (sexp) (((char*)p) + r->size);
        continue;
      }
      /* otherwise maybe print, then increment the stat and continue */
      if (sexp_oportp(out)) {
        sexp_print_simple(ctx, p, out, depth);
        sexp_write_char(ctx, '\n', out);
      }
      stats[sexp_pointer_tag(p)]++;
      if (sexp_pointer_tag(p) > hi_type)
        hi_type = sexp_pointer_tag(p);
      p = (sexp) (((char*)p) + sexp_heap_align(sexp_allocated_bytes(ctx, p)));
    }
  }

  /* build and return results */
  sexp_gc_preserve3(ctx, res, tmp, name);
  res = SEXP_NULL;
  for (i=hi_type; i>0; i--)
    if (stats[i]) {
      name = sexp_string_to_symbol(ctx, sexp_type_name_by_index(ctx, i));
      tmp = sexp_cons(ctx, name, sexp_make_fixnum(stats[i]));
      res = sexp_cons(ctx, tmp, res);
    }
  sexp_gc_release3(ctx);
  return res;
}

static sexp sexp_heap_stats (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_heap_walk(ctx, 0, 0);
}

static sexp sexp_heap_dump (sexp ctx, sexp self, sexp_sint_t n, sexp depth) {
  if (! sexp_fixnump(depth) || (sexp_unbox_fixnum(depth) < 0))
    return sexp_xtype_exception(ctx, self, "bad heap-dump depth", depth);
  return sexp_heap_walk(ctx, sexp_unbox_fixnum(depth), 1);
}

#else

static sexp sexp_heap_stats (sexp ctx, sexp self, sexp_sint_t n) {
  return SEXP_NULL;
}

static sexp sexp_heap_dump (sexp ctx, sexp self, sexp_sint_t n, sexp depth) {
  return SEXP_NULL;
}

#endif

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return sexp_global(ctx, SEXP_G_ABI_ERROR);
  sexp_define_foreign(ctx, env, "heap-stats", 0, sexp_heap_stats);
  sexp_define_foreign_opt(ctx, env, "heap-dump", 1, sexp_heap_dump, SEXP_ONE);
  return SEXP_VOID;
}
