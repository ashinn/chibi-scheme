
#include <chibi/eval.h>

#if SEXP_64_BIT
#define sexp_heap_align(n) sexp_align(n, 5)
#else
#define sexp_heap_align(n) sexp_align(n, 4)
#endif

extern sexp sexp_gc (sexp ctx, size_t *sum_freed);
extern sexp_uint_t sexp_allocated_bytes (sexp x);

static sexp sexp_heap_stats (sexp ctx) {
  size_t freed;
  sexp_uint_t stats[256], hi_type=0, i;
  sexp_heap h = sexp_context_heap(ctx);
  sexp p;
  sexp_free_list q, r;
  char *end;
  sexp_gc_var3(res, tmp, name);

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
      /* otherwise increment the stat and continue */
      stats[sexp_pointer_tag(p)]++;
      if (sexp_pointer_tag(p) > hi_type)
        hi_type = sexp_pointer_tag(p);
      p = (sexp) (((char*)p) + sexp_heap_align(sexp_allocated_bytes(p)));
    }
  }

  /* build and return results */
  sexp_gc_preserve3(ctx, res, tmp, name);
  res = SEXP_NULL;
  for (i=hi_type; i>0; i--)
    if (stats[i]) {
      name = sexp_intern(ctx, sexp_type_name_by_index(i));
      tmp = sexp_cons(ctx, name, sexp_make_fixnum(stats[i]));
      res = sexp_cons(ctx, tmp, res);
    }
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_init_library (sexp ctx, sexp env) {
  sexp_define_foreign(ctx, env, "heap-stats", 0, sexp_heap_stats);
  return SEXP_VOID;
}

