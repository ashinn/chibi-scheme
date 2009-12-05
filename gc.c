/*  gc.c -- simple mark&sweep garbage collector          */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "chibi/sexp.h"

/* These settings are configurable but only recommended for */
/* experienced users, so they're not in config.h.  */

/* the initial heap size in bytes */
#ifndef SEXP_INITIAL_HEAP_SIZE
#define SEXP_INITIAL_HEAP_SIZE (2*1024*1024)
#endif

/* the maximum heap size in bytes - if 0 there is no limit */
#ifndef SEXP_MAXIMUM_HEAP_SIZE
#define SEXP_MAXIMUM_HEAP_SIZE (4*1024*1024)
#endif

/* if after GC more than this percentage of memory is still in use, */
/* and we've not exceeded the maximum size, grow the heap */
#ifndef SEXP_GROW_HEAP_RATIO
#define SEXP_GROW_HEAP_RATIO 0.75
#endif

#define SEXP_MINIMUM_OBJECT_SIZE (sexp_sizeof(pair))

#if SEXP_64_BIT
#define sexp_heap_align(n) sexp_align(n, 5)
#else
#define sexp_heap_align(n) sexp_align(n, 4)
#endif

#if USE_GLOBAL_HEAP
static sexp_heap sexp_global_heap;
#endif

#if USE_DEBUG_GC
static sexp* stack_base;
#endif

static sexp_heap sexp_heap_last (sexp_heap h) {
  while (h->next) h = h->next;
  return h;
}

sexp_uint_t sexp_allocated_bytes (sexp x) {
  sexp_uint_t res;
  sexp t;
  if ((! sexp_pointerp(x)) || (sexp_pointer_tag(x) >= sexp_num_types))
    return sexp_heap_align(1);
  t = &(sexp_type_specs[sexp_pointer_tag(x)]);
  res = sexp_type_size_of_object(t, x);
  return res;
}

void sexp_mark (sexp x) {
  sexp_sint_t i, len;
  sexp t, *p;
  struct sexp_gc_var_t *saves;
 loop:
  if ((! x) || (! sexp_pointerp(x)) || sexp_gc_mark(x))
    return;
  sexp_gc_mark(x) = 1;
  if (sexp_contextp(x))
    for (saves=sexp_context_saves(x); saves; saves=saves->next)
      if (saves->var) sexp_mark(*(saves->var));
  t = &(sexp_type_specs[sexp_pointer_tag(x)]);
  p = (sexp*) (((char*)x) + sexp_type_field_base(t));
  len = sexp_type_num_slots_of_object(t, x) - 1;
  if (len >= 0) {
    for (i=0; i<len; i++)
      sexp_mark(p[i]);
    x = p[len];
    goto loop;
  }
}

#if USE_DEBUG_GC
int stack_references_pointer_p (sexp ctx, sexp x) {
  sexp *p;
  for (p=(&x)+1; p<stack_base; p++)
    if (*p == x)
      return 1;
  return 0;
}
#else
#define stack_references_pointer_p(ctx, x) 0
#endif

sexp sexp_sweep (sexp ctx, size_t *sum_freed_ptr) {
  size_t freed, max_freed=0, sum_freed=0, size;
  sexp_heap h = sexp_context_heap(ctx);
  sexp p;
  sexp_free_list q, r, s;
  char *end;
  sexp_proc2 finalizer;
  /* scan over the whole heap */
  for ( ; h; h=h->next) {
    p = (sexp) (h->data + sexp_heap_align(sexp_sizeof(pair)));
    q = h->free_list;
    end = (char*)h->data + h->size;
    while (((char*)p) < end) {
      /* find the preceding and succeeding free list pointers */
      for (r=q->next; r && ((char*)r<(char*)p); q=r, r=r->next)
        ;
      if ((char*)r == (char*)p) {
        p = (sexp) (((char*)p) + r->size);
        continue;
      }
      size = sexp_heap_align(sexp_allocated_bytes(p));
      if ((! sexp_gc_mark(p)) && (! stack_references_pointer_p(ctx, p))) {
        /* free p */
        finalizer = sexp_type_finalize(sexp_object_type(p));
        if (finalizer) finalizer(ctx, p);
        sum_freed += size;
        if (((((char*)q) + q->size) == (char*)p) && (q != h->free_list)) {
          /* merge q with p */
          if (r && ((((char*)p)+size) == (char*)r)) {
            /* ... and with r */
            q->next = r->next;
            freed = q->size + size + r->size;
            p = (sexp) (((char*)p) + size + r->size);
          } else {
            freed = q->size + size;
            p = (sexp) (((char*)p)+size);
          }
          q->size = freed;
        } else {
          s = (sexp_free_list)p;
          if (r && ((((char*)p)+size) == (char*)r)) {
            /* merge p with r */
            s->size = size + r->size;
            s->next = r->next;
            q->next = s;
            freed = size + r->size;
          } else {
            s->size = size;
            s->next = r;
            q->next = s;
            freed = size;
          }
          p = (sexp) (((char*)p)+freed);
        }
        if (freed > max_freed)
          max_freed = freed;
      } else {
        sexp_gc_mark(p) = 0;
        p = (sexp) (((char*)p)+size);
      }
    }
  }
  sum_freed_ptr[0] = sum_freed;
  return sexp_make_fixnum(max_freed);
}

sexp sexp_gc (sexp ctx, size_t *sum_freed) {
  sexp res;
#if USE_GLOBAL_SYMBOLS
  int i;
  for (i=0; i<SEXP_SYMBOL_TABLE_SIZE; i++)
    sexp_mark(sexp_symbol_table[i]);
#endif
  sexp_mark(ctx);
  res = sexp_sweep(ctx, sum_freed);
  return res;
}

sexp_heap sexp_make_heap (size_t size) {
  sexp_free_list free, next;
  sexp_heap h = (sexp_heap) malloc(sizeof(struct sexp_heap) + size);
  if (! h)
    errx(70, "out of memory allocating %zu byte heap, aborting\n", size);
  h->size = size;
  h->data = (char*) sexp_heap_align((sexp_uint_t)&(h->data));
  free = h->free_list = (sexp_free_list) h->data;
  h->next = NULL;
  next = (sexp_free_list) ((char*)free + sexp_heap_align(sexp_sizeof(pair)));
  free->size = 0; /* actually sexp_sizeof(pair) */
  free->next = next;
  next->size = size - sexp_heap_align(sexp_sizeof(pair));
  next->next = NULL;
  return h;
}

int sexp_grow_heap (sexp ctx, size_t size) {
  size_t cur_size, new_size;
  sexp_heap h = sexp_heap_last(sexp_context_heap(ctx));
  cur_size = h->size;
  new_size = sexp_heap_align(((cur_size > size) ? cur_size : size) * 2);
  h->next = sexp_make_heap(new_size);
  return (h->next != NULL);
}

void* sexp_try_alloc (sexp ctx, size_t size) {
  sexp_free_list ls1, ls2, ls3;
  sexp_heap h;
  for (h=sexp_context_heap(ctx); h; h=h->next)
    for (ls1=h->free_list, ls2=ls1->next; ls2; ls1=ls2, ls2=ls2->next)
      if (ls2->size >= size) {
        if (ls2->size >= (size + SEXP_MINIMUM_OBJECT_SIZE)) {
          ls3 = (sexp_free_list) (((char*)ls2)+size); /* the tail after ls2 */
          ls3->size = ls2->size - size;
          ls3->next = ls2->next;
          ls1->next = ls3;
        } else {                  /* take the whole chunk */
          ls1->next = ls2->next;
        }
        memset((void*)ls2, 0, size);
        return ls2;
      }
  return NULL;
}

void* sexp_alloc (sexp ctx, size_t size) {
  void *res;
  size_t max_freed, sum_freed;
  sexp_heap h;
  size = sexp_heap_align(size);
  res = sexp_try_alloc(ctx, size);
  if (! res) {
    max_freed = sexp_unbox_fixnum(sexp_gc(ctx, &sum_freed));
    h = sexp_heap_last(sexp_context_heap(ctx));
    if (((max_freed < size)
         || ((h->size - sum_freed) > (h->size*SEXP_GROW_HEAP_RATIO)))
        && ((! SEXP_MAXIMUM_HEAP_SIZE) || (size < SEXP_MAXIMUM_HEAP_SIZE)))
      sexp_grow_heap(ctx, size);
    res = sexp_try_alloc(ctx, size);
    if (! res)
      res = sexp_global(ctx, SEXP_G_OOM_ERROR);
  }
  return res;
}

void sexp_gc_init (void) {
#if USE_GLOBAL_HEAP || USE_DEBUG_GC
  sexp_uint_t size = sexp_heap_align(SEXP_INITIAL_HEAP_SIZE);
#endif
#if USE_GLOBAL_HEAP
  sexp_global_heap = sexp_make_heap(size);
#endif
#if USE_DEBUG_GC
  /* the +32 is a hack, but this is just for debugging anyway */
  stack_base = ((sexp*)&size) + 32;
#endif
}

