/*  gc.c -- simple mark&sweep garbage collector          */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "chibi/sexp.h"

#define SEXP_INITIAL_HEAP_SIZE (2*1024*1024)
#define SEXP_MAXIMUM_HEAP_SIZE 0
#define SEXP_MINIMUM_OBJECT_SIZE (sexp_sizeof(pair))
#define SEXP_GROW_HEAP_RATIO 0.7

#define sexp_heap_align(n) sexp_align(n, 4)

typedef struct sexp_heap *sexp_heap;

struct sexp_heap {
  sexp_uint_t size;
  sexp free_list;
  sexp_heap next;
  char *data;
};

static sexp_heap heap;

#if USE_DEBUG_GC
static sexp* stack_base;
#endif

extern sexp continuation_resumer, final_resumer;

static sexp_heap sexp_heap_last (sexp_heap h) {
  while (h->next) h = h->next;
  return h;
}

sexp_uint_t sexp_allocated_bytes (sexp x) {
  sexp_uint_t res, *len_ptr;
  sexp t;
  if ((! sexp_pointerp(x)) || (sexp_pointer_tag(x) > SEXP_CONTEXT))
    return sexp_heap_align(1);
  t = &(sexp_type_specs[sexp_pointer_tag(x)]);
  len_ptr = (sexp_uint_t*) (((char*)x) + sexp_type_size_off(t));
  res = sexp_type_size_base(t) + len_ptr[0] * sexp_type_size_scale(t);
  return res;
}

void sexp_mark (sexp x) {
  sexp_uint_t *len_ptr;
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
  len_ptr = (sexp_uint_t*) (((char*)x) + sexp_type_field_len_off(t));
  len = sexp_type_field_len_base(t)
    + len_ptr[0]*sexp_type_field_len_scale(t) - 1;
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
  sexp_heap h = heap;
  sexp p, q, r;
  char *end;
  /* scan over the whole heap */
  for ( ; h; h=h->next) {
    p = (sexp) (h->data + sexp_heap_align(sexp_sizeof(pair)));
    q = h->free_list;
    end = (char*)h->data + h->size;
    while (((char*)p) < end) {
      /* find the preceding and succeeding free list pointers */
      for (r=sexp_cdr(q); r && sexp_pairp(r) && (r<p); q=r, r=sexp_cdr(r))
        ;
      if (r == p) {
        p = (sexp) (((char*)p) + (sexp_uint_t)sexp_car(p));
        continue;
      }
      size = sexp_heap_align(sexp_allocated_bytes(p));
      if ((! sexp_gc_mark(p)) && (! stack_references_pointer_p(ctx, p))) {
        sum_freed += size;
        if (((((char*)q)+(sexp_uint_t)sexp_car(q)) == (char*)p)
            && (q != h->free_list)) {
          /* merge q with p */
          if (r && sexp_pairp(r) && ((((char*)p)+size) == (char*)r)) {
            /* ... and with r */
            sexp_cdr(q) = sexp_cdr(r);
            freed = (sexp_uint_t)sexp_car(q) + size + (sexp_uint_t)sexp_car(r);
            p = (sexp) (((char*)p)+size+(sexp_uint_t)sexp_car(r));
          } else {
            freed = (sexp_uint_t)sexp_car(q) + size;
            p = (sexp) (((char*)p)+size);
          }
          sexp_car(q) = (sexp)freed;
        } else {
          if (r && sexp_pairp(r) && ((((char*)p)+size) == (char*)r)) {
            sexp_car(p) = (sexp)(size+(sexp_uint_t)sexp_car(r));
            sexp_cdr(p) = sexp_cdr(r);
            sexp_cdr(q) = p;
            freed = size + (sexp_uint_t)sexp_car(r);
          } else {
            sexp_car(p) = (sexp)size;
            sexp_cdr(p) = r;
            sexp_cdr(q) = p;
            freed = size;
          }
          sexp_pointer_tag(p) = SEXP_PAIR;
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
  return sexp_make_integer(max_freed);
}

sexp sexp_gc (sexp ctx, size_t *sum_freed) {
  sexp res;
  int i;
  sexp_mark(continuation_resumer);
  sexp_mark(final_resumer);
  for (i=0; i<SEXP_SYMBOL_TABLE_SIZE; i++)
    sexp_mark(sexp_symbol_table[i]);
  sexp_mark(ctx);
  res = sexp_sweep(ctx, sum_freed);
  return res;
}

sexp_heap sexp_make_heap (size_t size) {
  sexp free, next;
  sexp_heap h = (sexp_heap) malloc(sizeof(struct sexp_heap) + size);
  if (! h) {
    fprintf(stderr, "out of memory allocating %zu byte heap, aborting\n", size);
    exit(70);
  }
  h->size = size;
  h->data = (char*) sexp_heap_align((sexp_uint_t)&(h->data));
  free = h->free_list = (sexp) h->data;
  h->next = NULL;
  next = (sexp) ((char*)free + sexp_heap_align(sexp_sizeof(pair)));
  sexp_pointer_tag(free) = SEXP_PAIR;
  sexp_car(free) = 0; /* actually sexp_sizeof(pair) */
  sexp_cdr(free) = next;
  sexp_pointer_tag(next) = SEXP_PAIR;
  sexp_car(next) = (sexp) (size - sexp_heap_align(sexp_sizeof(pair)));
  sexp_cdr(next) = SEXP_NULL;
  return h;
}

int sexp_grow_heap (sexp ctx, size_t size) {
  size_t cur_size, new_size;
  sexp_heap h = sexp_heap_last(heap);
  cur_size = h->size;
  new_size = sexp_heap_align(((cur_size > size) ? cur_size : size) * 2);
  h->next = sexp_make_heap(new_size);
  return (h->next != NULL);
}

void* sexp_try_alloc (sexp ctx, size_t size) {
  sexp ls1, ls2, ls3;
  sexp_heap h;
  for (h=heap; h; h=h->next) {
    ls1 = h->free_list;
    ls2 = sexp_cdr(ls1);
    while (sexp_pairp(ls2)) {
      if ((sexp_uint_t)sexp_car(ls2) >= size) {
        if ((sexp_uint_t)sexp_car(ls2) >= (size + SEXP_MINIMUM_OBJECT_SIZE)) {
          ls3 = (sexp) (((char*)ls2)+size); /* the free tail after ls2 */
          sexp_pointer_tag(ls3) = SEXP_PAIR;
          sexp_car(ls3) = (sexp) (((sexp_uint_t)sexp_car(ls2)) - size);
          sexp_cdr(ls3) = sexp_cdr(ls2);
          sexp_cdr(ls1) = ls3;
        } else {                  /* take the whole chunk */
          sexp_cdr(ls1) = sexp_cdr(ls2);
        }
        bzero((void*)ls2, size);
        return ls2;
      }
      ls1 = ls2;
      ls2 = sexp_cdr(ls2);
    }
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
    max_freed = sexp_unbox_integer(sexp_gc(ctx, &sum_freed));
    h = sexp_heap_last(heap);
    if (((max_freed < size)
         || ((h->size - sum_freed) < (h->size*(1 - SEXP_GROW_HEAP_RATIO))))
        && ((! SEXP_MAXIMUM_HEAP_SIZE) || (size < SEXP_MAXIMUM_HEAP_SIZE)))
      sexp_grow_heap(ctx, size);
    res = sexp_try_alloc(ctx, size);
    if (! res) {
      fprintf(stderr, "out of memory allocating %zu bytes, aborting\n", size);
      exit(70);
    }
  }
  return res;
}

void sexp_gc_init () {
  sexp_uint_t size = sexp_heap_align(SEXP_INITIAL_HEAP_SIZE);
  heap = sexp_make_heap(size);
#if USE_DEBUG_GC
  /* the +32 is a hack, but this is just for debugging anyway */
  stack_base = ((sexp*)&size) + 32;
#endif
}

