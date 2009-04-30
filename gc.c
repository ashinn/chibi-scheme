/*  gc.c -- simple garbage collector                     */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include <sexp.h>

#define SEXP_INITIAL_HEAP_SIZE 10000000
#define SEXP_MINIMUM_OBJECT_SIZE (sexp_sizeof(flonum))

static char* sexp_heap;
static char* sexp_heap_end;
static sexp sexp_free_list;

void *sexp_alloc (size_t size) {
  sexp ls1, ls2, ls3;
 try_alloc:
  ls1=sexp_free_list;
  for (ls2=sexp_cdr(ls1); sexp_pairp(ls2); ls1=ls2, ls2=sexp_cdr(ls2))
    if (sexp_car(ls2) >= size) {
      if (sexp_car(ls2) >= size + SEXP_MINIMUM_OBJECT_SIZE) {
        ls3 = (sexp) (((char*)ls2)+size);
        sexp_car(ls3) = (sexp) (sexp_car(ls2) - size);
        sexp_cdr(ls3) = sexp_cdr(ls2);
        sexp_cdr(ls1) = sexp_cdr(ls3);
      } else {
        sexp_cdr(ls1) = sexp_cdr(ls2);
      }
      return ls2;
    }
  if (sexp_unbox_integer(sexp_gc()) >= size) {
    goto try_alloc;
  } else {
    fprintf(stderr, "chibi: out of memory trying to allocate %ld bytes, aborting\n", size);
    exit(70);
  }
}

void sexp_mark (sexp x) {
  sexp *data;
  sexp_uint_t i;
 loop:
  if ((! sexp_pointerp(x)) || sexp_mark(x))
    return;
  sexp_mark(x) = 1;
  switch (sexp_tag(x)) {
  case SEXP_PAIR:
    sexp_mark(sexp_car(x));
    x = sexp_cdr(x);
    goto loop;
  case SEXP_VECTOR:
    data = sexp_vector_data(x);
    for (i=sexp_vector_length(x)-1; i>=0; i--)
      sexp_mark(data[i]);
  }
}

sexp sexp_sweep () {
  sexp_uint_t freed=0, size;
  sexp p=(sexp)sexp_heap, f=sexp_free_list;
  /* XXXX make p skip over areas already in the free_list */
  while (p<sexp_heap_end) {
    size = sexp_allocated_bytes(p);
    if (! sexp_mark(p)) {
      freed += size;
      sexp_car(p) = (sexp)size;
      sexp_cdr(p) = f;
      f = p;
    } else {
      sexp_mark(p) = 0;
    }
    p += size;
  }
  sexp_free_list = f;
  return sexp_make_integer(freed);
}

sexp sexp_gc () {
  /* XXXX change FFI to pass context for marking */
  return sexp_sweep();
}

void sexp_gc_init () {
  sexp_heap = malloc(SEXP_INITIAL_HEAP_SIZE);
  sexp_heap_end = sexp_heap + SEXP_INITIAL_HEAP_SIZE;
  sexp_free_list = sexp_heap;
  sexp_car(sexp_free_list) = SEXP_INITIAL_HEAP_SIZE;
  sexp_cdr(sexp_free_list) = SEXP_NULL;
}

