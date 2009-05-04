/*  gc.c -- simple garbage collector                     */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include <sexp.h>

#define SEXP_INITIAL_HEAP_SIZE 10000000
#define SEXP_MINIMUM_OBJECT_SIZE (sexp_sizeof(flonum))

static char* sexp_heap;
static char* sexp_heap_end;
static sexp sexp_free_list;

sexp_uint_t sexp_allocated_bytes (sexp x) {
  switch (sexp_tag(x)) {
  case SEXP_PAIR: return sexp_sizeof(pair);
  case SEXP_SYMBOL: return sexp_sizeof(symbol);
  case SEXP_STRING: return sexp_sizeof(string)+sexp_string_length(x);
  case SEXP_VECTOR:
    return sexp_sizeof(vector)+(sexp_vector_length(x)*sizeof(sexp));
  case SEXP_FLONUM: return sexp_sizeof(flonum);
  case SEXP_BIGNUM: return sexp_sizeof(bignum);
  case SEXP_IPORT:
  case SEXP_OPORT: return sexp_sizeof(port);
  case SEXP_EXCEPTION: return sexp_sizeof(exception);
  case SEXP_PROCEDURE: return sexp_sizeof(procedure);
  case SEXP_MACRO: return sexp_sizeof(macro);
  case SEXP_SYNCLO: return sexp_sizeof(synclo);
  case SEXP_ENV: return sexp_sizeof(env);
  case SEXP_BYTECODE: return sexp_sizeof(bytecode)+sexp_bytecode_length(x);
  case SEXP_CORE: return sexp_sizeof(core);
  case SEXP_OPCODE: return sexp_sizeof(opcode);
  case SEXP_LAMBDA: return sexp_sizeof(lambda);
  case SEXP_CND: return sexp_sizeof(cnd);
  case SEXP_REF: return sexp_sizeof(ref);
  case SEXP_SET: return sexp_sizeof(set);
  case SEXP_SEQ: return sexp_sizeof(seq);
  case SEXP_LIT: return sexp_sizeof(lit);
  case SEXP_CONTEXT: return sexp_sizeof(context);
  default: return 0;
  }
}

void *sexp_alloc (sexp ctx, size_t size) {
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
  if (sexp_unbox_integer(sexp_gc(ctx)) >= size) {
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
  sexp_gc_mark(x) = 1;
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
  sexp p=(sexp)sexp_heap, f1=sexp_free_list, f2;
  while (p<sexp_heap_end) {
    for (f2=sexp_cdr(f1); sexp_pairp(f2) && (f2 < p); f1=f2, f2=sexp_cdr(f2))
      ;
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

sexp sexp_gc (sexp ctx) {
  int i;
  sexp ctx2, stack = sexp_context_stack(ctx);
  for (i=0; i<sexp_context_top(ctx); i++)
    sexp_mark(stack[i]);
  for ( ; ctx; ctx=sexp_context_(ctx)) {
    sexp_gc_mark(ctx) = 1;
    sexp_gc_mark(sexp_context_bc(ctx)) = 1;
    sexp_mark(sexp_context_env(ctx));
  }
  return sexp_sweep();
}

void sexp_gc_init () {
  sexp_heap = malloc(SEXP_INITIAL_HEAP_SIZE);
  sexp_heap_end = sexp_heap + SEXP_INITIAL_HEAP_SIZE;
  sexp_free_list = sexp_heap;
  sexp_car(sexp_free_list) = SEXP_INITIAL_HEAP_SIZE;
  sexp_cdr(sexp_free_list) = SEXP_NULL;
}

