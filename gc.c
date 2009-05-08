/*  gc.c -- simple garbage collector                     */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "sexp.h"

#define SEXP_INITIAL_HEAP_SIZE 100000000
#define SEXP_MINIMUM_OBJECT_SIZE (sexp_sizeof(flonum))

static char* sexp_heap;
static char* sexp_heap_end;
static sexp sexp_free_list;

sexp_uint_t sexp_allocated_bytes (sexp x) {
  switch (sexp_pointer_tag(x)) {
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

void sexp_mark (sexp x) {
  sexp *data;
  sexp_uint_t i;
 loop:
  if ((! sexp_pointerp(x)) || sexp_gc_mark(x))
    return;
  sexp_gc_mark(x) = 1;
  switch (sexp_pointer_tag(x)) {
  case SEXP_PAIR:
    sexp_mark(sexp_car(x));
    x = sexp_cdr(x);
    goto loop;
  case SEXP_VECTOR:
    data = sexp_vector_data(x);
    for (i=sexp_vector_length(x)-1; i>0; i--)
      sexp_mark(data[i]);
    x = data[i];
    goto loop;
  case SEXP_BYTECODE:
    x = sexp_bytecode_literals(x);
    goto loop;
  case SEXP_ENV:
    sexp_mark(sexp_env_lambda(x));
    sexp_mark(sexp_env_bindings(x));
    x = sexp_env_parent(x);
    if (x) goto loop; else break;
  case SEXP_PROCEDURE:
    sexp_mark(sexp_procedure_code(x));
    x = sexp_procedure_vars(x);
    goto loop;
  case SEXP_MACRO:
    sexp_mark(sexp_macro_proc(x));
    x = sexp_macro_env(x);
    goto loop;
  case SEXP_SYNCLO:
    sexp_mark(sexp_synclo_free_vars(x));
    sexp_mark(sexp_synclo_expr(x));
    x = sexp_synclo_env(x);
    goto loop;
  case SEXP_OPCODE:
    if (sexp_opcode_proc(x)) sexp_mark(sexp_opcode_proc(x));
    if (sexp_opcode_default(x)) sexp_mark(sexp_opcode_default(x));
    if (sexp_opcode_data(x)) sexp_mark(sexp_opcode_data(x));
    break;
  case SEXP_IPORT:
  case SEXP_OPORT:
    x = sexp_port_cookie(x);
    if (x) goto loop; else break;
  case SEXP_LAMBDA:
    sexp_mark(sexp_lambda_name(x));
    sexp_mark(sexp_lambda_params(x));
    sexp_mark(sexp_lambda_locals(x));
    sexp_mark(sexp_lambda_defs(x));
    sexp_mark(sexp_lambda_flags(x));
    sexp_mark(sexp_lambda_body(x));
    sexp_mark(sexp_lambda_fv(x));
    sexp_mark(sexp_lambda_sv(x));
    x = sexp_lambda_body(x);
    goto loop;
  case SEXP_CND:
    sexp_mark(sexp_cnd_test(x));
    sexp_mark(sexp_cnd_fail(x));
    x = sexp_cnd_pass(x);
    goto loop;
  case SEXP_SET:
    sexp_mark(sexp_set_var(x));
    x = sexp_set_value(x);
    goto loop;
  case SEXP_REF:
    sexp_mark(sexp_ref_name(x));
    x = sexp_ref_cell(x);
    goto loop;
  case SEXP_SEQ:
    x = sexp_seq_ls(x);
    goto loop;
  case SEXP_LIT:
    x = sexp_lit_value(x);
    goto loop;
  }
}

sexp sexp_sweep (sexp ctx) {
  sexp_uint_t freed=0, size;
  sexp p=(sexp)sexp_heap, f1=sexp_free_list, f2;
  while ((char*)p<sexp_heap_end) {
    for (f2=sexp_cdr(f1); sexp_pairp(f2) && (f2 < p); f1=f2, f2=sexp_cdr(f2))
      ;
    size = sexp_allocated_bytes(p);
    if (! sexp_gc_mark(p)) {
      freed += size;
      sexp_car(p) = (sexp)size;
      sexp_cdr(p) = f2;
      f1 = f2;
    } else {
      sexp_gc_mark(p) = 0;
    }
    p += size;
  }
  return sexp_make_integer(freed);
}

sexp sexp_gc (sexp ctx) {
  int i;
  struct sexp_gc_var_t *saves;
  sexp *stack = sexp_context_stack(ctx);
  fprintf(stderr, "************* garbage collecting *************\n");
  for (i=0; i<SEXP_SYMBOL_TABLE_SIZE; i++)
    sexp_mark(sexp_symbol_table[i]);
  for (i=0; i<sexp_context_top(ctx); i++)
    sexp_mark(stack[i]);
  for ( ; ctx; ctx=sexp_context_parent(ctx)) {
    sexp_gc_mark(ctx) = 1;
    if (sexp_context_bc(ctx)) sexp_mark(sexp_context_bc(ctx));
    sexp_mark(sexp_context_env(ctx));
    for (saves=sexp_context_saves(ctx); saves; saves=saves->next)
      if (saves->var) sexp_mark(*(saves->var));
  }
  return sexp_sweep(ctx);
}

void *sexp_alloc (sexp ctx, size_t size) {
  sexp ls1, ls2, ls3;
  size = sexp_align(size, 3);
 try_alloc:
  ls1=sexp_free_list;
  for (ls2=sexp_cdr(ls1); sexp_pairp(ls2); ls1=ls2, ls2=sexp_cdr(ls2))
    if ((sexp_uint_t)sexp_car(ls2) >= size) {
      if ((sexp_uint_t)sexp_car(ls2) >= size + SEXP_MINIMUM_OBJECT_SIZE) {
        ls3 = (sexp) (((char*)ls2)+size);
        sexp_pointer_tag(ls3) = SEXP_PAIR;
        sexp_car(ls3) = (sexp) (((sexp_uint_t)sexp_car(ls2)) - size);
        sexp_cdr(ls3) = sexp_cdr(ls2);
        sexp_cdr(ls1) = ls3;
      } else {
        sexp_cdr(ls1) = sexp_cdr(ls2);
      }
      bzero((void*)ls2, size);
      return ls2;
    }
  if (sexp_unbox_integer(sexp_gc(ctx)) >= size) {
    goto try_alloc;
  } else {
    fprintf(stderr, "chibi: out of memory trying to allocate %ld bytes, aborting\n", size);
    exit(70);
  }
}

void sexp_gc_init () {
  sexp next;
  sexp_heap = malloc(SEXP_INITIAL_HEAP_SIZE);
  sexp_heap_end = sexp_heap + SEXP_INITIAL_HEAP_SIZE;
  sexp_free_list = (sexp)sexp_heap;
  next = (sexp) (sexp_heap + sexp_sizeof(pair));
  sexp_pointer_tag(sexp_free_list) = SEXP_PAIR;
  sexp_car(sexp_free_list) = 0; /* actually sexp_sizeof(pair) */
  sexp_cdr(sexp_free_list) = next;
  sexp_pointer_tag(next) = SEXP_PAIR;
  sexp_car(next) = (sexp) (SEXP_INITIAL_HEAP_SIZE-sexp_sizeof(pair));
  sexp_cdr(next) = SEXP_NULL;
}

