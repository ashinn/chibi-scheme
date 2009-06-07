/*  gc.c -- simple garbage collector                     */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "sexp.h"

#define SEXP_INITIAL_HEAP_SIZE 50000
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
  case SEXP_STACK:
    return sexp_sizeof(stack)+(sexp_stack_length(x)*sizeof(sexp));
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
  default: return sexp_align(1, 4);
  }
}

void sexp_mark (sexp x) {
  sexp *data;
  sexp_uint_t i;
  struct sexp_gc_var_t *saves;
 loop:
  if (((char*)x < sexp_heap) || ((char*)x >= sexp_heap_end)) {
    if (x && sexp_pointerp(x) && (sexp_pointer_tag(x) != SEXP_OPCODE))
      fprintf(stderr, "--------------- outside heap: %p (%x) ------------------\n", x, sexp_pointer_tag(x));
    return;
  }
  if ((! x) || (! sexp_pointerp(x)) || sexp_gc_mark(x))
    return;
  sexp_gc_mark(x) = 1;
  fprintf(stderr, "----------------- marking %p (%x) --------------------\n",
          x, sexp_pointer_tag(x));
  switch (sexp_pointer_tag(x)) {
  case SEXP_PAIR:
    sexp_mark(sexp_car(x));
    x = sexp_cdr(x);
    goto loop;
  case SEXP_STACK:
    data = sexp_stack_data(x);
    if (! sexp_stack_top(x)) break;
    for (i=sexp_stack_top(x)-1; i>0; i--)
      sexp_mark(data[i]);
    x = data[0];
    goto loop;
  case SEXP_VECTOR:
    data = sexp_vector_data(x);
    if (! sexp_vector_length(x)) break;
    for (i=sexp_vector_length(x)-1; i>0; i--)
      sexp_mark(data[i]);
    x = data[0];
    goto loop;
  case SEXP_SYMBOL:
    x = sexp_symbol_string(x);
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
  case SEXP_CONTEXT:
    sexp_mark(sexp_context_env(x));
    sexp_mark(sexp_context_bc(x));
    sexp_mark(sexp_context_fv(x));
    sexp_mark(sexp_context_lambda(x));
    sexp_mark(sexp_context_parent(x));
    for (saves=sexp_context_saves(x); saves; saves=saves->next)
      if (saves->var) sexp_mark(*(saves->var));
    x = sexp_context_stack(x);
    goto loop;
  }
}

void simple_write (sexp obj, int depth, FILE *out) {
  unsigned long len, c, res;
  long i=0;
  double f;
  char *str=NULL;

  if (! obj) {
    fputs("#<null>", out);
  } if (! sexp_pointerp(obj)) {
    if (sexp_integerp(obj)) {
      fprintf(out, "%ld", sexp_unbox_integer(obj));
    } else if (sexp_charp(obj)) {
      if (obj == sexp_make_character(' '))
        fputs("#\\space", out);
      else if (obj == sexp_make_character('\n'))
        fputs("#\\newline", out);
      else if (obj == sexp_make_character('\r'))
        fputs("#\\return", out);
      else if (obj == sexp_make_character('\t'))
        fputs("#\\tab", out);
      else if ((33 <= sexp_unbox_character(obj))
               && (sexp_unbox_character(obj) < 127))
        fprintf(out, "#\\%c", sexp_unbox_character(obj));
      else
        fprintf(out, "#\\x%02d", sexp_unbox_character(obj));
    } else if (sexp_symbolp(obj)) {

#if USE_HUFF_SYMS
      if (((sexp_uint_t)obj&7)==7) {
        c = ((sexp_uint_t)obj)>>3;
        while (c) {
#include "sexp-unhuff.c"
          putc(res, out);
        }
      }
#endif

    } else {
      switch ((sexp_uint_t) obj) {
      case (sexp_uint_t) SEXP_NULL:
        fputs("()", out); break;
      case (sexp_uint_t) SEXP_TRUE:
        fputs("#t", out); break;
      case (sexp_uint_t) SEXP_FALSE:
        fputs("#f", out); break;
      case (sexp_uint_t) SEXP_EOF:
        fputs("#<eof>", out); break;
      case (sexp_uint_t) SEXP_UNDEF:
      case (sexp_uint_t) SEXP_VOID:
        fputs("#<undef>", out); break;
      default:
        fprintf(out, "#<invalid: %p>", obj);
      }
    }
  } else if (depth <= 0) {
    fprintf(out, "#<...>");
  } else {
    switch (sexp_pointer_tag(obj)) {
    case SEXP_PAIR:
      putc('(', out);
      simple_write(sexp_car(obj), depth-1, out);
      if (sexp_pairp(sexp_cdr(obj))) {
        fputs(" ...", out);
      } else if (! sexp_nullp(sexp_cdr(obj))) {
        fputs(" . ", out);
        simple_write(sexp_cdr(obj), depth-1, out);
      }
      putc(')', out);
      break;
    case SEXP_VECTOR:
      len = sexp_vector_length(obj);
      if (len == 0) {
        fputs("#()", out);
      } else {
        fprintf(out, "#(... %ld ...)", len);
      }
      break;
    case SEXP_FLONUM:
      f = sexp_flonum_value(obj);
      fprintf(out, "%.15g%s", f, (f == trunc(f)) ? ".0" : "");
      break;
    case SEXP_PROCEDURE:
      fputs("#<procedure: ", out);
      simple_write(sexp_bytecode_name(sexp_procedure_code(obj)), depth-1, out);
      putc('>', out);
      break;
    case SEXP_IPORT:
      fputs("#<input-port>", out); break;
    case SEXP_OPORT:
      fputs("#<output-port>", out); break;
    case SEXP_CORE:
      fputs("#<core-form>", out); break;
    case SEXP_OPCODE:
      fputs("#<opcode>", out); break;
    case SEXP_BYTECODE:
      fputs("#<bytecode>", out); break;
    case SEXP_ENV:
      fprintf(out, "#<env %p>", obj); break;
    case SEXP_EXCEPTION:
      fputs("#<exception>", out); break;
    case SEXP_MACRO:
      fputs("#<macro>", out); break;
    case SEXP_LAMBDA:
      fputs("#<lambda ", out);
      simple_write(sexp_lambda_params(obj), depth-1, out);
      putc(' ', out);
      simple_write(sexp_lambda_body(obj), depth-1, out);
      putc('>', out);
      break;
    case SEXP_SEQ:
      fputs("#<seq ", out);
      simple_write(sexp_seq_ls(obj), depth-1, out);
      putc('>', out);
      break;
    case SEXP_CND:
      fputs("#<if ", out);
      simple_write(sexp_cnd_test(obj), depth-1, out);
      putc(' ', out);
      simple_write(sexp_cnd_pass(obj), depth-1, out);
      putc(' ', out);
      simple_write(sexp_cnd_fail(obj), depth-1, out);
      putc('>', out);
      break;
    case SEXP_REF:
      fputs("#<ref: ", out);
      simple_write(sexp_ref_name(obj), depth-1, out);
      fprintf(out, " %p>", sexp_ref_loc(obj));
      break;
    case SEXP_SET:
      fputs("#<set! ", out);
      simple_write(sexp_set_var(obj), depth-1, out);
      putc(' ', out);
      simple_write(sexp_set_value(obj), depth-1, out);
      putc('>', out);
      break;
    case SEXP_LIT:
      fputs("#<lit ", out);
      simple_write(sexp_lit_value(obj), depth-1, out);
      putc('>', out);
      break;
    case SEXP_CONTEXT:
      fputs("#<context>", out);
      break;
    case SEXP_SYNCLO:
      fputs("#<sc ", out);
      simple_write(sexp_synclo_expr(obj), depth-1, out);
      putc('>', out);
      break;
    case SEXP_STRING:
      putc('"', out);
      i = sexp_string_length(obj);
      str = sexp_string_data(obj);
      for ( ; i>0; str++, i--) {
        switch (str[0]) {
        case '\\': fputs("\\\\", out); break;
        case '"': fputs("\\\"", out); break;
        case '\n': fputs("\\n", out); break;
        case '\r': fputs("\\r", out); break;
        case '\t': fputs("\\t", out); break;
        default: putc(str[0], out);
        }
      }
      putc('"', out);
      break;
    case SEXP_SYMBOL:
      i = sexp_string_length(sexp_symbol_string(obj));
      str = sexp_string_data(sexp_symbol_string(obj));
      for ( ; i>0; str++, i--) {
        if ((str[0] == '\\') || is_separator(str[0]))
          putc('\\', out);
        putc(str[0], out);
      }
      break;
    default:
      fprintf(out, "#<invalid type: %d>", sexp_pointer_tag(obj));
      break;
    }
  }
}

void sexp_show_free_list (sexp ctx) {
  sexp p=sexp_free_list;
  fputs("free-list:", stderr);
  while (p && sexp_pairp(p) && ((char*) p < sexp_heap_end)) {
    fprintf(stderr, " %p-%p", p, p+(sexp_uint_t)sexp_car(p));
    p = sexp_cdr(p);
  }
  putc('\n', stderr);
}

sexp sexp_sweep (sexp ctx) {
  sexp_uint_t freed=0, size;
  sexp p=(sexp)(sexp_heap+sexp_align(SEXP_MINIMUM_OBJECT_SIZE, 4));
  sexp f1=sexp_free_list, f2;
  /* scan over the whole heap */
  while ((char*)p<sexp_heap_end) {
    /* find the preceding and succeeding free list pointers */
    for (f2=sexp_cdr(f1); f2 && sexp_pairp(f2) && (f2 < p); f1=f2, f2=sexp_cdr(f2))
      ;
    fprintf(stderr, "p: %p f1: %p f2: %p\n", p, f1, f2);
    size = sexp_align(sexp_allocated_bytes(p), 4);
    if (! sexp_gc_mark(p)) {
      fprintf(stderr, "freeing %lu bytes @ %p (%x) ", size, p, sexp_pointer_tag(p));
      simple_write(p, 1, stderr);
      fprintf(stderr, " -\n");
      freed += size;
      sexp_pointer_tag(p) = SEXP_PAIR;
      sexp_car(p) = (sexp)size;
      sexp_cdr(p) = f2;
      sexp_cdr(f1) = p;
      /* f1 = f2; */
    } else {
      fprintf(stderr, "saving  %lu bytes @ %p (%x) ", size, p, sexp_pointer_tag(p));
      simple_write(p, 1, stderr);
      fprintf(stderr, " +\n");
      sexp_gc_mark(p) = 0;
    }
    p = (sexp) (((char*)p)+size);
  }
  fprintf(stderr, "**************** freed %ld bytes ****************\n", freed);
  return sexp_make_integer(freed);
}

extern sexp continuation_resumer, final_resumer;

sexp sexp_gc (sexp ctx) {
  int i;
  fprintf(stderr, "************* garbage collecting *************\n");
  sexp_show_free_list(ctx);
  sexp_mark(continuation_resumer);
  sexp_mark(final_resumer);
  for (i=0; i<SEXP_SYMBOL_TABLE_SIZE; i++)
    sexp_mark(sexp_symbol_table[i]);
  sexp_mark(ctx);
  return sexp_sweep(ctx);
}

void *sexp_alloc (sexp ctx, size_t size) {
  int tries = 0;
  sexp ls1, ls2, ls3;
  size = sexp_align(size, 4);
 try_alloc:
  ls1 = sexp_free_list;
  ls2 = sexp_cdr(ls1);
  for (ls2=sexp_cdr(ls1); sexp_pairp(ls2); ) {
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
    ls1=ls2;
    ls2=sexp_cdr(ls2);
  }
  if ((! tries) && (sexp_unbox_integer(sexp_gc(ctx)) >= size)) {
    tries++;
    goto try_alloc;
  } else {
    fprintf(stderr,
            "chibi: out of memory trying to allocate %ld bytes, aborting\n",
            size);
    exit(70);
  }
}

void sexp_gc_init () {
  sexp next;
  sexp_heap = malloc(SEXP_INITIAL_HEAP_SIZE);
  sexp_heap_end = sexp_heap + SEXP_INITIAL_HEAP_SIZE;
  sexp_free_list = (sexp)sexp_heap;
  next = (sexp) (sexp_heap + sexp_align(sexp_sizeof(pair), 4));
  sexp_pointer_tag(sexp_free_list) = SEXP_PAIR;
  sexp_car(sexp_free_list) = 0; /* actually sexp_sizeof(pair) */
  sexp_cdr(sexp_free_list) = next;
  sexp_pointer_tag(next) = SEXP_PAIR;
  sexp_car(next) = (sexp) (SEXP_INITIAL_HEAP_SIZE
                           - sexp_align(sexp_sizeof(pair), 4));
  sexp_cdr(next) = SEXP_NULL;
  fprintf(stderr, "heap: %p - %p, next: %p\n", sexp_heap, sexp_heap_end, next);
}

