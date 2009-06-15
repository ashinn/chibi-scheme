/*  gc.c -- simple mark&sweep garbage collector          */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "sexp.h"

/* #define SEXP_INITIAL_HEAP_SIZE (3*1024*1024) */
#define SEXP_INITIAL_HEAP_SIZE 37000
#define SEXP_MAXIMUM_HEAP_SIZE 0
#define SEXP_MINIMUM_OBJECT_SIZE (sexp_sizeof(flonum))

static char* sexp_heap;
static char* sexp_heap_end;
static sexp sexp_free_list;

static sexp* stack_base;

sexp_uint_t sexp_allocated_bytes (sexp x) {
  sexp_uint_t res, *len_ptr;
  sexp t;
  if ((! sexp_pointerp(x)) || (sexp_pointer_tag(x) > SEXP_CONTEXT))
    return sexp_align(1, 4);
  t = &(sexp_types[sexp_pointer_tag(x)]);
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
  t = &(sexp_types[sexp_pointer_tag(x)]);
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

void simple_write (sexp obj, int depth, FILE *out) {
  unsigned long len, c, res;
  long i=0;
  double f;
  char *str=NULL;

  if (! obj) {
    fputs("#<null>", out);
  } else if (! sexp_pointerp(obj)) {
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
  sexp p=sexp_free_list, prev=NULL;
  fputs("free-list:", stderr);
  while (p && sexp_pairp(p) && ((char*) p < sexp_heap_end)) {
    if (p < prev) {
      fprintf(stderr, " \x1B[31m%p-%p\x1B[0m", p, ((char*)p)+(sexp_uint_t)sexp_car(p));
    } else {
      fprintf(stderr, " %p-%p", p, ((char*)p)+(sexp_uint_t)sexp_car(p));
    }
    prev = (sexp) (((char*)p)+(sexp_uint_t)sexp_car(p));
    p = sexp_cdr(p);
  }
  putc('\n', stderr);
}

void validate_free_list () {
  sexp p=sexp_free_list, prev=NULL;
  while (p && sexp_pairp(p) && ((char*) p < sexp_heap_end)) {
    if (((char*)p < sexp_heap) || ((char*)p >= sexp_heap_end))
      fprintf(stderr, " \x1B[31mfree-list outside heap: %p prev: %p\x1B[0m", p, prev);
    if (p < prev)
      fprintf(stderr, " \x1B[31mfree-list out of order at: %p prev: %p cdr: %p\x1B[0m", p, prev, sexp_cdr(p));
    if ((sexp_uint_t)p != sexp_align((sexp_uint_t)p, 4))
      fprintf(stderr, " \x1B[31mfree-list misaligned: %p prev: %p\x1B[0m", p, prev);
    prev = (sexp) (((char*)p)+(sexp_uint_t)sexp_car(p));
    p = sexp_cdr(p);
  }
}

void validate_heap (sexp ctx) {
  sexp_uint_t size;
  sexp p=(sexp)(sexp_heap+sexp_align(SEXP_MINIMUM_OBJECT_SIZE, 4));
  sexp q=sexp_free_list, r;
  /* scan over the whole heap */
  while (((char*)p) < sexp_heap_end) {
    /* find the preceding and succeeding free list pointers */
    for (r=sexp_cdr(q); r && sexp_pairp(r) && (r<p); q=r, r=sexp_cdr(r))
      ;
    if (r == p) {
      p = (sexp) (((char*)p) + (sexp_uint_t)sexp_car(p));
      continue;
    }
    /* if (((sexp_uint_t)p >= 0x29e00) && ((sexp_uint_t)p <= 0x2a000)) */
    /*   fprintf(stderr, "validate heap: %p (%p .. %p)\n", p, q, r); */
    size = sexp_align(sexp_allocated_bytes(p), 4);
    if (sexp_pointer_tag(p) == 0) {
      fprintf(stderr, "bare object found at %p\n", p);
    } else if (sexp_pointer_tag(p) > SEXP_CONTEXT) {
      fprintf(stderr, "bad type at %p: %d\n", p, sexp_pointer_tag(p));
    }
    p = (sexp) (((char*)p)+size);
  }
}

void validate_gc_vars (sexp ctx) {
  struct sexp_gc_var_t *saves, *prev=NULL;
  if (! ctx)
    return;
  for (saves=sexp_context_saves(ctx); saves; saves=saves->next) {
    if (saves->var && *(saves->var) && sexp_pointerp(*(saves->var))) {
      if (((char*)*(saves->var) < sexp_heap)
          || ((char*)*(saves->var) >= sexp_heap_end))
        fprintf(stderr, "bad variable in gc var: %s => %p\n", saves->name, *(saves->var));
      if ((sexp_uint_t)*(saves->var)
          != sexp_align((sexp_uint_t)*(saves->var), 4))
        fprintf(stderr, "misaligned gc var: %p\n", *(saves->var));
    }
    if (prev && (prev > saves)) {
      fprintf(stderr, "gc vars out of order: %p > %p\n", prev, saves);
      return;
    } else if (prev == saves) {
      fprintf(stderr, "loop in gc vars at %p\n", saves);
      return;
    }
    prev = saves;
  }
}

int validate_freed_pointer (sexp ctx, sexp x) {
  int freep = 1;
  sexp *p;
  struct sexp_gc_var_t *saves, *prev=NULL;
  char *v1, *v2;

  for (p=&x; p<stack_base; p++) {
    if (*p == x) {
      v1 = v2 = NULL;
      for (saves=sexp_context_saves(ctx); saves; saves=saves->next) {
        if (saves->var && prev && prev->var
            && (((saves->var <= p) && (prev->var >= p))
                || ((saves->var >= p) && (prev->var <= p)))) {
          v1 = saves->name;
          v2 = prev->name;
          break;
        }
        prev = saves;
      }
      if (v1 && v2)
        fprintf(stderr, "reference to freed var %p at %p between %s and %s: ",
                x, p, v1, v2);
      else if (sexp_context_saves(ctx) && (p <= sexp_context_saves(ctx)->var))
        fprintf(stderr, "reference to freed var %p at %p after %s: ",
                x, p, sexp_context_saves(ctx)->name);
      else if (prev && (p >= prev->var))
        fprintf(stderr, "reference to freed var %p at %p before %s: ",
                x, p, prev->name);
      else
        fprintf(stderr, "reference to freed var %p at %p: ", x, p);
      simple_write(x, 1, stderr);
      putc('\n', stderr);
      freep = 0;
    }
  }

  return freep;
}

sexp sexp_sweep (sexp ctx) {
  sexp_uint_t freed, max_freed=0, sum_freed=0, size;
  sexp p=(sexp)(sexp_heap+sexp_align(SEXP_MINIMUM_OBJECT_SIZE, 4));
  sexp q=sexp_free_list, r;
  /* scan over the whole heap */
  while (((char*)p) < sexp_heap_end) {
    /* find the preceding and succeeding free list pointers */
    for (r=sexp_cdr(q); r && sexp_pairp(r) && (r<p); q=r, r=sexp_cdr(r))
      ;
    /* fprintf(stderr, "p: %p q: %p r: %p\n", p, q, r); */
    if (r == p) {
      p = (sexp) (((char*)p) + (sexp_uint_t)sexp_car(p));
      continue;
    } else if (p <= q) {
      fprintf(stderr, "sweep: p: %p <= q: %p\n", p, q);
    }
    size = sexp_align(sexp_allocated_bytes(p), 4);
    if ((! sexp_gc_mark(p))/*  && validate_freed_pointer(ctx, p) */) {
/*       fprintf(stderr, "\x1B[31mfreeing %lu bytes @ %p (%x) ", size, p, sexp_pointer_tag(p)); */
/*       simple_write(p, 1, stderr); */
/*       fprintf(stderr, "\x1B[0m\n"); */
      sum_freed += size;
      if (((((char*)q)+(sexp_uint_t)sexp_car(q)) == (char*)p)
          && (q != sexp_free_list)) {
        /* merge q with p */
/*         fprintf(stderr, "\x1B[34mleft merging  %lu bytes @ %p ", size, p); */
/*         simple_write(p, 1, stderr); */
/*         fprintf(stderr, " with %lu bytes @ %p (%p)\x1B[0m\n", */
/*                 (sexp_uint_t)sexp_car(q), q, sexp_cdr(q)); */
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
          /* merge p with r */
/*           fprintf(stderr, "\x1B[34mright merging %lu bytes @ %p ", size, p); */
/*           simple_write(p, 1, stderr); */
/*           fprintf(stderr, " with %lu bytes @ %p (%p)\x1B[0m\n", */
/*                   (sexp_uint_t)sexp_car(r), r, sexp_cdr(r)); */
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
/*       fprintf(stderr, "\x1B[32msaving  %lu bytes @ %p (%x) ", size, p, sexp_pointer_tag(p)); */
/*       simple_write(p, 1, stderr); */
/*       fprintf(stderr, "\x1B[0m\n"); */
      sexp_gc_mark(p) = 0;
      p = (sexp) (((char*)p)+size);
    }
  }
  fprintf(stderr, "**************** freed %ld bytes, max %ld ****************\n", sum_freed, max_freed);
  return sexp_make_integer(max_freed);
}

extern sexp continuation_resumer, final_resumer;

sexp sexp_gc (sexp ctx) {
  sexp res;
  int i;
  fprintf(stderr, "************* garbage collecting *************\n");
  /* sexp_show_free_list(ctx); */
  sexp_mark(continuation_resumer);
  sexp_mark(final_resumer);
  for (i=0; i<SEXP_SYMBOL_TABLE_SIZE; i++)
    sexp_mark(sexp_symbol_table[i]);
  sexp_mark(ctx);
  res = sexp_sweep(ctx);
  fprintf(stderr, "************* post gc validation *************\n");
  validate_heap(ctx);
  validate_free_list();
  validate_gc_vars(ctx);
  fprintf(stderr, "************* done post gc validation *************\n");
  return res;
}

#define _adjust(x) if ((x) && (sexp_pointerp(x))) (x) = (sexp) (((char*)(x))+offset)

void sexp_adjust_pointers (sexp x, sexp_sint_t offset) {
  sexp_uint_t *len_ptr;
  sexp_sint_t i, len;
  sexp t, *p;
  struct sexp_gc_var_t *saves;

  if ((! x) || (! sexp_pointerp(x)))
    return;
  if (sexp_contextp(x))
    for (saves=sexp_context_saves(x); saves; saves=saves->next)
      if (saves->var) _adjust(*(saves->var));
  t = &(sexp_types[sexp_pointer_tag(x)]);
  p = (sexp*) (((char*)x) + sexp_type_field_base(t));
  len_ptr = (sexp_uint_t*) (((char*)x) + sexp_type_field_len_off(t));
  len = sexp_type_field_len_base(t) + len_ptr[0]*sexp_type_field_len_scale(t);
  for (i=0; i<len; i++)
    _adjust(p[i]);
}

void sexp_adjust_heap (char *start, char *end,
                       sexp_sint_t offset, size_t new_size) {
  sexp p=(sexp)(start+sexp_align(SEXP_MINIMUM_OBJECT_SIZE, 4)), q, r, *t;
  /* adjust the free list in the new heap */
  _adjust(sexp_free_list);
  fprintf(stderr, "free-list: %p, start: %p, end: %p, offset: %ld\n", sexp_free_list, start, end, offset);
  q = sexp_free_list;
  _adjust(sexp_cdr(q));
  r = sexp_cdr(q);
  for ( ; r && sexp_pairp(r) && (((char*)r) < end); q=r, r=sexp_cdr(r))
    _adjust(sexp_cdr(r));
  r = (sexp) end;
  sexp_cdr(q) = r;
  sexp_pointer_tag(r) = SEXP_PAIR;
  sexp_car(r) = (sexp) (new_size - (end-start));
  sexp_cdr(r) = NULL;
  fprintf(stderr, "************* done adjusting free list *************\n");
  validate_free_list();
  /* adjust the new heap */
  q = sexp_free_list;
  while (((char*)p) < end) {
    /* find the preceding and succeeding free list pointers */
    for (r=sexp_cdr(q); r && sexp_pairp(r) && (r<p); q=r, r=sexp_cdr(r))
      ;
    if (r == p) {
      p = (sexp) (((char*)p) + (sexp_uint_t)sexp_car(p));
      continue;
    }
    sexp_adjust_pointers(p, offset);
    p = (sexp) (((char*)p) + sexp_align(sexp_allocated_bytes(p), 4));
  }
  fprintf(stderr, "************* done adjusting heap *************\n");
  for (t=(sexp*)start; t<(sexp*)end; t++)
    if (*t && sexp_pointerp(*t)
        && (*t > (sexp)(start-offset)) && (*t < (sexp)(end-offset)))
      fprintf(stderr, "bad address at %p: %p\n", t, *t);
}

void* sexp_realloc_heap (char *heap, size_t cur_size, size_t new_size) {
  char *res = malloc(new_size);
  memcpy(res, heap, cur_size);
  return res;
}

/* #define sexp_realloc_heap(h, cs, ns) realloc(h, ns) */

int sexp_grow_heap (sexp ctx, size_t size) {
  char *tmp1, *tmp2;
  sexp q;
  size_t cur_size = sexp_heap_end - sexp_heap, new_size;
  new_size = sexp_align(((cur_size > size) ? cur_size : size) * 2, 4);
  fprintf(stderr, "************* growing heap *************\n");
  validate_heap(ctx);
  if (SEXP_MAXIMUM_HEAP_SIZE && (new_size > SEXP_MAXIMUM_HEAP_SIZE)) {
    fprintf(stderr, "************* heap too large *************\n");
    return 0;
  }
  if (! (tmp1 = sexp_realloc_heap(sexp_heap, cur_size, new_size))) {
    fprintf(stderr, "************* couldn't realloc *************\n");
    return 0;
  }
  if (tmp1 != sexp_heap) {
    fprintf(stderr, "************* adjusting heap: %p => %p (%d) *************\n", sexp_heap, tmp1, tmp1-sexp_heap);
    sexp_adjust_heap(tmp1, tmp1+cur_size, tmp1-sexp_heap, new_size);
    fprintf(stderr, "************* done adjusting *************\n");
    tmp2 = sexp_heap;
    sexp_heap = tmp1;
    free(tmp2);
  } else {
    for (q = sexp_free_list;
         sexp_cdr(q) && sexp_pairp(sexp_cdr(q));
         q = sexp_cdr(q))
      ;
    sexp_cdr(q) = (sexp) sexp_heap_end;
    q = sexp_cdr(q);
    sexp_pointer_tag(q) = SEXP_PAIR;
    sexp_car(q) = (sexp) (new_size - cur_size);
    sexp_cdr(q) = SEXP_NULL;
  }
  sexp_heap_end = sexp_heap + new_size;
  sexp_show_free_list(ctx);
  return 1;
}

void* sexp_try_alloc (sexp ctx, size_t size) {
  sexp ls1, ls2, ls3;
  ls1 = sexp_free_list;
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
  return NULL;
}

void* sexp_alloc (sexp ctx, size_t size) {
  void *res;
/*   validate_heap(ctx); */
/*   validate_free_list(ctx); */
/*   validate_gc_vars(ctx); */
  size = sexp_align(size, 4);
  res = sexp_try_alloc(ctx, size);
  if (! res) {
    if (sexp_unbox_integer(sexp_gc(ctx)) >= size)
      res = sexp_try_alloc(ctx, size);
    if ((! res) && sexp_grow_heap(ctx, size))
      res = sexp_try_alloc(ctx, size);
    if (! res) {
      fprintf(stderr,
              "chibi: out of memory trying to allocate %ld bytes, aborting\n",
              size);
      exit(70);
    }
  }
  /* fprintf(stderr, "sexp_alloc %lu => %p\n", size, res); */
  return res;
}

void sexp_gc_init () {
  sexp_uint_t size = sexp_align(SEXP_INITIAL_HEAP_SIZE, 4);
  sexp next;
  sexp_heap = malloc(size);
  sexp_heap_end = sexp_heap + size;
  sexp_free_list = (sexp)sexp_heap;
  next = (sexp) (sexp_heap + sexp_align(sexp_sizeof(pair), 4));
  sexp_pointer_tag(sexp_free_list) = SEXP_PAIR;
  sexp_car(sexp_free_list) = 0; /* actually sexp_sizeof(pair) */
  sexp_cdr(sexp_free_list) = next;
  sexp_pointer_tag(next) = SEXP_PAIR;
  sexp_car(next) = (sexp) (size - sexp_align(sexp_sizeof(pair), 4));
  sexp_cdr(next) = SEXP_NULL;
  stack_base = &next + 32;
  fprintf(stderr, "heap: %p - %p, next: %p, stack_base: %p\n",
          sexp_heap, sexp_heap_end, next, stack_base);
}

