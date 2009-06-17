/*  sexp.c -- standalone sexp library implementation     */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "sexp.h"

/* optional huffman-compressed immediate symbols */
#ifdef USE_HUFF_SYMS
struct huff_entry {
  unsigned char len;
  unsigned short bits;
};
#include "opt/sexp-hufftabs.c"
static struct huff_entry huff_table[] = {
#include "opt/sexp-huff.c"
};
#endif

static int sexp_initialized_p = 0;

static sexp the_dot_symbol;
static sexp the_quote_symbol;
static sexp the_quasiquote_symbol;
static sexp the_unquote_symbol;
static sexp the_unquote_splicing_symbol;
static sexp the_read_error_symbol;
static sexp the_empty_vector;

static char sexp_separators[] = {
  /* 1  2  3  4  5  6  7  8  9  a  b  c  d  e  f         */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, /* x0_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x1_ */
  1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, /* x2_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, /* x3_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x4_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, /* x5_ */
};

static int digit_value (c) {
  return (((c)<='9') ? ((c) - '0') : ((toupper(c) - 'A') + 10));
}

static int is_separator(int c) {
  return 0<c && c<0x60 && sexp_separators[c];
}

sexp sexp_symbol_table[SEXP_SYMBOL_TABLE_SIZE];

sexp sexp_alloc_tagged(sexp ctx, size_t size, sexp_uint_t tag) {
  sexp res = (sexp) sexp_alloc(ctx, size);
  if (res) sexp_pointer_tag(res) = tag;
  return res;
}

#define _DEF_TYPE(t,fb,flb,flo,fls,sb,so,sc,n) \
  {.tag=SEXP_TYPE, .value={.type={t,fb,flb,flo,fls,sb,so,sc,n}}}

static struct sexp_struct sexp_types[] = {
  _DEF_TYPE(SEXP_OBJECT, 0, 0, 0, 0, 0, 0, 0, "object"),
  _DEF_TYPE(SEXP_TYPE, 0, 0, 0, 0, sexp_sizeof(type), 0, 0, "type"),
  _DEF_TYPE(SEXP_FIXNUM, 0, 0, 0, 0, 0, 0, 0, "fixnum"),
  _DEF_TYPE(SEXP_CHAR, 0, 0, 0, 0, 0, 0, 0, "char"),
  _DEF_TYPE(SEXP_BOOLEAN, 0, 0, 0, 0, 0, 0, 0, "boolean"),
  _DEF_TYPE(SEXP_PAIR, sexp_offsetof(pair, car), 2, 0, 0, sexp_sizeof(pair), 0, 0, "pair"),
  _DEF_TYPE(SEXP_SYMBOL, sexp_offsetof(symbol, string), 1, 0, 0, sexp_sizeof(symbol), 0, 0, "symbol"),
  _DEF_TYPE(SEXP_STRING, 0, 0, 0, 0, sexp_sizeof(string)+1, sexp_offsetof(string, length), 1, "string"),
  _DEF_TYPE(SEXP_VECTOR, sexp_offsetof(vector, data), 0, sexp_offsetof(vector, length), 1, sexp_sizeof(vector), sexp_offsetof(vector, length), 4, "vector"),
  _DEF_TYPE(SEXP_FLONUM, 0, 0, 0, 0, sexp_sizeof(flonum), 0, 0, "flonum"),
  _DEF_TYPE(SEXP_BIGNUM, 0, 0, 0, 0, sexp_sizeof(bignum), sexp_offsetof(bignum, length), 4, "bignum"),
  _DEF_TYPE(SEXP_IPORT, sexp_offsetof(port, cookie), 1, 0, 0, sexp_sizeof(port), 0, 0, "input-port"),
  _DEF_TYPE(SEXP_OPORT, sexp_offsetof(port, cookie), 1, 0, 0, sexp_sizeof(port), 0, 0, "output-port"),
  _DEF_TYPE(SEXP_EXCEPTION, sexp_offsetof(exception, kind), 6, 0, 0, sexp_sizeof(exception), 0, 0, "exception"),
  _DEF_TYPE(SEXP_PROCEDURE, sexp_offsetof(procedure, bc), 2, 0, 0, sexp_sizeof(procedure), 0, 0, "procedure"),
  _DEF_TYPE(SEXP_MACRO, sexp_offsetof(macro, proc), 2, 0, 0, sexp_sizeof(macro), 0, 0, "macro"),
  _DEF_TYPE(SEXP_SYNCLO, sexp_offsetof(synclo, env), 3, 0, 0, sexp_sizeof(synclo), 0, 0, "syntactic-closure"),
  _DEF_TYPE(SEXP_ENV, sexp_offsetof(env, parent), 3, 0, 0, sexp_sizeof(env), 0, 0, "environment"),
  _DEF_TYPE(SEXP_BYTECODE, sexp_offsetof(bytecode, name), 2, 0, 0, sexp_sizeof(bytecode), offsetof(struct sexp_struct, value.bytecode.length), 1, "bytecode"),
  _DEF_TYPE(SEXP_CORE, 0, 0, 0, 0, sexp_sizeof(core), 0, 0, "core-form"),
  _DEF_TYPE(SEXP_OPCODE, sexp_offsetof(opcode, dflt), 2, 0, 0, sexp_sizeof(opcode), 0, 0, "opcode"),
  _DEF_TYPE(SEXP_LAMBDA, sexp_offsetof(lambda, name), 8, 0, 0, sexp_sizeof(lambda), 0, 0, "lambda"),
  _DEF_TYPE(SEXP_CND, sexp_offsetof(cnd, test), 3, 0, 0, sexp_sizeof(cnd), 0, 0, "conditoinal"),
  _DEF_TYPE(SEXP_REF, sexp_offsetof(ref, name), 2, 0, 0, sexp_sizeof(ref), 0, 0, "reference"),
  _DEF_TYPE(SEXP_SET, sexp_offsetof(set, var), 2, 0, 0, sexp_sizeof(set), 0, 0, "set!"),
  _DEF_TYPE(SEXP_SEQ, sexp_offsetof(seq, ls), 1, 0, 0, sexp_sizeof(seq), 0, 0, "sequence"),
  _DEF_TYPE(SEXP_LIT, sexp_offsetof(lit, value), 1, 0, 0, sexp_sizeof(lit), 0, 0, "literal"),
  _DEF_TYPE(SEXP_STACK, sexp_offsetof(stack, data), 1, sexp_offsetof(stack, top), 1, sexp_sizeof(stack), offsetof(struct sexp_struct, value.stack.length), 4, "stack"),
  _DEF_TYPE(SEXP_CONTEXT, sexp_offsetof(context, bc), 6, 0, 0, sexp_sizeof(context), 0, 0, "context"),
};

#undef _DEF_TYPE

#if ! USE_BOEHM
#if ! USE_MALLOC
#include "gc.c"
#endif
#endif

/***************************** exceptions *****************************/

sexp sexp_make_exception (sexp ctx, sexp kind, sexp message, sexp irritants,
                          sexp procedure, sexp file, sexp line) {
  sexp exn = sexp_alloc_type(ctx, exception, SEXP_EXCEPTION);
  sexp_exception_kind(exn) = kind;
  sexp_exception_message(exn) = message;
  sexp_exception_irritants(exn) = irritants;
  sexp_exception_procedure(exn) = procedure;
  sexp_exception_file(exn) = file;
  sexp_exception_line(exn) = line;
  return exn;
}

sexp sexp_user_exception (sexp ctx, sexp self, char *message, sexp irritants) {
  sexp res;
  sexp_gc_var(ctx, sym, s_sym);
  sexp_gc_var(ctx, str, s_str);
  sexp_gc_var(ctx, irr, s_irr);
  sexp_gc_preserve(ctx, sym, s_sym);
  sexp_gc_preserve(ctx, str, s_str);
  sexp_gc_preserve(ctx, irr, s_irr);
  res = sexp_make_exception(ctx, sym = sexp_intern(ctx, "user"),
                            str = sexp_c_string(ctx, message, -1),
                            ((sexp_pairp(irritants) || sexp_nullp(irritants))
                             ? irritants : (irr = sexp_list1(ctx, irritants))),
                            self, SEXP_FALSE, SEXP_FALSE);
  sexp_gc_release(ctx, sym, s_sym);
  return res;
}

sexp sexp_type_exception (sexp ctx, char *message, sexp obj) {
  sexp res;
  sexp_gc_var(ctx, sym, s_sym);
  sexp_gc_var(ctx, str, s_str);
  sexp_gc_var(ctx, irr, s_irr);
  sexp_gc_preserve(ctx, sym, s_sym);
  sexp_gc_preserve(ctx, str, s_str);
  sexp_gc_preserve(ctx, irr, s_irr);
  res = sexp_make_exception(ctx, sym = sexp_intern(ctx, "type"),
                            str = sexp_c_string(ctx, message, -1),
                            irr = sexp_list1(ctx, obj),
                            SEXP_FALSE, SEXP_FALSE, SEXP_FALSE);
  sexp_gc_release(ctx, sym, s_sym);
  return res;
}

sexp sexp_range_exception (sexp ctx, sexp obj, sexp start, sexp end) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_var(ctx, msg, s_msg);
  sexp_gc_preserve(ctx, res, s_res);
  sexp_gc_preserve(ctx, msg, s_msg);
  msg = sexp_c_string(ctx, "bad index range", -1);
  res = sexp_list2(ctx, start, end);
  res = sexp_cons(ctx, obj, res);
  res = sexp_make_exception(ctx, sexp_intern(ctx, "range"), msg, res,
                            SEXP_FALSE, SEXP_FALSE, SEXP_FALSE);
  sexp_gc_release(ctx, res, s_res);
  return res;
}

sexp sexp_print_exception (sexp ctx, sexp exn, sexp out) {
  sexp ls;
  sexp_write_string("ERROR", out);
  if (sexp_exceptionp(exn)) {
    if (sexp_procedurep(sexp_exception_procedure(exn))) {
      ls = sexp_bytecode_name(
             sexp_procedure_code(sexp_exception_procedure(exn)));
      if (sexp_symbolp(ls)) {
        sexp_write_string(" in ", out);
        sexp_write(ls, out);
      }
    }
    if (sexp_integerp(sexp_exception_line(exn))
        && (sexp_exception_line(exn) > sexp_make_integer(0))) {
      sexp_write_string(" on line ", out);
      sexp_write(sexp_exception_line(exn), out);
    }
    if (sexp_stringp(sexp_exception_file(exn))) {
      sexp_write_string(" of file ", out);
      sexp_write_string(sexp_string_data(sexp_exception_file(exn)), out);
    }
    sexp_write_string(": ", out);
    sexp_write_string(sexp_string_data(sexp_exception_message(exn)), out);
    if (sexp_exception_irritants(exn)
        && sexp_pairp(sexp_exception_irritants(exn))) {
      if (sexp_nullp(sexp_cdr(sexp_exception_irritants(exn)))) {
        sexp_write_string(": ", out);
        sexp_write(sexp_car(sexp_exception_irritants(exn)), out);
        sexp_write_string("\n", out);
      } else {
        sexp_write_string("\n", out);
        for (ls=sexp_exception_irritants(exn);
             sexp_pairp(ls); ls=sexp_cdr(ls)) {
          sexp_write_string("    ", out);
          sexp_write(sexp_car(ls), out);
          sexp_write_char('\n', out);
        }
      }
    } else {
      sexp_write_char('\n', out);
    }
  } else {
    sexp_write_string(": ", out);
    if (sexp_stringp(exn))
      sexp_write_string(sexp_string_data(exn), out);
    else
      sexp_write(exn, out);
    sexp_write_char('\n', out);
  }
  return SEXP_VOID;
}

static sexp sexp_read_error (sexp ctx, char *msg, sexp irritants, sexp port) {
  sexp res;
  sexp_gc_var(ctx, name, s_name);
  sexp_gc_var(ctx, str, s_str);
  sexp_gc_preserve(ctx, name, s_name);
  sexp_gc_preserve(ctx, str, s_str);
  name = (sexp_port_name(port)
          ? sexp_c_string(ctx, sexp_port_name(port), -1) : SEXP_FALSE);
  str = sexp_c_string(ctx, msg, -1);
  res = sexp_make_exception(ctx, the_read_error_symbol,
                            str, irritants, SEXP_FALSE, name,
                            sexp_make_integer(sexp_port_line(port)));
  sexp_gc_release(ctx, name, s_name);
  return res;
}

/*************************** list utilities ***************************/

sexp sexp_cons (sexp ctx, sexp head, sexp tail) {
  sexp pair = sexp_alloc_type(ctx, pair, SEXP_PAIR);
  sexp_car(pair) = head;
  sexp_cdr(pair) = tail;
  return pair;
}

sexp sexp_list2 (sexp ctx, sexp a, sexp b) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_preserve(ctx, res, s_res);
  res = sexp_cons(ctx, b, SEXP_NULL);
  res = sexp_cons(ctx, a, res);
  sexp_gc_release(ctx, res, s_res);
  return res;
}

sexp sexp_listp (sexp ctx, sexp hare) {
  sexp turtle;
  if (! sexp_pairp(hare))
    return sexp_make_boolean(sexp_nullp(hare));
  turtle = hare;
  hare = sexp_cdr(hare);
  for ( ; sexp_pairp(hare); turtle=sexp_cdr(turtle)) {
    if (hare == turtle) return SEXP_FALSE;
    hare = sexp_cdr(hare);
    if (sexp_pairp(hare)) hare = sexp_cdr(hare);
  }
  return sexp_make_boolean(sexp_nullp(hare));
}

sexp sexp_memq (sexp ctx, sexp x, sexp ls) {
  while (sexp_pairp(ls))
    if (x == sexp_car(ls))
      return ls;
    else
      ls = sexp_cdr(ls);
  return SEXP_FALSE;
}

sexp sexp_assq (sexp ctx, sexp x, sexp ls) {
  while (sexp_pairp(ls))
    if (sexp_pairp(sexp_car(ls)) && (x == sexp_caar(ls)))
      return sexp_car(ls);
    else
      ls = sexp_cdr(ls);
  return SEXP_FALSE;
}

sexp sexp_reverse (sexp ctx, sexp ls) {
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_preserve(ctx, res, s_res);
  for (res=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls))
    res = sexp_cons(ctx, sexp_car(ls), res);
  sexp_gc_release(ctx, res, s_res);
  return res;
}

sexp sexp_nreverse (sexp ctx, sexp ls) {
  sexp a, b, tmp;
  if (ls == SEXP_NULL) {
    return ls;
  } else if (! sexp_pairp(ls)) {
    return SEXP_NULL;  /* XXXX return an exception */
  } else {
    b = ls;
    a = sexp_cdr(ls);
    sexp_cdr(b) = SEXP_NULL;
    for ( ; sexp_pairp(a); b=a, a=tmp) {
      tmp = sexp_cdr(a);
      sexp_cdr(a) = b;
    }
    return b;
  }
}

sexp sexp_append2 (sexp ctx, sexp a, sexp b) {
  sexp_gc_var(ctx, a1, s_a1);
  sexp_gc_var(ctx, b1, s_b1);
  sexp_gc_preserve(ctx, a1, s_a1);
  sexp_gc_preserve(ctx, b1, s_b1);
  b1 = b;
  for (a1=sexp_reverse(ctx, a); sexp_pairp(a1); a1=sexp_cdr(a1))
    b1 = sexp_cons(ctx, sexp_car(a1), b1);
  sexp_gc_release(ctx, a1, s_a1);
  return b1;
}

sexp sexp_length (sexp ctx, sexp ls) {
  sexp_uint_t res=0;
  for ( ; sexp_pairp(ls); res++, ls=sexp_cdr(ls))
    ;
  return sexp_make_integer(res);
}

sexp sexp_equalp (sexp ctx, sexp a, sexp b) {
  sexp_uint_t len;
  sexp *v1, *v2;
 loop:
  if (a == b)
    return SEXP_TRUE;
  if (! sexp_pointerp(a))
    return sexp_make_boolean(sexp_integerp(a) && sexp_pointerp(b)
                             && (sexp_unbox_integer(a)
                                 == sexp_flonum_value(b)));
  else if (! sexp_pointerp(b))
    return sexp_make_boolean(sexp_integerp(b) && sexp_pointerp(a)
                             && (sexp_unbox_integer(b)
                                 == sexp_flonum_value(a)));
  if (sexp_pointer_tag(a) != sexp_pointer_tag(b))
    return SEXP_FALSE;
  switch (sexp_pointer_tag(a)) {
  case SEXP_PAIR:
    if (sexp_equalp(ctx, sexp_car(a), sexp_car(b)) == SEXP_FALSE)
      return SEXP_FALSE;
    a = sexp_cdr(a);
    b = sexp_cdr(b);
    goto loop;
  case SEXP_VECTOR:
    len = sexp_vector_length(a);
    if (len != sexp_vector_length(b))
      return SEXP_FALSE;
    v1 = sexp_vector_data(a);
    v2 = sexp_vector_data(b);
    for (len--; len > 0; len--)
      if (sexp_equalp(ctx, v1[len], v2[len]) == SEXP_FALSE)
        return SEXP_FALSE;
    return SEXP_TRUE;
  case SEXP_STRING:
    return sexp_make_boolean((sexp_string_length(a) == sexp_string_length(b))
                             && (! strncmp(sexp_string_data(a),
                                           sexp_string_data(b),
                                           sexp_string_length(a))));
  case SEXP_FLONUM:
    return sexp_make_boolean(sexp_flonum_value(a) == sexp_flonum_value(b));
  default:
    return SEXP_FALSE;
  }
}

/********************* strings, symbols, vectors **********************/

sexp sexp_make_flonum(sexp ctx, double f) {
  sexp x = sexp_alloc_type(ctx, flonum, SEXP_FLONUM);
  sexp_flonum_value(x) = f;
  return x;
}

sexp sexp_make_string(sexp ctx, sexp len, sexp ch) {
  sexp_sint_t clen = sexp_unbox_integer(len);
  sexp s = sexp_alloc_atomic(ctx, sexp_sizeof(string)+clen+1);
  sexp_pointer_tag(s) = SEXP_STRING;
  if (clen < 0) return sexp_type_exception(ctx, "negative length", len);
  sexp_string_length(s) = clen;
  if (sexp_charp(ch))
    memset(sexp_string_data(s), sexp_unbox_character(ch), clen);
  sexp_string_data(s)[clen] = '\0';
  return s;
}

sexp sexp_c_string(sexp ctx, char *str, sexp_sint_t slen) {
  sexp_sint_t len = ((slen >= 0) ? slen : strlen(str));
  sexp s = sexp_make_string(ctx, sexp_make_integer(len), SEXP_VOID);
  memcpy(sexp_string_data(s), str, len+1);
  return s;
}

sexp sexp_substring (sexp ctx, sexp str, sexp start, sexp end) {
  sexp res;
  if (! sexp_stringp(str))
    return sexp_type_exception(ctx, "not a string", str);
  if (! sexp_integerp(start))
    return sexp_type_exception(ctx, "not a number", start);
  if (end == SEXP_FALSE)
    end = sexp_make_integer(sexp_string_length(str));
  if (! sexp_integerp(end))
    return sexp_type_exception(ctx, "not a number", end);
  if ((sexp_unbox_integer(start) < 0)
      || (sexp_unbox_integer(start) > sexp_string_length(str))
      || (sexp_unbox_integer(end) < 0)
      || (sexp_unbox_integer(end) > sexp_string_length(str))
      || (end < start))
    return sexp_range_exception(ctx, str, start, end);
  res = sexp_make_string(ctx, sexp_fx_sub(end, start), SEXP_VOID);
  memcpy(sexp_string_data(res),
         sexp_string_data(str)+sexp_unbox_integer(start),
         sexp_string_length(res)+1);
  return res;
}

#if USE_HASH_SYMS

#define FNV_PRIME 16777619
#define FNV_OFFSET_BASIS 2166136261uL

static sexp_uint_t sexp_string_hash(char *str, sexp_uint_t acc) {
  while (*str) {acc *= FNV_PRIME; acc ^= *str++;}
  return acc;
}

#endif

sexp sexp_intern(sexp ctx, char *str) {
  struct huff_entry he;
  sexp_uint_t len, res=FNV_OFFSET_BASIS, space=3, newbits, bucket;
  char c, *p=str;
  sexp ls;
  sexp_gc_var(ctx, sym, s_sym);

#if USE_HUFF_SYMS
  res = 0;
  for ( ; (c=*p); p++) {
    he = huff_table[(unsigned char)c];
    newbits = he.len;
    if ((space+newbits) > (sizeof(sexp)*8)) {
      goto normal_intern;
    }
    res |= (((sexp_uint_t) he.bits) << space);
    space += newbits;
  }
  return (sexp) (res + SEXP_ISYMBOL_TAG);
#endif

 normal_intern:
#if USE_HASH_SYMS
  bucket = (sexp_string_hash(p, res) % SEXP_SYMBOL_TABLE_SIZE);
#else
  bucket = 0;
#endif
  len = strlen(str);
  for (ls=sexp_symbol_table[bucket]; sexp_pairp(ls); ls=sexp_cdr(ls))
    if (! strncmp(str, sexp_string_data(sexp_symbol_string(sexp_car(ls))), len))
      return sexp_car(ls);

  /* not found, make a new symbol */
  sexp_gc_preserve(ctx, sym, s_sym);
  sym = sexp_alloc_type(ctx, symbol, SEXP_SYMBOL);
  sexp_symbol_string(sym) = sexp_c_string(ctx, str, len);
  sexp_push(ctx, sexp_symbol_table[bucket], sym);
  sexp_gc_release(ctx, sym, s_sym);
  return sym;
}

sexp sexp_string_to_symbol (sexp ctx, sexp str) {
  return sexp_intern(ctx, sexp_string_data(str));
}

sexp sexp_make_vector(sexp ctx, sexp len, sexp dflt) {
  sexp v, *x;
  int i, clen = sexp_unbox_integer(len);
  if (! clen) return the_empty_vector;
  v = sexp_alloc_tagged(ctx, sexp_sizeof(vector) + clen*sizeof(sexp),
                        SEXP_VECTOR);
  x = sexp_vector_data(v);
  for (i=0; i<clen; i++)
    x[i] = dflt;
  sexp_vector_length(v) = clen;
  return v;
}

sexp sexp_list_to_vector(sexp ctx, sexp ls) {
  sexp x, vec = sexp_make_vector(ctx, sexp_length(ctx, ls), SEXP_VOID);
  sexp *elts = sexp_vector_data(vec);
  int i;
  for (i=0, x=ls; sexp_pairp(x); i++, x=sexp_cdr(x))
    elts[i] = sexp_car(x);
  return vec;
}

sexp sexp_vector(sexp ctx, int count, ...) {
  sexp vec = sexp_make_vector(ctx, sexp_make_integer(count), SEXP_VOID);
  sexp *elts = sexp_vector_data(vec);
  va_list ap;
  int i;

  va_start(ap, count);
  for (i=0; i<count; i++)
    elts[i] = va_arg(ap, sexp);
  va_end(ap);
  return vec;
}

/************************ reading and writing *************************/

#if USE_STRING_STREAMS

#define SEXP_INIT_STRING_PORT_SIZE 128

#if SEXP_BSD

#define sexp_stream_buf(vec) sexp_vector_ref((sexp)vec, sexp_make_integer(0))
#define sexp_stream_size(vec) sexp_vector_ref((sexp)vec, sexp_make_integer(1))
#define sexp_stream_pos(vec) sexp_vector_ref((sexp)vec, sexp_make_integer(2))

int sstream_read (void *vec, char *dst, int n) {
  sexp_uint_t len = sexp_unbox_integer(sexp_stream_size(vec));
  sexp_uint_t pos = sexp_unbox_integer(sexp_stream_pos(vec));
  if (pos >= len) return 0;
  if (n > (len - pos)) n = (len - pos);
  memcpy(dst, sexp_string_data(sexp_stream_buf(vec))+pos, n);
  sexp_stream_pos(vec) = sexp_make_integer(n);
  return n;
}

int sstream_write (void *vec, const char *src, int n) {
  sexp_uint_t len, pos, newpos;
  sexp newbuf;
  len = sexp_unbox_integer(sexp_stream_size(vec));
  pos = sexp_unbox_integer(sexp_stream_pos(vec));
  newpos = pos+n;
  if (newpos >= len) {
    newbuf = sexp_make_string(NULL, sexp_make_integer(newpos*2), SEXP_VOID);
    memcpy(sexp_string_data(newbuf),
           sexp_string_data(sexp_stream_buf(vec)),
           pos);
    sexp_stream_buf(vec) = newbuf;
    sexp_stream_size(vec) = sexp_make_integer(newpos*2);
  }
  memcpy(sexp_string_data(sexp_stream_buf(vec))+pos, src, n);
  sexp_stream_pos(vec) = sexp_make_integer(newpos);
  return n;
}

off_t sstream_seek (void *vec, off_t offset, int whence) {
  sexp_sint_t pos;
  if (whence == SEEK_SET) {
    pos = offset;
  } else if (whence == SEEK_CUR) {
    pos = sexp_unbox_integer(sexp_stream_pos(vec)) + offset;
  } else {                      /* SEEK_END */
    pos = sexp_unbox_integer(sexp_stream_size(vec)) + offset;
  }
  sexp_stream_pos(vec) = sexp_make_integer(pos);
  return pos;
}

sexp sexp_make_input_string_port (sexp ctx, sexp str) {
  FILE *in;
  sexp res;
  sexp_gc_var(ctx, cookie, s_cookie);
  sexp_gc_preserve(ctx, cookie, s_cookie);
  cookie = sexp_vector(ctx, 3, str, sexp_make_integer(sexp_string_length(str)),
                       sexp_make_integer(0));
  in = funopen(cookie, &sstream_read, NULL, &sstream_seek, NULL);
  res = sexp_make_input_port(ctx, in, NULL);
  sexp_port_cookie(res) = cookie;
  sexp_gc_release(ctx, cookie, s_cookie);
  return res;
}

sexp sexp_make_output_string_port (sexp ctx) {
  FILE *out;
  sexp res, size;
  sexp_gc_var(ctx, cookie, s_cookie);
  sexp_gc_preserve(ctx, cookie, s_cookie);
  size = sexp_make_integer(SEXP_INIT_STRING_PORT_SIZE);
  cookie = sexp_vector(ctx, 3, sexp_make_string(NULL, size, SEXP_VOID),
                       size, sexp_make_integer(0));
  out = funopen(cookie, NULL, &sstream_write, &sstream_seek, NULL);
  res = sexp_make_output_port(ctx, out, NULL);
  sexp_port_cookie(res) = cookie;
  sexp_gc_release(ctx, cookie, s_cookie);
  return res;
}

sexp sexp_get_output_string (sexp ctx, sexp port) {
  sexp cookie = sexp_port_cookie(port);
  fflush(sexp_port_stream(port));
  return sexp_substring(ctx,
                        sexp_stream_buf(cookie),
                        sexp_make_integer(0),
                        sexp_stream_pos(cookie));
}

#else

sexp sexp_make_input_string_port (sexp ctx, sexp str) {
  FILE *in = fmemopen(sexp_string_data(str), sexp_string_length(str), "r");
  return sexp_make_input_port(in, NULL);
}

sexp sexp_make_output_string_port (sexp ctx) {
  FILE *out;
  sexp buf = sexp_alloc_type(ctx, string, SEXP_STRING), res;
  out = open_memstream(&sexp_string_data(buf), &sexp_string_length(buf));
  res = sexp_make_input_port(out, NULL);
  sexp_port_cookie(res) = buf;
  return res;
}

sexp sexp_get_output_string (sexp ctx, sexp port) {
  sexp cookie = sexp_port_cookie(port);
  fflush(sexp_port_stream(port));
  return sexp_substring(cookie,
                        sexp_make_integer(0),
                        sexp_make_integer(sexp_string_length(cookie)));
}

#endif

#endif

sexp sexp_make_input_port (sexp ctx, FILE* in, char *path) {
  sexp p = sexp_alloc_type(ctx, port, SEXP_IPORT);
  sexp_port_stream(p) = in;
  sexp_port_name(p) = path;
  sexp_port_line(p) = 0;
  return p;
}

sexp sexp_make_output_port (sexp ctx, FILE* out, char *path) {
  sexp p = sexp_alloc_type(ctx, port, SEXP_OPORT);
  sexp_port_stream(p) = out;
  sexp_port_name(p) = path;
  sexp_port_line(p) = 0;
  return p;
}

void sexp_write (sexp obj, sexp out) {
  unsigned long len, c, res;
  long i=0;
  double f;
  sexp x, *elts;
  char *str=NULL;

  if (! obj) {
    sexp_write_string("#<null>", out);
  } else if (sexp_pointerp(obj)) {
    switch (sexp_pointer_tag(obj)) {
    case SEXP_PAIR:
      sexp_write_char('(', out);
      sexp_write(sexp_car(obj), out);
      for (x=sexp_cdr(obj); sexp_pairp(x); x=sexp_cdr(x)) {
        sexp_write_char(' ', out);
        sexp_write(sexp_car(x), out);
      }
      if (! sexp_nullp(x)) {
        sexp_write_string(" . ", out);
        sexp_write(x, out);
      }
      sexp_write_char(')', out);
      break;
    case SEXP_VECTOR:
      len = sexp_vector_length(obj);
      elts = sexp_vector_data(obj);
      if (len == 0) {
        sexp_write_string("#()", out);
      } else {
        sexp_write_string("#(", out);
        sexp_write(elts[0], out);
        for (i=1; i<len; i++) {
          sexp_write_char(' ', out);
          sexp_write(elts[i], out);
        }
        sexp_write_char(')', out);
      }
      break;
    case SEXP_FLONUM:
      f = sexp_flonum_value(obj);
      sexp_printf(out, "%.15g%s", f, (f == trunc(f)) ? ".0" : "");
      break;
    case SEXP_PROCEDURE:
      sexp_write_string("#<procedure: ", out);
      sexp_write(sexp_bytecode_name(sexp_procedure_code(obj)), out);
      sexp_write_string(">", out);
      break;
    case SEXP_IPORT:
      sexp_write_string("#<input-port>", out); break;
    case SEXP_OPORT:
      sexp_write_string("#<output-port>", out); break;
    case SEXP_CORE:
      sexp_write_string("#<core-form>", out); break;
    case SEXP_OPCODE:
      sexp_write_string("#<opcode>", out); break;
    case SEXP_BYTECODE:
      sexp_write_string("#<bytecode>", out); break;
    case SEXP_ENV:
      sexp_printf(out, "#<env %p>", obj); break;
    case SEXP_EXCEPTION:
      sexp_write_string("#<exception>", out); break;
    case SEXP_MACRO:
      sexp_write_string("#<macro>", out); break;
#if USE_DEBUG
    case SEXP_LAMBDA:
      sexp_write_string("#<lambda ", out);
      sexp_write(sexp_lambda_params(obj), out);
      sexp_write_char(' ', out);
      sexp_write(sexp_lambda_body(obj), out);
      sexp_write_char('>', out);
      break;
    case SEXP_SEQ:
      sexp_write_string("#<seq ", out);
      sexp_write(sexp_seq_ls(obj), out);
      sexp_write_char('>', out);
      break;
    case SEXP_CND:
      sexp_write_string("#<if ", out);
      sexp_write(sexp_cnd_test(obj), out);
      sexp_write_char(' ', out);
      sexp_write(sexp_cnd_pass(obj), out);
      sexp_write_char(' ', out);
      sexp_write(sexp_cnd_fail(obj), out);
      sexp_write_char('>', out);
      break;
    case SEXP_REF:
      sexp_write_string("#<ref: ", out);
      sexp_write(sexp_ref_name(obj), out);
      sexp_printf(out, " %p>", sexp_ref_loc(obj));
      break;
    case SEXP_SET:
      sexp_write_string("#<set! ", out);
      sexp_write(sexp_set_var(obj), out);
      sexp_write_char(' ', out);
      sexp_write(sexp_set_value(obj), out);
      sexp_write_string(">", out);
      break;
    case SEXP_SYNCLO:
      sexp_write_string("#<sc ", out);
      sexp_write(sexp_synclo_expr(obj), out);
      sexp_write_string(">", out);
      break;
#endif
    case SEXP_TYPE:
      sexp_write_string("#<type ", out);
      sexp_write_string(sexp_type_name(obj), out);
      sexp_write_string(">", out);
      break;
    case SEXP_STRING:
      sexp_write_char('"', out);
      i = sexp_string_length(obj);
      str = sexp_string_data(obj);
      for ( ; i>0; str++, i--) {
        switch (str[0]) {
        case '\\': sexp_write_string("\\\\", out); break;
        case '"': sexp_write_string("\\\"", out); break;
        case '\n': sexp_write_string("\\n", out); break;
        case '\r': sexp_write_string("\\r", out); break;
        case '\t': sexp_write_string("\\t", out); break;
        default: sexp_write_char(str[0], out);
        }
      }
      sexp_write_char('"', out);
      break;
    case SEXP_SYMBOL:
      i = sexp_string_length(sexp_symbol_string(obj));
      str = sexp_string_data(sexp_symbol_string(obj));
      for ( ; i>0; str++, i--) {
        if ((str[0] == '\\') || is_separator(str[0]))
          sexp_write_char('\\', out);
        sexp_write_char(str[0], out);
      }
      break;
    }
  } else if (sexp_integerp(obj)) {
    sexp_printf(out, "%ld", sexp_unbox_integer(obj));
  } else if (sexp_charp(obj)) {
    if (obj == sexp_make_character(' '))
      sexp_write_string("#\\space", out);
    else if (obj == sexp_make_character('\n'))
      sexp_write_string("#\\newline", out);
    else if (obj == sexp_make_character('\r'))
      sexp_write_string("#\\return", out);
    else if (obj == sexp_make_character('\t'))
      sexp_write_string("#\\tab", out);
    else if ((33 <= sexp_unbox_character(obj))
             && (sexp_unbox_character(obj) < 127))
      sexp_printf(out, "#\\%c", sexp_unbox_character(obj));
    else
      sexp_printf(out, "#\\x%02d", sexp_unbox_character(obj));
  } else if (sexp_symbolp(obj)) {

#if USE_HUFF_SYMS
    if (((sexp_uint_t)obj&7)==7) {
      c = ((sexp_uint_t)obj)>>3;
      while (c) {
#include "opt/sexp-unhuff.c"
        sexp_write_char(res, out);
      }
    }
#endif

  } else {
    switch ((sexp_uint_t) obj) {
    case (sexp_uint_t) SEXP_NULL:
      sexp_write_string("()", out); break;
    case (sexp_uint_t) SEXP_TRUE:
      sexp_write_string("#t", out); break;
    case (sexp_uint_t) SEXP_FALSE:
      sexp_write_string("#f", out); break;
    case (sexp_uint_t) SEXP_EOF:
      sexp_write_string("#<eof>", out); break;
    case (sexp_uint_t) SEXP_UNDEF:
    case (sexp_uint_t) SEXP_VOID:
      sexp_write_string("#<undef>", out); break;
    default:
      sexp_printf(out, "#<invalid: %p>", obj);
    }
  }
}

#define INIT_STRING_BUFFER_SIZE 128

sexp sexp_read_string(sexp ctx, sexp in) {
  int c, i=0, size=INIT_STRING_BUFFER_SIZE;
  char initbuf[INIT_STRING_BUFFER_SIZE];
  char *buf=initbuf, *tmp;
  sexp res;

  for (c = sexp_read_char(in); c != '"'; c = sexp_read_char(in)) {
    if (c == '\\') {
      c = sexp_read_char(in);
      switch (c) {case 'n': c = '\n'; break; case 't': c = '\t'; break;}
    }
    if (c == EOF) {
      res = sexp_read_error(ctx, "premature end of string", SEXP_NULL, in);
      break;
    }
    buf[i++] = c;
    if (i >= size) {       /* expand buffer w/ malloc(), later free() it */
      tmp = malloc(size*2);
      memcpy(tmp, buf, i);
      if (size != INIT_STRING_BUFFER_SIZE) free(buf);
      buf = tmp;
      size *= 2;
    }
  }

  buf[i] = '\0';
  res = sexp_c_string(ctx, buf, i);
  if (size != INIT_STRING_BUFFER_SIZE) free(buf);
  return res;
}

sexp sexp_read_symbol(sexp ctx, sexp in, int init, int internp) {
  int c, i=0, size=INIT_STRING_BUFFER_SIZE;
  char initbuf[INIT_STRING_BUFFER_SIZE];
  char *buf=initbuf, *tmp;
  sexp res;

  if (init != EOF)
    buf[i++] = init;

  for (c = sexp_read_char(in); c != '"'; c = sexp_read_char(in)) {
    if (c == '\\') c = sexp_read_char(in);
    if (c == EOF || is_separator(c)) {
      sexp_push_char(c, in);
      break;
    }
    buf[i++] = c;
    if (i >= size) {       /* expand buffer w/ malloc(), later free() it */
      tmp = malloc(size*2);
      memcpy(tmp, buf, i);
      if (size != INIT_STRING_BUFFER_SIZE) free(buf);
      buf = tmp;
      size *= 2;
    }
  }

  buf[i] = '\0';
  res = (internp ? sexp_intern(ctx, buf) : sexp_c_string(ctx, buf, i));
  if (size != INIT_STRING_BUFFER_SIZE) free(buf);
  return res;
}

sexp sexp_read_float_tail(sexp ctx, sexp in, sexp_sint_t whole) {
  sexp exponent;
  double res=0.0, scale=0.1, e=0.0;
  int c;
  for (c=sexp_read_char(in); isdigit(c); c=sexp_read_char(in), scale*=0.1)
    res += digit_value(c)*scale;
  sexp_push_char(c, in);
  if (c=='e' || c=='E') {
    exponent = sexp_read_number(ctx, in, 10);
    if (sexp_exceptionp(exponent)) return exponent;
    e = (sexp_integerp(exponent) ? sexp_unbox_integer(exponent)
         : sexp_flonump(exponent) ? sexp_flonum_value(exponent) : 0.0);
  } else if ((c!=EOF) && ! is_separator(c))
    return sexp_read_error(ctx, "invalid numeric syntax",
                           sexp_list1(ctx, sexp_make_character(c)), in);
  return sexp_make_flonum(ctx, (whole + res) * pow(10, e));
}

sexp sexp_read_number(sexp ctx, sexp in, int base) {
  sexp f;
  sexp_sint_t res = 0, negativep = 0, c;

  c = sexp_read_char(in);
  if (c == '-')
    negativep = 1;
  else if (isdigit(c))
    res = digit_value(c);

  if (base == 16)
    for (c=sexp_read_char(in); isxdigit(c); c=sexp_read_char(in))
      res = res * base + digit_value(c);
    for (c=sexp_read_char(in); isdigit(c); c=sexp_read_char(in))
      res = res * base + digit_value(c);

  if (c=='.' || c=='e' || c=='E') {
    if (base != 10)
      return sexp_read_error(ctx, "decimal found in non-base 10", SEXP_NULL, in);
    if (c!='.')
      sexp_push_char(c, in);
    f = sexp_read_float_tail(ctx, in, res);
    if (! sexp_flonump(f)) return f;
    if ((c!='.') && (sexp_flonum_value(f) == round(sexp_flonum_value(f)))) {
      res = (sexp_sint_t) sexp_flonum_value(f);
    } else {
      if (negativep) sexp_flonum_value(f) = -sexp_flonum_value(f);
      return f;
    }
  } else {
    sexp_push_char(c, in);
    if ((c!=EOF) && ! is_separator(c))
      return sexp_read_error(ctx, "invalid numeric syntax",
                             sexp_list1(ctx, sexp_make_character(c)), in);
  }

  return sexp_make_integer(negativep ? -res : res);
}

sexp sexp_read_raw (sexp ctx, sexp in) {
  char *str;
  int c1, c2;
  sexp tmp2;
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, res, s_res);
  sexp_gc_preserve(ctx, tmp, s_tmp);

 scan_loop:
  switch (c1 = sexp_read_char(in)) {
  case EOF:
    res = SEXP_EOF;
    break;
  case ';':
    sexp_port_line(in)++;
    while ((c1 = sexp_read_char(in)) != EOF)
      if (c1 == '\n')
        break;
    /* ... FALLTHROUGH ... */
  case ' ':
  case '\t':
  case '\r':
    goto scan_loop;
  case '\n':
    sexp_port_line(in)++;
    goto scan_loop;
  case '\'':
    res = sexp_read(ctx, in);
    res = sexp_list2(ctx, the_quote_symbol, res);
    break;
  case '`':
    res = sexp_read(ctx, in);
    res = sexp_list2(ctx, the_quasiquote_symbol, res);
    break;
  case ',':
    if ((c1 = sexp_read_char(in)) == '@') {
      res = sexp_read(ctx, in);
      res = sexp_list2(ctx, the_unquote_splicing_symbol, res);
    } else {
      sexp_push_char(c1, in);
      res = sexp_read(ctx, in);
      res = sexp_list2(ctx, the_unquote_symbol, res);
    }
    break;
  case '"':
    res = sexp_read_string(ctx, in);
    break;
  case '(':
    res = SEXP_NULL;
    tmp = sexp_read_raw(ctx, in);
    while ((tmp != SEXP_EOF) && (tmp != SEXP_CLOSE) && (tmp != SEXP_RAWDOT)) {
      res = sexp_cons(ctx, tmp, res);
      tmp = sexp_read_raw(ctx, in);
      if (sexp_exceptionp(tmp)) {
        res = tmp;
        break;
      }
    }
    if (! sexp_exceptionp(res)) {
      if (tmp == SEXP_RAWDOT) { /* dotted list */
        if (res == SEXP_NULL) {
          res = sexp_read_error(ctx, "dot before any elements in list",
                                SEXP_NULL, in);
        } else {
          tmp = sexp_read_raw(ctx, in);
          if (sexp_exceptionp(tmp)) {
            res = tmp;
          } else if (tmp == SEXP_CLOSE) {
            res = sexp_read_error(ctx, "no final element in list after dot",
                                  SEXP_NULL, in);
          } else if (sexp_read_raw(ctx, in) != SEXP_CLOSE) {
            res = sexp_read_error(ctx, "multiple tokens in dotted tail",
                                  SEXP_NULL, in);
          } else {
            tmp2 = res;
            res = sexp_nreverse(ctx, res);
            sexp_cdr(tmp2) = tmp;
          }
        }
      } else if (tmp == SEXP_CLOSE) {
        res = (sexp_pairp(res) ? sexp_nreverse(ctx, res) : res);
      } else {
        res = sexp_read_error(ctx, "missing trailing ')'", SEXP_NULL, in);
      }
    }
    break;
  case '#':
    switch (c1=sexp_read_char(in)) {
    case 'b':
      res = sexp_read_number(ctx, in, 2); break;
    case 'o':
      res = sexp_read_number(ctx, in, 8); break;
    case 'd':
      res = sexp_read_number(ctx, in, 10); break;
    case 'x':
      res = sexp_read_number(ctx, in, 16); break;
    case 'e':
      res = sexp_read(ctx, in);
      if (sexp_flonump(res))
        res = sexp_make_integer((sexp_sint_t)sexp_flonum_value(res));
      break;
    case 'i':
      res = sexp_read(ctx, in);
      if (sexp_integerp(res))
        res = sexp_make_flonum(ctx, sexp_unbox_integer(res));
      break;
    case 'f':
    case 't':
      c2 = sexp_read_char(in);
      if (c2 == EOF || is_separator(c2)) {
        res = (c1 == 't' ? SEXP_TRUE : SEXP_FALSE);
        sexp_push_char(c2, in);
      } else {
        res = sexp_read_error(ctx, "invalid syntax #%c%c",
                              sexp_list2(ctx,
                                         sexp_make_character(c1),
                                         sexp_make_character(c2)),
                              in);
      }
      break;
/*     case '0': case '1': case '2': case '3': case '4': */
/*     case '5': case '6': case '7': case '8': case '9': */
    case ';':
      tmp = sexp_read_raw(ctx, in);   /* discard */
      if (sexp_exceptionp(tmp))
        res = tmp;
      else
        goto scan_loop;
    case '\\':
      c1 = sexp_read_char(in);
      res = sexp_read_symbol(ctx, in, c1, 0);
      if (sexp_stringp(res)) {
        str = sexp_string_data(res);
        if (sexp_string_length(res) == 0)
          res =
            sexp_read_error(ctx, "unexpected end of character literal",
                            SEXP_NULL, in);
        if (sexp_string_length(res) == 1) {
          res = sexp_make_character(c1);
        } else if ((c1 == 'x' || c1 == 'X') &&
                   isxdigit(str[0]) && isxdigit(str[1]) && str[2] == '\0') {
          res = sexp_make_character(16 * digit_value(c1) + digit_value(str[1]));
        } else {
          if (strcasecmp(str, "space") == 0)
            res = sexp_make_character(' ');
          else if (strcasecmp(str, "newline") == 0)
            res = sexp_make_character('\n');
          else if (strcasecmp(str, "return") == 0)
            res = sexp_make_character('\r');
          else if (strcasecmp(str, "tab") == 0)
            res = sexp_make_character('\t');
          else {
            res = sexp_read_error(ctx, "unknown character name",
                                  sexp_list1(ctx, sexp_c_string(ctx, str, -1)),
                                  in);
          }
        }
      }
      break;
    case '(':
      sexp_push_char(c1, in);
      res = sexp_read(ctx, in);
      if (sexp_listp(ctx, res) == SEXP_FALSE) {
        if (! sexp_exceptionp(res)) {
          res = sexp_read_error(ctx, "dotted list not allowed in vector syntax",
                                SEXP_NULL,
                                in);
        }
      } else {
        res = sexp_list_to_vector(ctx, res);
      }
      break;
    default:
      res = sexp_read_error(ctx, "invalid # syntax",
                            sexp_list1(ctx, sexp_make_character(c1)), in);
    }
    break;
  case '.':
    c1 = sexp_read_char(in);
    if (c1 == EOF || is_separator(c1)) {
      res = SEXP_RAWDOT;
    } else if (isdigit(c1)) {
      sexp_push_char(c1,in );
      res = sexp_read_float_tail(ctx, in, 0);
    } else {
      sexp_push_char(c1, in);
      res = sexp_read_symbol(ctx, in, '.', 1);
    }
    break;
  case ')':
    res = SEXP_CLOSE;
    break;
  case '+':
  case '-':
    c2 = sexp_read_char(in);
    if (c2 == '.' || isdigit(c2)) {
      sexp_push_char(c2, in);
      res = sexp_read_number(ctx, in, 10);
      if ((c1 == '-') && ! sexp_exceptionp(res)) {
#ifdef USE_FLONUMS
        if (sexp_flonump(res))
          sexp_flonum_value(res) = -1 * sexp_flonum_value(res);
        else
#endif
          res = sexp_fx_mul(res, -1);
      }
    } else {
      sexp_push_char(c2, in);
      res = sexp_read_symbol(ctx, in, c1, 1);
    }
    break;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    sexp_push_char(c1, in);
    res = sexp_read_number(ctx, in, 10);
    break;
  default:
    res = sexp_read_symbol(ctx, in, c1, 1);
    break;
  }

  sexp_gc_release(ctx, res, s_res);
  return res;
}

sexp sexp_read (sexp ctx, sexp in) {
  sexp res = sexp_read_raw(ctx, in);
  if (res == SEXP_CLOSE)
    return sexp_read_error(ctx, "too many ')'s", SEXP_NULL, in);
  if (res == SEXP_RAWDOT)
    return sexp_read_error(ctx, "unexpected '.'", SEXP_NULL, in);
  return res;
}

#if USE_STRING_STREAMS
sexp sexp_read_from_string(sexp ctx, char *str) {
  sexp res;
  sexp_gc_var(ctx, s, s_s);
  sexp_gc_var(ctx, in, s_in);
  sexp_gc_preserve(ctx, s, s_s);
  sexp_gc_preserve(ctx, in, s_in);
  s = sexp_c_string(ctx, str, -1);
  in = sexp_make_input_string_port(ctx, s);
  res = sexp_read(ctx, in);
  sexp_gc_release(ctx, s, s_s);
  return res;
}
#endif

void sexp_init() {
  int i;
  sexp ctx;
  if (! sexp_initialized_p) {
    sexp_initialized_p = 1;
#if USE_BOEHM
    GC_init();
    GC_add_roots((char*)&sexp_symbol_table,
                 ((char*)&sexp_symbol_table)+sizeof(sexp_symbol_table)+1);
#elif ! USE_MALLOC
    sexp_gc_init();
#endif
    for (i=0; i<SEXP_SYMBOL_TABLE_SIZE; i++)
      sexp_symbol_table[i] = SEXP_NULL;
    ctx = sexp_alloc_type(NULL, context, SEXP_CONTEXT);
    the_dot_symbol = sexp_intern(ctx, ".");
    the_quote_symbol = sexp_intern(ctx, "quote");
    the_quasiquote_symbol = sexp_intern(ctx, "quasiquote");
    the_unquote_symbol = sexp_intern(ctx, "unquote");
    the_unquote_splicing_symbol = sexp_intern(ctx, "unquote-splicing");
    the_read_error_symbol = sexp_intern(ctx, "read");
    the_empty_vector = sexp_alloc_type(ctx, vector, SEXP_VECTOR);
    sexp_vector_length(the_empty_vector) = 0;
  }
}

