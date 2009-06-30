/*  sexp.c -- standalone sexp library implementation     */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#define SEXP_API
#include "chibi/sexp.h"

/* optional huffman-compressed immediate symbols */
struct sexp_huff_entry {
  unsigned char len;
  unsigned short bits;
};

#if USE_HUFF_SYMS
#include "opt/sexp-hufftabs.c"
static struct sexp_huff_entry huff_table[] = {
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

static int hex_digit (n) {
  return ((n<=9) ? ('0' + n) : ('A' + n - 10));
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

#if ! USE_BOEHM

#define _DEF_TYPE(t,fb,flb,flo,fls,sb,so,sc,n) \
  {.tag=SEXP_TYPE, .value={.type={t,fb,flb,flo,fls,sb,so,sc,n}}}

static struct sexp_struct sexp_type_specs[] = {
  _DEF_TYPE(SEXP_OBJECT, 0, 0, 0, 0, 0, 0, 0, "object"),
  _DEF_TYPE(SEXP_TYPE, 0, 0, 0, 0, sexp_sizeof(type), 0, 0, "type"),
  _DEF_TYPE(SEXP_FIXNUM, 0, 0, 0, 0, 0, 0, 0, "fixnum"),
  _DEF_TYPE(SEXP_CHAR, 0, 0, 0, 0, 0, 0, 0, "char"),
  _DEF_TYPE(SEXP_BOOLEAN, 0, 0, 0, 0, 0, 0, 0, "boolean"),
  _DEF_TYPE(SEXP_PAIR, sexp_offsetof(pair, car), 3, 0, 0, sexp_sizeof(pair), 0, 0, "pair"),
  _DEF_TYPE(SEXP_SYMBOL, sexp_offsetof(symbol, string), 1, 0, 0, sexp_sizeof(symbol), 0, 0, "symbol"),
  _DEF_TYPE(SEXP_STRING, 0, 0, 0, 0, sexp_sizeof(string)+1, sexp_offsetof(string, length), 1, "string"),
  _DEF_TYPE(SEXP_VECTOR, sexp_offsetof(vector, data), 0, sexp_offsetof(vector, length), 1, sexp_sizeof(vector), sexp_offsetof(vector, length), 4, "vector"),
  _DEF_TYPE(SEXP_FLONUM, 0, 0, 0, 0, sexp_sizeof(flonum), 0, 0, "flonum"),
  _DEF_TYPE(SEXP_BIGNUM, 0, 0, 0, 0, sexp_sizeof(bignum), sexp_offsetof(bignum, length), 4, "bignum"),
  _DEF_TYPE(SEXP_IPORT, sexp_offsetof(port, name), 2, 0, 0, sexp_sizeof(port), 0, 0, "input-port"),
  _DEF_TYPE(SEXP_OPORT, sexp_offsetof(port, name), 2, 0, 0, sexp_sizeof(port), 0, 0, "output-port"),
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

#if ! USE_MALLOC
#include "gc.c"
#endif

#endif  /* ! USE_BOEHM */

/***************************** exceptions *****************************/

sexp sexp_make_exception (sexp ctx, sexp kind, sexp message, sexp irritants,
                          sexp procedure, sexp source) {
  sexp exn = sexp_alloc_type(ctx, exception, SEXP_EXCEPTION);
  sexp_exception_kind(exn) = kind;
  sexp_exception_message(exn) = message;
  sexp_exception_irritants(exn) = irritants;
  sexp_exception_procedure(exn) = procedure;
  sexp_exception_source(exn) = source;
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
                            self, SEXP_FALSE);
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
                            SEXP_FALSE, SEXP_FALSE);
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
                            SEXP_FALSE, SEXP_FALSE);
  sexp_gc_release(ctx, res, s_res);
  return res;
}

sexp sexp_print_exception (sexp ctx, sexp exn, sexp out) {
  sexp ls;
  sexp_write_string(ctx, "ERROR", out);
  if (sexp_exceptionp(exn)) {
    if (sexp_procedurep(sexp_exception_procedure(exn))) {
      ls = sexp_bytecode_name(
             sexp_procedure_code(sexp_exception_procedure(exn)));
      if (sexp_symbolp(ls)) {
        sexp_write_string(ctx, " in ", out);
        sexp_write(ctx, ls, out);
      }
    }
    if (sexp_pairp(sexp_exception_source(exn))) {
      if (sexp_integerp(sexp_cdr(sexp_exception_source(exn)))
          && (sexp_cdr(sexp_exception_source(exn)) >= sexp_make_integer(0))) {
        sexp_write_string(ctx, " on line ", out);
        sexp_write(ctx, sexp_cdr(sexp_exception_source(exn)), out);
      }
      if (sexp_stringp(sexp_car(sexp_exception_source(exn)))) {
        sexp_write_string(ctx, " of file ", out);
        sexp_write_string(ctx, sexp_string_data(sexp_car(sexp_exception_source(exn))), out);
      }
    }
    sexp_write_string(ctx, ": ", out);
    sexp_write_string(ctx, sexp_string_data(sexp_exception_message(exn)), out);
    if (sexp_exception_irritants(exn)
        && sexp_pairp(sexp_exception_irritants(exn))) {
      if (sexp_nullp(sexp_cdr(sexp_exception_irritants(exn)))) {
        sexp_write_string(ctx, ": ", out);
        sexp_write(ctx, sexp_car(sexp_exception_irritants(exn)), out);
        sexp_write_string(ctx, "\n", out);
      } else {
        sexp_write_string(ctx, "\n", out);
        for (ls=sexp_exception_irritants(exn);
             sexp_pairp(ls); ls=sexp_cdr(ls)) {
          sexp_write_string(ctx, "    ", out);
          sexp_write(ctx, sexp_car(ls), out);
          sexp_write_char(ctx, '\n', out);
        }
      }
    } else {
      sexp_write_char(ctx, '\n', out);
    }
  } else {
    sexp_write_string(ctx, ": ", out);
    if (sexp_stringp(exn))
      sexp_write_string(ctx, sexp_string_data(exn), out);
    else
      sexp_write(ctx, exn, out);
    sexp_write_char(ctx, '\n', out);
  }
  return SEXP_VOID;
}

static sexp sexp_read_error (sexp ctx, char *msg, sexp irritants, sexp port) {
  sexp res;
  sexp_gc_var(ctx, name, s_name);
  sexp_gc_var(ctx, str, s_str);
  sexp_gc_var(ctx, irr, s_irr);
  sexp_gc_var(ctx, src, s_src);
  sexp_gc_preserve(ctx, name, s_name);
  sexp_gc_preserve(ctx, str, s_str);
  sexp_gc_preserve(ctx, irr, s_irr);
  sexp_gc_preserve(ctx, src, s_src);
  name = (sexp_port_name(port) ? sexp_port_name(port) : SEXP_FALSE);
  name = sexp_cons(ctx, name, sexp_make_integer(sexp_port_line(port)));
  str = sexp_c_string(ctx, msg, -1);
  irr = ((sexp_pairp(irritants) || sexp_nullp(irritants))
         ? irritants : sexp_list1(ctx, irritants));
  res = sexp_make_exception(ctx, the_read_error_symbol,
                            str, irr, SEXP_FALSE, name);
  sexp_gc_release(ctx, name, s_name);
  return res;
}

/*************************** list utilities ***************************/

sexp sexp_cons (sexp ctx, sexp head, sexp tail) {
  sexp pair = sexp_alloc_type(ctx, pair, SEXP_PAIR);
  sexp_car(pair) = head;
  sexp_cdr(pair) = tail;
  sexp_pair_source(pair) = SEXP_FALSE;
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
#if USE_IMMEDIATE_FLONUMS
  if ((! sexp_pointerp(a)) || (! sexp_pointerp(b)))
    return
      sexp_make_boolean((a == b)
                        || (sexp_flonump(a)
                            && sexp_make_integer(sexp_flonum_value(a)) == b)
                        || (sexp_flonump(b)
                            && sexp_make_integer(sexp_flonum_value(b)) == a));
#else
  if (! sexp_pointerp(a))
    return sexp_make_boolean(sexp_integerp(a) && sexp_pointerp(b)
                             && (sexp_unbox_integer(a)
                                 == sexp_flonum_value(b)));
  else if (! sexp_pointerp(b))
    return sexp_make_boolean(sexp_integerp(b) && sexp_pointerp(a)
                             && (sexp_unbox_integer(b)
                                 == sexp_flonum_value(a)));
#endif
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
#if ! USE_IMMEDIATE_FLONUMS
  case SEXP_FLONUM:
    return sexp_make_boolean(sexp_flonum_value(a) == sexp_flonum_value(b));
#endif
  default:
    return SEXP_FALSE;
  }
}

/********************* strings, symbols, vectors **********************/

#if ! USE_IMMEDIATE_FLONUMS
sexp sexp_make_flonum(sexp ctx, double f) {
  sexp x = sexp_alloc_type(ctx, flonum, SEXP_FLONUM);
  sexp_flonum_value(x) = f;
  return x;
}
#endif

sexp sexp_make_string(sexp ctx, sexp len, sexp ch) {
  sexp_sint_t clen = sexp_unbox_integer(len);
  sexp s;
  if (clen < 0) return sexp_type_exception(ctx, "negative length", len);
  s = sexp_alloc_atomic(ctx, sexp_sizeof(string)+clen+1);
  sexp_pointer_tag(s) = SEXP_STRING;
  sexp_string_length(s) = clen;
  if (sexp_charp(ch))
    memset(sexp_string_data(s), sexp_unbox_character(ch), clen);
  sexp_string_data(s)[clen] = '\0';
  return s;
}

sexp sexp_c_string(sexp ctx, char *str, sexp_sint_t slen) {
  sexp_sint_t len = ((slen >= 0) ? slen : strlen(str));
  sexp s = sexp_make_string(ctx, sexp_make_integer(len), SEXP_VOID);
  memcpy(sexp_string_data(s), str, len);
  sexp_string_data(s)[len] = '\0';
  return s;
}

sexp sexp_substring (sexp ctx, sexp str, sexp start, sexp end) {
  sexp res;
  if (! sexp_stringp(str))
    return sexp_type_exception(ctx, "not a string", str);
  if (! sexp_integerp(start))
    return sexp_type_exception(ctx, "not a number", start);
  if (sexp_not(end))
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
         sexp_string_length(res));
  sexp_string_data(res)[sexp_string_length(res)] = '\0';
  return res;
}

sexp sexp_string_concatenate (sexp ctx, sexp str_ls) {
  sexp res, ls;
  sexp_uint_t len=0;
  char *p;
  for (ls=str_ls; sexp_pairp(ls); ls=sexp_cdr(ls))
    if (! sexp_stringp(sexp_car(ls)))
      return sexp_type_exception(ctx, "not a string", sexp_car(ls));
    else
      len += sexp_string_length(sexp_car(ls));
  res = sexp_make_string(ctx, sexp_make_integer(len), SEXP_VOID);
  p = sexp_string_data(res);
  for (ls=str_ls; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    len = sexp_string_length(sexp_car(ls));
    memcpy(p, sexp_string_data(sexp_car(ls)), len);
    p += len;
  }
  *p = '\0';
  return res;
}

#define FNV_PRIME 16777619
#define FNV_OFFSET_BASIS 2166136261uL

#if USE_HASH_SYMS

static sexp_uint_t sexp_string_hash(char *str, sexp_uint_t acc) {
  while (*str) {acc *= FNV_PRIME; acc ^= *str++;}
  return acc;
}

#endif

sexp sexp_intern(sexp ctx, char *str) {
  struct sexp_huff_entry he;
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

/************************ reading and writing *************************/

#if USE_STRING_STREAMS

#define SEXP_INIT_STRING_PORT_SIZE 128

#if SEXP_BSD

#define sexp_stream_ctx(vec) sexp_vector_ref((sexp)vec, sexp_make_integer(0))
#define sexp_stream_buf(vec) sexp_vector_ref((sexp)vec, sexp_make_integer(1))
#define sexp_stream_size(vec) sexp_vector_ref((sexp)vec, sexp_make_integer(2))
#define sexp_stream_pos(vec) sexp_vector_ref((sexp)vec, sexp_make_integer(3))

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
    newbuf = sexp_make_string(sexp_stream_ctx(vec),
                              sexp_make_integer(newpos*2),
                              SEXP_VOID);
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
  cookie = sexp_make_vector(ctx, sexp_make_integer(4), SEXP_VOID);
  sexp_stream_ctx(cookie) = ctx;
  sexp_stream_buf(cookie) = str;
  sexp_stream_size(cookie) = sexp_make_integer(sexp_string_length(str));
  sexp_stream_pos(cookie) = sexp_make_integer(0);
  in = funopen(cookie, &sstream_read, NULL, &sstream_seek, NULL);
  res = sexp_make_input_port(ctx, in, SEXP_FALSE);
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
  cookie = sexp_make_vector(ctx, sexp_make_integer(4), SEXP_VOID);
  sexp_stream_ctx(cookie) = ctx;
  sexp_stream_buf(cookie) = sexp_make_string(ctx, size, SEXP_VOID);
  sexp_stream_size(cookie) = size;
  sexp_stream_pos(cookie) = sexp_make_integer(0);
  out = funopen(cookie, NULL, &sstream_write, &sstream_seek, NULL);
  res = sexp_make_output_port(ctx, out, SEXP_FALSE);
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
  sexp res = sexp_make_input_port(ctx, in, SEXP_FALSE);
  sexp_port_cookie(res) = str;  /* for gc preservation */
  return res;
}

sexp sexp_make_output_string_port (sexp ctx) {
  sexp res = sexp_make_output_port(ctx, NULL, SEXP_FALSE);
  sexp_port_stream(res)
    = open_memstream(&sexp_port_buf(res), &sexp_port_size(res));
  return res;
}

sexp sexp_get_output_string (sexp ctx, sexp port) {
  fflush(sexp_port_stream(port));
  return sexp_c_string(ctx, sexp_port_buf(port), sexp_port_size(port));
}

#endif

#else

#define SEXP_PORT_BUFFER_SIZE 4096

int sexp_buffered_read_char (sexp ctx, sexp p) {
  if (sexp_port_offset(p) < sexp_port_size(p)) {
    return sexp_port_buf(p)[sexp_port_offset(p)++];
  } else if (! sexp_port_stream(p)) {
    return EOF;
  } else {
    sexp_port_size(p)
      = fread(sexp_port_buf(p), 1, SEXP_PORT_BUFFER_SIZE, sexp_port_stream(p));
    sexp_port_offset(p) = 0;
    return ((sexp_port_offset(p) < sexp_port_size(p))
            ? sexp_port_buf(p)[sexp_port_offset(p)++] : EOF);
  }
}

sexp sexp_buffered_write_char (sexp ctx, int c, sexp p) {
  if (sexp_port_offset(p) >= sexp_port_size(p))
    sexp_buffered_flush(ctx, p);
  sexp_port_buf(p)[sexp_port_offset(p)++] = c;
  return SEXP_VOID;
}

sexp sexp_buffered_write_string_n (sexp ctx, char *str, sexp_uint_t len, sexp p) {
  if (sexp_port_offset(p) >= sexp_port_size(p))
    sexp_buffered_flush(ctx, p);
  memcpy(sexp_port_buf(p)+sexp_port_offset(p), str, len);
  sexp_port_offset(p) += len;
  return SEXP_VOID;
}

sexp sexp_buffered_write_string (sexp ctx, char *str, sexp p) {
  return sexp_buffered_write_string_n(ctx, str, strlen(str), p);
}

sexp sexp_buffered_flush (sexp ctx, sexp p) {
  sexp_gc_var(ctx, tmp, s_tmp);
  if (! sexp_oportp(p))
    return sexp_type_exception(ctx, "not an output-port", p);
  else if (! sexp_port_openp(p))
    return sexp_user_exception(ctx, SEXP_FALSE, "port is closed", p);
  else {
    if (sexp_port_stream(p)) {
      fwrite(sexp_port_buf(p), 1, sexp_port_offset(p), sexp_port_stream(p));
      fflush(sexp_port_stream(p));
    } else if (sexp_port_offset(p) > 0) {
      sexp_gc_preserve(ctx, tmp, s_tmp);
      tmp = sexp_c_string(ctx, sexp_port_buf(p), sexp_port_offset(p));
      sexp_push(ctx, sexp_port_cookie(p), tmp);
      sexp_gc_release(ctx, tmp, s_tmp);
    }
    sexp_port_offset(p) = 0;
    return SEXP_VOID;
  }
}

sexp sexp_make_input_string_port (sexp ctx, sexp str) {
  sexp res = sexp_make_input_port(ctx, NULL, SEXP_FALSE);
  sexp_port_cookie(res) = str;
  sexp_port_buf(res) = sexp_string_data(str);
  sexp_port_offset(res) = 0;
  sexp_port_size(res) = sexp_string_length(str);
  return res;
}

sexp sexp_make_output_string_port (sexp ctx) {
  sexp res = sexp_make_output_port(ctx, NULL, SEXP_FALSE);
  sexp_port_buf(res) = (char*) malloc(SEXP_PORT_BUFFER_SIZE);
  sexp_port_size(res) = SEXP_PORT_BUFFER_SIZE;
  sexp_port_offset(res) = 0;
  sexp_port_cookie(res) = SEXP_NULL;
  return res;
}

sexp sexp_get_output_string (sexp ctx, sexp out) {
  sexp res;
  sexp_gc_var(ctx, ls, s_ls);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, ls, s_ls);
  sexp_gc_preserve(ctx, tmp, s_tmp);
  if (sexp_port_offset(out) > 0) {
    tmp = sexp_c_string(ctx, sexp_port_buf(out), sexp_port_offset(out));
    ls = sexp_cons(ctx, tmp, sexp_port_cookie(out));
  } else {
    ls = sexp_port_cookie(out);
  }
  res = sexp_string_concatenate(ctx, ls);
  sexp_gc_release(ctx, ls, s_ls);
  return res;
}

#endif

sexp sexp_make_input_port (sexp ctx, FILE* in, sexp name) {
  sexp p = sexp_alloc_type(ctx, port, SEXP_IPORT);
  sexp_port_stream(p) = in;
  sexp_port_name(p) = name;
  sexp_port_line(p) = 1;
  sexp_port_buf(p) = NULL;
  sexp_port_openp(p) = 1;
  sexp_port_sourcep(p) = 1;
  sexp_port_cookie(p) = SEXP_VOID;
  return p;
}

sexp sexp_make_output_port (sexp ctx, FILE* out, sexp name) {
  sexp p = sexp_make_input_port(ctx, out, name);
  sexp_pointer_tag(p) = SEXP_OPORT;
  return p;
}

void sexp_write (sexp ctx, sexp obj, sexp out) {
  unsigned long len, c, res;
  long i=0;
  double f;
  sexp x, *elts;
  char *str=NULL, numbuf[20];

  if (! obj) {
    sexp_write_string(ctx, "#<null>", out); /* shouldn't happen */
  } else if (sexp_pointerp(obj)) {
    switch (sexp_pointer_tag(obj)) {
    case SEXP_PAIR:
      sexp_write_char(ctx, '(', out);
      sexp_write(ctx, sexp_car(obj), out);
      for (x=sexp_cdr(obj); sexp_pairp(x); x=sexp_cdr(x)) {
        sexp_write_char(ctx, ' ', out);
        sexp_write(ctx, sexp_car(x), out);
      }
      if (! sexp_nullp(x)) {
        sexp_write_string(ctx, " . ", out);
        sexp_write(ctx, x, out);
      }
      sexp_write_char(ctx, ')', out);
      break;
    case SEXP_VECTOR:
      len = sexp_vector_length(obj);
      elts = sexp_vector_data(obj);
      if (len == 0) {
        sexp_write_string(ctx, "#()", out);
      } else {
        sexp_write_string(ctx, "#(", out);
        sexp_write(ctx, elts[0], out);
        for (i=1; i<len; i++) {
          sexp_write_char(ctx, ' ', out);
          sexp_write(ctx, elts[i], out);
        }
        sexp_write_char(ctx, ')', out);
      }
      break;
#if ! USE_IMMEDIATE_FLONUMS
    case SEXP_FLONUM:
      f = sexp_flonum_value(obj);
      i = sprintf(numbuf, "%.15g", f);
      if (f == trunc(f)) {
        numbuf[i++] = '.'; numbuf[i++] = '0'; numbuf[i++] = '\0';
      }
      sexp_write_string(ctx, numbuf, out);
      break;
#endif
    case SEXP_PROCEDURE:
      sexp_write_string(ctx, "#<procedure: ", out);
      sexp_write(ctx, sexp_bytecode_name(sexp_procedure_code(obj)), out);
      sexp_write_string(ctx, ">", out);
      break;
    case SEXP_STRING:
      sexp_write_char(ctx, '"', out);
      i = sexp_string_length(obj);
      str = sexp_string_data(obj);
      for ( ; i>0; str++, i--) {
        switch (str[0]) {
        case '\\': sexp_write_string(ctx, "\\\\", out); break;
        case '"': sexp_write_string(ctx, "\\\"", out); break;
        case '\n': sexp_write_string(ctx, "\\n", out); break;
        case '\r': sexp_write_string(ctx, "\\r", out); break;
        case '\t': sexp_write_string(ctx, "\\t", out); break;
        default: sexp_write_char(ctx, str[0], out);
        }
      }
      sexp_write_char(ctx, '"', out);
      break;
    case SEXP_SYMBOL:
      i = sexp_string_length(sexp_symbol_string(obj));
      str = sexp_string_data(sexp_symbol_string(obj));
      for ( ; i>0; str++, i--) {
        if ((str[0] == '\\') || is_separator(str[0]))
          sexp_write_char(ctx, '\\', out);
        sexp_write_char(ctx, str[0], out);
      }
      break;
    default:
      i = sexp_pointer_tag(obj);
      sexp_write_string(ctx, "#<", out);
      sexp_write_string(ctx,
                        (i < SEXP_NUM_TYPES)
                        ? sexp_type_name(&(sexp_type_specs[i])) : "invalid",
                        out);
      sexp_write_char(ctx, '>', out);
      break;
    }
  } else if (sexp_integerp(obj)) {
    sprintf(numbuf, "%ld", sexp_unbox_integer(obj));
    sexp_write_string(ctx, numbuf, out);
#if USE_IMMEDIATE_FLONUMS
  } else if (sexp_flonump(obj)) {
    f = sexp_flonum_value(obj);
    i = sprintf(numbuf, "%.15g", f);
    if (f == trunc(f)) {
      numbuf[i++] = '.'; numbuf[i++] = '0'; numbuf[i++] = '\0';
    }
    sexp_write_string(ctx, numbuf, out);
#endif
  } else if (sexp_charp(obj)) {
    if (obj == sexp_make_character(' '))
      sexp_write_string(ctx, "#\\space", out);
    else if (obj == sexp_make_character('\n'))
      sexp_write_string(ctx, "#\\newline", out);
    else if (obj == sexp_make_character('\r'))
      sexp_write_string(ctx, "#\\return", out);
    else if (obj == sexp_make_character('\t'))
      sexp_write_string(ctx, "#\\tab", out);
    else if ((33 <= sexp_unbox_character(obj))
             && (sexp_unbox_character(obj) < 127)) {
      sexp_write_string(ctx, "#\\", out);
      sexp_write_char(ctx, sexp_unbox_character(obj), out);
    } else {
      sexp_write_string(ctx, "#\\x", out);
      sexp_write_char(ctx, hex_digit(sexp_unbox_character(obj)>>4), out);
      sexp_write_char(ctx, hex_digit(sexp_unbox_character(obj)&0xF), out);
    }
  } else if (sexp_symbolp(obj)) {

#if USE_HUFF_SYMS
    if (((sexp_uint_t)obj&7)==7) {
      c = ((sexp_uint_t)obj)>>3;
      while (c) {
#include "opt/sexp-unhuff.c"
        sexp_write_char(ctx, res, out);
      }
    }
#endif

  } else {
    switch ((sexp_uint_t) obj) {
    case (sexp_uint_t) SEXP_NULL:
      sexp_write_string(ctx, "()", out); break;
    case (sexp_uint_t) SEXP_TRUE:
      sexp_write_string(ctx, "#t", out); break;
    case (sexp_uint_t) SEXP_FALSE:
      sexp_write_string(ctx, "#f", out); break;
    case (sexp_uint_t) SEXP_EOF:
      sexp_write_string(ctx, "#<eof>", out); break;
    case (sexp_uint_t) SEXP_UNDEF:
    case (sexp_uint_t) SEXP_VOID:
      sexp_write_string(ctx, "#<undef>", out); break;
    default:
      sexp_write_string(ctx, "#<invalid immediate>", out);
    }
  }
}

#define INIT_STRING_BUFFER_SIZE 128

sexp sexp_read_string(sexp ctx, sexp in) {
  int c, i=0, size=INIT_STRING_BUFFER_SIZE;
  char initbuf[INIT_STRING_BUFFER_SIZE];
  char *buf=initbuf, *tmp;
  sexp res;

  for (c = sexp_read_char(ctx, in); c != '"'; c = sexp_read_char(ctx, in)) {
    if (c == '\\') {
      c = sexp_read_char(ctx, in);
      switch (c) {case 'n': c = '\n'; break; case 't': c = '\t'; break;}
    }
    if (c == EOF) {
      res = sexp_read_error(ctx, "premature end of string", SEXP_NULL, in);
      break;
    }
    buf[i++] = c;
    if (i >= size) {       /* expand buffer w/ malloc(), later free() it */
      tmp = (char*) malloc(size*2);
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

  for (c = sexp_read_char(ctx, in); c != '"'; c = sexp_read_char(ctx, in)) {
    if (c == '\\') c = sexp_read_char(ctx, in);
    if (c == EOF || is_separator(c)) {
      sexp_push_char(ctx, c, in);
      break;
    }
    buf[i++] = c;
    if (i >= size) {       /* expand buffer w/ malloc(), later free() it */
      tmp = (char*) malloc(size*2);
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
  for (c=sexp_read_char(ctx, in);
       isdigit(c);
       c=sexp_read_char(ctx, in), scale*=0.1)
    res += digit_value(c)*scale;
  sexp_push_char(ctx, c, in);
  if (c=='e' || c=='E') {
    exponent = sexp_read_number(ctx, in, 10);
    if (sexp_exceptionp(exponent)) return exponent;
    e = (sexp_integerp(exponent) ? sexp_unbox_integer(exponent)
         : sexp_flonump(exponent) ? sexp_flonum_value(exponent) : 0.0);
  } else if ((c!=EOF) && ! is_separator(c))
    return sexp_read_error(ctx, "invalid numeric syntax",
                           sexp_make_character(c), in);
  return sexp_make_flonum(ctx, (whole + res) * pow(10, e));
}

sexp sexp_read_number(sexp ctx, sexp in, int base) {
  sexp f, den;
  sexp_uint_t res = 0, negativep = 0;
  int c;

  c = sexp_read_char(ctx, in);
  if (c == '-')
    negativep = 1;
  else if (isdigit(c))
    res = digit_value(c);

  if (base == 16)
    for (c=sexp_read_char(ctx, in); isxdigit(c); c=sexp_read_char(ctx, in))
      res = res * base + digit_value(c);
  else
    for (c=sexp_read_char(ctx, in); isdigit(c); c=sexp_read_char(ctx, in))
      res = res * base + digit_value(c);

  if (c=='.' || c=='e' || c=='E') {
    if (base != 10)
      return
        sexp_read_error(ctx, "decimal found in non-base 10", SEXP_NULL, in);
    if (c!='.')
      sexp_push_char(ctx, c, in);
    f = sexp_read_float_tail(ctx, in, res);
    if (! sexp_flonump(f)) return f;
    if ((c!='.') && (sexp_flonum_value(f) == round(sexp_flonum_value(f)))) {
      res = (sexp_sint_t) sexp_flonum_value(f);
    } else {
      if (negativep)
#if USE_IMMEDIATE_FLONUMS
        f = sexp_make_flonum(ctx, -sexp_flonum_value(f));
#else
        sexp_flonum_value(f) = -sexp_flonum_value(f);
#endif
      return f;
    }
  } else if (c=='/') {
    den = sexp_read_number(ctx, in, base);
    if (! sexp_integerp(den))
      return (sexp_exceptionp(den)
              ? den : sexp_read_error(ctx, "invalid rational syntax", den, in));
    return sexp_make_flonum(ctx, (double)(negativep ? -res : res)
                            / (double)sexp_unbox_integer(den));
  } else {
    if ((c!=EOF) && ! is_separator(c))
      return sexp_read_error(ctx, "invalid numeric syntax",
                             sexp_make_character(c), in);
    sexp_push_char(ctx, c, in);
  }

  return sexp_make_integer(negativep ? -res : res);
}

sexp sexp_read_raw (sexp ctx, sexp in) {
  char *str;
  int c1, c2, line;
  sexp tmp2;
  sexp_gc_var(ctx, res, s_res);
  sexp_gc_var(ctx, tmp, s_tmp);
  sexp_gc_preserve(ctx, res, s_res);
  sexp_gc_preserve(ctx, tmp, s_tmp);

 scan_loop:
  switch (c1 = sexp_read_char(ctx, in)) {
  case EOF:
    res = SEXP_EOF;
    break;
  case ';':
    while ((c1 = sexp_read_char(ctx, in)) != EOF)
      if (c1 == '\n')
        break;
    /* ... FALLTHROUGH ... */
  case '\n':
    sexp_port_line(in)++;
    goto scan_loop;
  case ' ':
  case '\t':
  case '\r':
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
    if ((c1 = sexp_read_char(ctx, in)) == '@') {
      res = sexp_read(ctx, in);
      res = sexp_list2(ctx, the_unquote_splicing_symbol, res);
    } else {
      sexp_push_char(ctx, c1, in);
      res = sexp_read(ctx, in);
      res = sexp_list2(ctx, the_unquote_symbol, res);
    }
    break;
  case '"':
    res = sexp_read_string(ctx, in);
    break;
  case '(':
    line = (sexp_port_sourcep(in) ? sexp_port_line(in) : -1);
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
    if ((line >= 0) && sexp_pairp(res)) {
      sexp_pair_source(res)
        = sexp_cons(ctx, sexp_port_name(in), sexp_make_integer(line));
    }
    if (sexp_port_sourcep(in))
      for (tmp=res; sexp_pairp(tmp); tmp=sexp_cdr(tmp))
        sexp_immutablep(tmp) = 1;
    break;
  case '#':
    switch (c1=sexp_read_char(ctx, in)) {
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
      c2 = sexp_read_char(ctx, in);
      if (c2 == EOF || is_separator(c2)) {
        res = (c1 == 't' ? SEXP_TRUE : SEXP_FALSE);
        sexp_push_char(ctx, c2, in);
      } else {
        tmp = sexp_list2(ctx, sexp_make_character(c1), sexp_make_character(c2));
        res = sexp_read_error(ctx, "invalid syntax #%c%c", tmp, in);
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
      c1 = sexp_read_char(ctx, in);
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
                   isxdigit(str[1]) && isxdigit(str[2]) && str[3] == '\0') {
          res = sexp_make_character(16 * digit_value(str[1])
                                    + digit_value(str[2]));
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
            tmp = sexp_c_string(ctx, str, -1);
            res = sexp_read_error(ctx, "unknown character name", tmp, in);
          }
        }
      }
      break;
    case '(':
      sexp_push_char(ctx, c1, in);
      res = sexp_read(ctx, in);
      if (sexp_not(sexp_listp(ctx, res))) {
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
                            sexp_make_character(c1), in);
    }
    break;
  case '.':
    c1 = sexp_read_char(ctx, in);
    if (c1 == EOF || is_separator(c1)) {
      res = SEXP_RAWDOT;
    } else if (isdigit(c1)) {
      sexp_push_char(ctx, c1, in);
      res = sexp_read_float_tail(ctx, in, 0);
    } else {
      sexp_push_char(ctx, c1, in);
      res = sexp_read_symbol(ctx, in, '.', 1);
    }
    break;
  case ')':
    res = SEXP_CLOSE;
    break;
  case '+':
  case '-':
    c2 = sexp_read_char(ctx, in);
    if (c2 == '.' || isdigit(c2)) {
      sexp_push_char(ctx, c2, in);
      res = sexp_read_number(ctx, in, 10);
      if ((c1 == '-') && ! sexp_exceptionp(res)) {
#if USE_FLONUMS
        if (sexp_flonump(res))
#if USE_IMMEDIATE_FLONUMS
          res = sexp_make_flonum(ctx, -1 * sexp_flonum_value(res));
#else
          sexp_flonum_value(res) = -1 * sexp_flonum_value(res);
#endif
        else
#endif
          res = sexp_fx_mul(res, -1);
      }
    } else {
      sexp_push_char(ctx, c2, in);
      res = sexp_read_symbol(ctx, in, c1, 1);
    }
    break;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    sexp_push_char(ctx, c1, in);
    res = sexp_read_number(ctx, in, 10);
    break;
  default:
    res = sexp_read_symbol(ctx, in, c1, 1);
    break;
  }

  if (sexp_port_sourcep(in) && sexp_pointerp(res))
    sexp_immutablep(res) = 1;
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

