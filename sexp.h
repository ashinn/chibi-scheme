/*  sexp.h -- header for sexp library                    */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifndef SEXP_H
#define SEXP_H

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "config.h"

#if HAVE_ERR_H
#include <err.h>
#else
/* requires that msg be a string literal */
#define errx(code, msg, ...) (fprintf(stderr,msg"\n",__VA_ARGS__), exit(code))
#endif

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__) || defined(__OpenBSD__)
#define SEXP_BSD 1
#else
#define SEXP_BSD 0
#endif

#if USE_BOEHM
#include "gc/include/gc.h"
#define SEXP_ALLOC        GC_malloc
#define SEXP_ALLOC_ATOMIC GC_malloc_atomic
#define SEXP_REALLOC      GC_realloc
#define SEXP_FREE         GC_free
#else
#define SEXP_ALLOC        malloc
#define SEXP_ALLOC_ATOMIC SEXP_ALLOC
#define SEXP_REALLOC      realloc
#define SEXP_FREE         free
#endif

#define SEXP_NEW()       ((sexp) SEXP_ALLOC(sizeof(struct sexp_struct)))

/* tagging system
 *   bits end in  00:  pointer
 *                01:  fixnum
 *               011:  symbol
 *               111:  immediate symbol
 *              0110:  char
 *              1110:  other immediate object (NULL, TRUE, FALSE)
 */

#define SEXP_FIXNUM_BITS 2
#define SEXP_IMMEDIATE_BITS 3
#define SEXP_EXTENDED_BITS 4

#define SEXP_FIXNUM_MASK 3
#define SEXP_IMMEDIATE_MASK 7
#define SEXP_EXTENDED_MASK 15

#define SEXP_POINTER_TAG 0
#define SEXP_FIXNUM_TAG 1
#define SEXP_LSYMBOL_TAG 3
#define SEXP_ISYMBOL_TAG 7
#define SEXP_CHAR_TAG 6

enum sexp_types {
  SEXP_OBJECT,
  SEXP_FIXNUM,
  SEXP_CHAR,
  SEXP_BOOLEAN,
  SEXP_PAIR,
  SEXP_SYMBOL,
  SEXP_STRING,
  SEXP_VECTOR,
  SEXP_FLONUM,
  SEXP_BIGNUM,
  SEXP_IPORT,
  SEXP_OPORT,
  /* the following are used only by the evaluator */
  SEXP_EXCEPTION,
  SEXP_PROCEDURE,
  SEXP_ENV,
  SEXP_BYTECODE,
  SEXP_CORE,
  SEXP_OPCODE,
};

typedef struct sexp_struct {
  char tag;
  void *data1;
  void *data2;
} *sexp;

typedef unsigned long sexp_uint_t;
typedef long sexp_sint_t;

#define MAKE_IMMEDIATE(n)  ((sexp) ((n<<4) + 14))
#define SEXP_NULL   MAKE_IMMEDIATE(0)
#define SEXP_FALSE  MAKE_IMMEDIATE(1)
#define SEXP_TRUE   MAKE_IMMEDIATE(2)
#define SEXP_EOF    MAKE_IMMEDIATE(3)
#define SEXP_UNDEF  MAKE_IMMEDIATE(4)
#define SEXP_ERROR  MAKE_IMMEDIATE(5)
#define SEXP_CLOSE  MAKE_IMMEDIATE(6) /* internal use */
#define SEXP_RAWDOT MAKE_IMMEDIATE(7) /* internal use */

#define SEXP_NULLP(x)    ((x) == SEXP_NULL)
#define SEXP_POINTERP(x) (((sexp_uint_t)(x) & SEXP_FIXNUM_MASK) == SEXP_POINTER_TAG)
#define SEXP_INTEGERP(x) (((sexp_uint_t)(x) & SEXP_FIXNUM_MASK) == SEXP_FIXNUM_TAG)
#define SEXP_ISYMBOLP(x) (((sexp_uint_t)(x) & SEXP_IMMEDIATE_MASK) == SEXP_ISYMBOL_TAG)
#define SEXP_CHARP(x)    (((sexp_uint_t)(x) & SEXP_EXTENDED_MASK) == SEXP_CHAR_TAG)
#define SEXP_BOOLEANP(x) (((x) == SEXP_TRUE) || ((x) == SEXP_FALSE))

#define SEXP_PAIRP(x)     (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_PAIR)
#define SEXP_STRINGP(x)   (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_STRING)
#define SEXP_LSYMBOLP(x)  (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_SYMBOL)
#define SEXP_VECTORP(x)   (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_VECTOR)
#define SEXP_FLONUMP(x)   (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_FLONUM)
#define SEXP_IPORTP(x)    (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_IPORT)
#define SEXP_OPORTP(x)    (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_OPORT)
#define SEXP_PROCEDUREP(x) (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_PROCEDURE)
#define SEXP_ENVP(x)      (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_ENV)
#define SEXP_BYTECODEP(x) (SEXP_POINTERP(x) && ((sexp)(x))->tag ==SEXP_BYTECODE)
#define SEXP_COREP(x)     (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_CORE)
#define SEXP_OPCODEP(x)   (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_OPCODE)

#define SEXP_SYMBOLP(x)  (SEXP_ISYMBOLP(x) || SEXP_LSYMBOLP(x))

#if USE_HUFF_SYMS
#define SEXP_DOTP(x)     (((sexp_uint_t)(x))==((0x5D00<<SEXP_IMMEDIATE_BITS)+SEXP_ISYMBOL_TAG))
#else
#define SEXP_DOTP(x)     ((x)==sexp_the_dot_symbol)
#endif

#define sexp_make_integer(n)    ((sexp) (((long) n<<SEXP_FIXNUM_BITS) + SEXP_FIXNUM_TAG))
#define sexp_unbox_integer(n)   ((long) n>>SEXP_FIXNUM_BITS)
#define sexp_make_character(n)  ((sexp) (((long) n<<SEXP_EXTENDED_BITS) + SEXP_CHAR_TAG))
#define sexp_unbox_character(n) ((long) n>>SEXP_EXTENDED_BITS)

#define sexp_flonum_value(f) (((double*)(((sexp_uint_t)f)+sizeof(char)))[0])

#define sexp_vector_length(x) ((sexp_uint_t) x->data1)
#define sexp_vector_data(x)   ((sexp*) (((sexp)x)->data2))

#define sexp_vector_ref(x, i) (sexp_vector_data(x)[sexp_unbox_integer(i)])
#define sexp_vector_set(x, i, v) (sexp_vector_data(x)[sexp_unbox_integer(i)] = (v))

#define sexp_procedure_num_args(x) (((procedure)x)->num_args)
#define sexp_procedure_variadic_p(x) (sexp_unbox_integer(((procedure)x)->flags) & 1)
#define sexp_procedure_code(x) ((bytecode) ((procedure)x)->bc)
#define sexp_procedure_vars(x)   ((sexp) ((procedure)x)->vars)

#define sexp_string_length(x) ((sexp_uint_t) x->data1)
#define sexp_string_data(x)   ((char*) x->data2)

#define sexp_string_ref(x, i) (sexp_make_character(sexp_string_data(x)[sexp_unbox_integer(i)]))
#define sexp_string_set(x, i, v) (sexp_string_data(x)[sexp_unbox_integer(i)] = sexp_unbox_character(v))

#define sexp_port_stream(p)  ((FILE*) ((sexp)p)->data1)

#if USE_STRING_STREAMS
#if SEXP_BSD
#define fmemopen(str, len, m) funopen(sexp_vector(3, (sexp)str, (sexp)len, (sexp)0), sstream_read, sstream_write, sstream_seek, sstream_close)
int sstream_read(void *vec, char *dst, int n);
int sstream_write(void *vec, const char *src, int n);
off_t sstream_seek(void *vec, off_t offset, int whence);
int sstream_close(void *vec);
#endif
#define sexp_read_char(p) (getc(sexp_port_stream(p)))
#define sexp_push_char(c, p) (ungetc(c, sexp_port_stream(p)))
#define sexp_write_char(c, p) (putc(c, sexp_port_stream(p)))
#define sexp_write_string(s, p) (fputs(s, sexp_port_stream(p)))
#define sexp_printf(p, s, ...) (fprintf(sexp_port_stream(p), s, __VA_ARGS__))
#define sexp_flush(p) (fflush(sexp_port_stream(p)))
#else
sexp sexp_read_char(sexp port);
void sexp_push_char(sexp ch, sexp port);
void sexp_write_char(sexp ch, sexp port);
void sexp_write_string(sexp str, sexp port);
void sexp_printf(sexp port, sexp fmt, ...);
#endif

#define sexp_symbol_pointer(x) (x)
#define sexp_symbol_length(x)  ((sexp_uint_t) (sexp_symbol_pointer(x)->data1))
#define sexp_symbol_data(x)    ((char*) (sexp_symbol_pointer(x)->data2))

#define sexp_add(a, b) ((sexp)(((sexp_sint_t)a)+((sexp_sint_t)b)-SEXP_FIXNUM_TAG))
#define sexp_sub(a, b) ((sexp)(((sexp_sint_t)a)-((sexp_sint_t)b)+SEXP_FIXNUM_TAG))
#define sexp_mul(a, b) ((sexp)((((((sexp_sint_t)a)-SEXP_FIXNUM_TAG)*(((sexp_sint_t)b)>>SEXP_FIXNUM_BITS))+SEXP_FIXNUM_TAG)))
#define sexp_div(a, b) ((sexp)(((((sexp_sint_t)a)>>SEXP_FIXNUM_BITS)/(((sexp_sint_t)b)>>SEXP_FIXNUM_BITS))<<SEXP_FIXNUM_BITS)+SEXP_FIXNUM_TAG)
#define sexp_mod(a, b) ((sexp)(((((sexp_sint_t)a)>>SEXP_FIXNUM_BITS)%(((sexp_sint_t)b)>>SEXP_FIXNUM_BITS))<<SEXP_FIXNUM_BITS)+SEXP_FIXNUM_TAG)

#define sexp_list1(a)          sexp_cons(a, SEXP_NULL)
#define sexp_list2(a, b)       sexp_cons(a, sexp_cons(b, SEXP_NULL))
#define sexp_list3(a, b, c)    sexp_cons(a, sexp_cons(b, sexp_cons(c, SEXP_NULL)))
#define sexp_list4(a, b, c, d) sexp_cons(a, sexp_cons(b, sexp_cons(c, sexp_cons(d, SEXP_NULL))))

#define SEXP_CAR(x)       (((sexp)x)->data1)
#define SEXP_CDR(x)       (((sexp)x)->data2)

#define SEXP_CAAR(x)      (SEXP_CAR(SEXP_CAR(x)))
#define SEXP_CADR(x)      (SEXP_CAR(SEXP_CDR(x)))
#define SEXP_CDAR(x)      (SEXP_CDR(SEXP_CAR(x)))
#define SEXP_CDDR(x)      (SEXP_CDR(SEXP_CDR(x)))

#define SEXP_CAAAR(x)     (SEXP_CAR(SEXP_CAAR(x)))
#define SEXP_CAADR(x)     (SEXP_CAR(SEXP_CADR(x)))
#define SEXP_CADAR(x)     (SEXP_CAR(SEXP_CDAR(x)))
#define SEXP_CADDR(x)     (SEXP_CAR(SEXP_CDDR(x)))
#define SEXP_CDAAR(x)     (SEXP_CDR(SEXP_CAAR(x)))
#define SEXP_CDADR(x)     (SEXP_CDR(SEXP_CADR(x)))
#define SEXP_CDDAR(x)     (SEXP_CDR(SEXP_CDAR(x)))
#define SEXP_CDDDR(x)     (SEXP_CDR(SEXP_CDDR(x)))

#define SEXP_CADDDR(x)    (SEXP_CADR(SEXP_CDDR(x)))
#define SEXP_CDDDDR(x)    (SEXP_CDDR(SEXP_CDDR(x)))

sexp sexp_cons(sexp head, sexp tail);
int sexp_listp(sexp obj);
int sexp_list_index(sexp ls, sexp elt);
sexp sexp_lset_diff(sexp a, sexp b);
sexp sexp_reverse(sexp ls);
sexp sexp_nreverse(sexp ls);
sexp sexp_append(sexp a, sexp b);
sexp sexp_list(int count, ...);
sexp sexp_memq(sexp x, sexp ls);
sexp sexp_assq(sexp x, sexp ls);
unsigned long sexp_length(sexp ls);
sexp sexp_make_string(char *str);
sexp sexp_make_flonum(double f);
int sexp_string_hash(char *str, int acc);
sexp sexp_intern(char *str);
sexp sexp_make_vector(unsigned int len, sexp dflt);
sexp sexp_list_to_vector(sexp ls);
sexp sexp_vector(int count, ...);
void sexp_write(sexp obj, sexp out);
void sexp_free(sexp obj);
char* sexp_read_string(sexp in);
char* sexp_read_symbol(sexp in, int init);
sexp sexp_read_number(sexp in, int base);
sexp sexp_read_raw(sexp in);
sexp sexp_read(sexp in);
sexp sexp_read_from_string(char *str);
sexp sexp_make_input_port(FILE* in);
sexp sexp_make_output_port(FILE* out);
sexp sexp_make_input_string_port(sexp str);
sexp sexp_make_output_string_port();
sexp sexp_get_output_string(sexp port);
void sexp_init();

#endif /* ! SEXP_H */

