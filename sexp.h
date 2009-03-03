/*  sexp.h -- header for sexp library */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt */

#ifndef SEXP_H
#define SEXP_H

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "config.h"

#ifdef HAVE_ERR_H
#include <err.h>
#else
/* requires that msg be a string literal */
#define errx(code, msg, ...) (fprintf(stderr,msg"\n",__VA_ARGS__), exit(code))
#endif

#define sexp_debug(msg, obj) (fprintf(stderr,msg), fflush(stderr), write_sexp(stderr, obj), fprintf(stderr,"\n"))

#ifdef USE_BOEHM
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
  SEXP_FIXNUM,
  SEXP_CHAR,
  SEXP_BOOLEAN,
  SEXP_PAIR,
  SEXP_SYMBOL,
  SEXP_STRING,
  SEXP_VECTOR,
  SEXP_FLONUM,
  SEXP_BIGNUM,
  /* the following are used only by the evaluator */
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
#define SEXP_PROCEDUREP(x) (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_PROCEDURE)
#define SEXP_ENVP(x)      (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_ENV)
#define SEXP_BYTECODEP(x) (SEXP_POINTERP(x) && ((sexp)(x))->tag ==SEXP_BYTECODE)
#define SEXP_COREP(x)     (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_CORE)
#define SEXP_OPCODEP(x)   (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_OPCODE)

#define SEXP_SYMBOLP(x)  (SEXP_ISYMBOLP(x) || SEXP_LSYMBOLP(x))

#ifdef USE_HUFF_SYMS
#define SEXP_DOTP(x)     (((sexp_uint_t)(x))==((0x5D00<<SEXP_IMMEDIATE_BITS)+SEXP_ISYMBOL_TAG))
#else
#define SEXP_DOTP(x)     ((x)==the_dot_symbol)
#endif

#define make_integer(n)    ((sexp) (((long) n<<SEXP_FIXNUM_BITS) + SEXP_FIXNUM_TAG))
#define unbox_integer(n)   ((long) n>>SEXP_FIXNUM_BITS)
#define make_character(n)  ((sexp) (((long) n<<SEXP_EXTENDED_BITS) + SEXP_CHAR_TAG))
#define unbox_character(n) ((long) n>>SEXP_EXTENDED_BITS)

#define flonum_value(f) (((double*)(((sexp_uint_t)f)+sizeof(char)))[0])

#define vector_length(x) ((sexp_uint_t) x->data1)
#define vector_data(x)   ((sexp*) x->data2)

#define vector_ref(x, i) (vector_data(x)[unbox_integer(i)])
#define vector_set(x, i, v) (vector_data(x)[unbox_integer(i)] = (v))

#define procedure_code(x) ((bytecode) ((sexp)x)->data1)
#define procedure_vars(x)   ((sexp) ((sexp)x)->data2)

#define string_length(x) ((sexp_uint_t) x->data1)
#define string_data(x)   ((char*) x->data2)

#define string_ref(x, i) (make_character(string_data(x)[unbox_integer(i)]))
#define string_set(x, i, v) (string_data(x)[unbox_integer(i)] = unbox_character(v))

#define symbol_pointer(x) ((sexp) (((sexp_uint_t)x)-SEXP_LSYMBOL_TAG))
#define symbol_length(x)  ((sexp_uint_t) (symbol_pointer(x)->data1))
#define symbol_data(x)    ((char*) (symbol_pointer(x)->data2))

#define sexp_add(a, b) ((sexp)(((sexp_sint_t)a)+((sexp_sint_t)b)-SEXP_FIXNUM_TAG))
#define sexp_sub(a, b) ((sexp)(((sexp_sint_t)a)-((sexp_sint_t)b)+SEXP_FIXNUM_TAG))
#define sexp_mul(a, b) ((sexp)((((((sexp_sint_t)a)-SEXP_FIXNUM_TAG)*(((sexp_sint_t)b)>>SEXP_FIXNUM_BITS))+SEXP_FIXNUM_TAG)))
#define sexp_div(a, b) ((sexp)(((((sexp_sint_t)a)>>SEXP_FIXNUM_BITS)/(((sexp_sint_t)b)>>SEXP_FIXNUM_BITS))<<SEXP_FIXNUM_BITS)+SEXP_FIXNUM_TAG)
#define sexp_mod(a, b) ((sexp)(((((sexp_sint_t)a)>>SEXP_FIXNUM_BITS)%(((sexp_sint_t)b)>>SEXP_FIXNUM_BITS))<<SEXP_FIXNUM_BITS)+SEXP_FIXNUM_TAG)

#define list2(a, b)       cons(a, cons(b, SEXP_NULL))
#define list3(a, b, c)    cons(a, cons(b, cons(c, SEXP_NULL)))
#define list4(a, b, c, d) cons(a, cons(b, cons(c, cons(d, SEXP_NULL))))

#define SEXP_CAR(x)       (((sexp)x)->data1)
#define SEXP_CDR(x)       (((sexp)x)->data2)

#define SEXP_CAAR(x)      (SEXP_CAR(SEXP_CAR(x)))
#define SEXP_CADR(x)      (SEXP_CAR(SEXP_CDR(x)))
#define SEXP_CDAR(x)      (SEXP_CDR(SEXP_CAR(x)))
#define SEXP_CDDR(x)      (SEXP_CDR(SEXP_CDR(x)))

#define SEXP_CADDR(x)     (SEXP_CAR(SEXP_CDDR(x)))
#define SEXP_CDDDR(x)     (SEXP_CDR(SEXP_CDDR(x)))
#define SEXP_CADDDR(x)    (SEXP_CADR(SEXP_CDDR(x)))
#define SEXP_CDDDDR(x)    (SEXP_CDDR(SEXP_CDDR(x)))

sexp cons(sexp head, sexp tail);
sexp car(sexp obj);
sexp cdr(sexp obj);
sexp set_car(sexp obj, sexp val);
sexp set_cdr(sexp obj, sexp val);

int listp(sexp obj);
int list_index(sexp ls, sexp elt);
sexp lset_diff(sexp a, sexp b);
sexp reverse(sexp ls);
sexp nreverse(sexp ls);
sexp append(sexp a, sexp b);
sexp list(int count, ...);
sexp memq(sexp x, sexp ls);
sexp assq (sexp x, sexp ls);
unsigned long length(sexp ls);
sexp make_string(char *str);
sexp make_flonum(double f);
int string_hash(char *str, int acc);
sexp intern(char *str);
sexp make_vector(unsigned long len, sexp dflt);
sexp list_to_vector(sexp ls);
sexp vector(int count, ...);
void write_sexp(FILE *out, sexp obj);
void free_sexp(sexp obj);
char* read_string(FILE *in);
char* read_symbol(FILE *in, int init);
sexp read_number(FILE *in, int base);
sexp read_sexp_raw(FILE *in);
sexp read_sexp(FILE *in);
void sexp_init();

#endif /* ! SEXP_H */

