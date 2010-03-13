/*  sexp.h -- header for sexp library                    */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifndef SEXP_H
#define SEXP_H

#ifdef __cplusplus
extern "C" {
#endif

#define SEXP_MODULE_PATH_VAR "CHIBI_MODULE_PATH"

#include "chibi/features.h"
#include "chibi/install.h"

#include <ctype.h>
#include <stdio.h>

#if SEXP_USE_DL
#ifndef __MINGW32__
#include <dlfcn.h>
#endif
#endif

#ifdef PLAN9
#include <u.h>
#include <libc.h>
#include <fcall.h>
#include <thread.h>
#include <9p.h>
typedef unsigned long size_t;
#else
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>
#endif

/* tagging system
 *   bits end in  00:  pointer
 *                01:  fixnum
 *               011:  immediate flonum (optional)
 *               111:  immediate symbol (optional)
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
#define SEXP_ISYMBOL_TAG 7
#define SEXP_IFLONUM_TAG 3
#define SEXP_CHAR_TAG 6
#define SEXP_EXTENDED_TAG 14

#if SEXP_USE_HASH_SYMS
#define SEXP_SYMBOL_TABLE_SIZE 389
#else
#define SEXP_SYMBOL_TABLE_SIZE 1
#endif

enum sexp_types {
  SEXP_OBJECT,
  SEXP_TYPE,
  SEXP_FIXNUM,
  SEXP_CHAR,
  SEXP_BOOLEAN,
  SEXP_PAIR,
  SEXP_SYMBOL,
  SEXP_STRING,
  SEXP_VECTOR,
  SEXP_FLONUM,
  SEXP_BIGNUM,
  SEXP_CPOINTER,
  SEXP_IPORT,
  SEXP_OPORT,
  SEXP_EXCEPTION,
  SEXP_PROCEDURE,
  SEXP_MACRO,
  SEXP_SYNCLO,
  SEXP_ENV,
  SEXP_BYTECODE,
  SEXP_CORE,
  SEXP_OPCODE,
  SEXP_LAMBDA,
  SEXP_CND,
  SEXP_REF,
  SEXP_SET,
  SEXP_SEQ,
  SEXP_LIT,
  SEXP_STACK,
  SEXP_CONTEXT,
  SEXP_NUM_CORE_TYPES
};

typedef unsigned long sexp_uint_t;
typedef long sexp_sint_t;
#if SEXP_64_BIT
typedef unsigned int sexp_tag_t;
#else
typedef unsigned short sexp_tag_t;
#endif
typedef struct sexp_struct *sexp;

#define __HALF_MAX_SIGNED(type) ((type)1 << (sizeof(type)*8-2))
#define __MAX_SIGNED(type) (__HALF_MAX_SIGNED(type) - 1 + __HALF_MAX_SIGNED(type))
#define __MIN_SIGNED(type) (-1 - __MAX_SIGNED(type))

#define SEXP_UINT_T_MAX ((sexp_uint_t)-1)
#define SEXP_UINT_T_MIN (0)
#define SEXP_SINT_T_MAX __MAX_SIGNED(sexp_sint_t)
#define SEXP_SINT_T_MIN __MIN_SIGNED(sexp_sint_t)

#define SEXP_MAX_FIXNUM ((((sexp_sint_t)1)<<(sizeof(sexp_sint_t)*8-SEXP_FIXNUM_BITS-1))-1)
#define SEXP_MIN_FIXNUM (-SEXP_MAX_FIXNUM-1)

/* procedure types */
typedef sexp (*sexp_proc0) (void);
typedef sexp (*sexp_proc1) (sexp);
typedef sexp (*sexp_proc2) (sexp, sexp);
typedef sexp (*sexp_proc3) (sexp, sexp, sexp);
typedef sexp (*sexp_proc4) (sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc5) (sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc6) (sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc7) (sexp, sexp, sexp, sexp, sexp, sexp, sexp);

typedef struct sexp_free_list_t *sexp_free_list;
struct sexp_free_list_t {
  sexp_uint_t size;
  sexp_free_list next;
};

typedef struct sexp_heap_t *sexp_heap;
struct sexp_heap_t {
  sexp_uint_t size;
  sexp_free_list free_list;
  sexp_heap next;
  char *data;
};

struct sexp_gc_var_t {
  sexp *var;
  /* char *name; */
  struct sexp_gc_var_t *next;
};

struct sexp_struct {
  sexp_tag_t tag;
  char gc_mark;
  unsigned int immutablep:1;
  unsigned int freep:1;
  union {
    /* basic types */
    double flonum;
    struct {
      sexp_tag_t tag;
      short field_base, field_eq_len_base, field_len_base, field_len_off;
      unsigned short field_len_scale;
      short size_base, size_off;
      unsigned short size_scale;
      char *name;
      sexp_proc2 finalize;
    } type;
    struct {
      sexp car, cdr;
      sexp source;
    } pair;
    struct {
      sexp_uint_t length;
      sexp data[];
    } vector;
    struct {
      sexp_uint_t length;
      char data[];
    } string;
    struct {
      sexp string;
    } symbol;
    struct {
      FILE *stream;
      char *buf;
      char openp, no_closep, sourcep;
      sexp_uint_t offset, line;
      size_t size;
      sexp name;
      sexp cookie;
    } port;
    struct {
      sexp kind, message, irritants, procedure, source;
    } exception;
    struct {
      char sign;
      sexp_uint_t length;
      sexp_uint_t data[];
    } bignum;
    struct {
      sexp_uint_t length;
      void *value;
      sexp parent;
      char body[];
    } cpointer;
    /* runtime types */
    struct {
      unsigned int syntacticp:1;
      sexp parent, lambda, bindings;
    } env;
    struct {
      sexp_uint_t length;
      sexp name, literals;
      unsigned char data[];
    } bytecode;
    struct {
      char flags;
      unsigned short num_args;
      sexp bc, vars;
    } procedure;
    struct {
      sexp proc, env;
    } macro;
    struct {
      sexp env, free_vars, expr;
    } synclo;
    struct {
      unsigned char op_class, code, num_args, flags,
        arg1_type, arg2_type, inverse;
      char *name;
      sexp data, data2, proc;
      sexp_proc1 func;
    } opcode;
    struct {
      char code;
      char *name;
    } core;
    /* ast types */
    struct {
      sexp name, params, body, defs, locals, flags, fv, sv;
    } lambda;
    struct {
      sexp test, pass, fail;
    } cnd;
    struct {
      sexp var, value;
    } set;
    struct {
      sexp name, cell;
    } ref;
    struct {
      sexp ls;
    } seq;
    struct {
      sexp value;
    } lit;
    /* compiler state */
    struct {
      sexp_uint_t length, top;
      sexp data[];
    } stack;
    struct {
      sexp_heap heap;
      struct sexp_gc_var_t *saves;
      sexp_uint_t pos, depth, tailp, tracep;
      sexp bc, lambda, stack, env, fv, parent, globals;
    } context;
  } value;
};

#define SEXP_MAKE_IMMEDIATE(n)  ((sexp) ((n<<SEXP_EXTENDED_BITS) \
                                          + SEXP_EXTENDED_TAG))

#define SEXP_NULL   SEXP_MAKE_IMMEDIATE(0) /* 14 0x0e */
#define SEXP_FALSE  SEXP_MAKE_IMMEDIATE(1) /* 30 0x1e */
#define SEXP_TRUE   SEXP_MAKE_IMMEDIATE(2) /* 46 0x2e */
#define SEXP_EOF    SEXP_MAKE_IMMEDIATE(3) /* 62 0x3e */
#define SEXP_VOID   SEXP_MAKE_IMMEDIATE(4) /* the unspecified value */
#define SEXP_UNDEF  SEXP_MAKE_IMMEDIATE(5) /* internal use */
#define SEXP_CLOSE  SEXP_MAKE_IMMEDIATE(6) /* internal use */
#define SEXP_RAWDOT SEXP_MAKE_IMMEDIATE(7) /* internal use */

#if SEXP_USE_BOEHM

#define sexp_gc_var(ctx, x, y)       sexp x;
#define sexp_gc_preserve(ctx, x, y)
#define sexp_gc_release(ctx, x, y)

#include "gc/gc.h"
#define sexp_alloc(ctx, size)        GC_malloc(size)
#define sexp_alloc_atomic(ctx, size) GC_malloc_atomic(size)
#define sexp_realloc(ctx, x, size)   GC_realloc(x, size)
#define sexp_free(ctx, x)
#define sexp_deep_free(ctx, x)

#else

#define sexp_gc_var(ctx, x, y)                  \
  sexp x = SEXP_VOID;                           \
  struct sexp_gc_var_t y = {NULL, NULL};

#define sexp_gc_preserve(ctx, x, y)     \
  do {                                  \
    (y).var = &(x);                     \
    /* (y).name = #x; */                      \
    (y).next = sexp_context_saves(ctx); \
    sexp_context_saves(ctx) = &(y);     \
  } while (0)

#define sexp_gc_release(ctx, x, y)   (sexp_context_saves(ctx) = y.next)

#if SEXP_USE_MALLOC
#define sexp_alloc(ctx, size)        malloc(size)
#define sexp_alloc_atomic(ctx, size) malloc(size)
#define sexp_realloc(ctx, x, size)   realloc(x, size)
#define sexp_free(ctx, x)            free(x)
void sexp_deep_free(sexp ctx, sexp obj);

#else  /* native gc */
void *sexp_alloc(sexp ctx, size_t size);
#define sexp_alloc_atomic            sexp_alloc
void *sexp_realloc(sexp ctx, sexp x, size_t size);
#define sexp_free(ctx, x)
#define sexp_deep_free(ctx, x)

#endif
#endif

#define sexp_gc_var1(x) sexp_gc_var(ctx, x, __sexp_gc_preserver1)
#define sexp_gc_var2(x, y) sexp_gc_var1(x); sexp_gc_var(ctx, y, __sexp_gc_preserver2)
#define sexp_gc_var3(x, y, z) sexp_gc_var2(x, y); sexp_gc_var(ctx, z, __sexp_gc_preserver3)
#define sexp_gc_var4(x, y, z, w) sexp_gc_var3(x, y, z); sexp_gc_var(ctx, w, __sexp_gc_preserver4)
#define sexp_gc_var5(x, y, z, w, v) sexp_gc_var4(x, y, z, w); sexp_gc_var(ctx, v, __sexp_gc_preserver5)
#define sexp_gc_var6(x, y, z, w, v, u) sexp_gc_var5(x, y, z, w, v); sexp_gc_var(ctx, u, __sexp_gc_preserver6)

#define sexp_gc_preserve1(ctx, x) sexp_gc_preserve(ctx, x, __sexp_gc_preserver1)
#define sexp_gc_preserve2(ctx, x, y) sexp_gc_preserve1(ctx, x); sexp_gc_preserve(ctx, y, __sexp_gc_preserver2)
#define sexp_gc_preserve3(ctx, x, y, z) sexp_gc_preserve2(ctx, x, y); sexp_gc_preserve(ctx, z, __sexp_gc_preserver3)
#define sexp_gc_preserve4(ctx, x, y, z, w) sexp_gc_preserve3(ctx, x, y, z); sexp_gc_preserve(ctx, w, __sexp_gc_preserver4)
#define sexp_gc_preserve5(ctx, x, y, z, w, v) sexp_gc_preserve4(ctx, x, y, z, w); sexp_gc_preserve(ctx, v, __sexp_gc_preserver5)
#define sexp_gc_preserve6(ctx, x, y, z, w, v, u) sexp_gc_preserve5(ctx, x, y, z, w, v); sexp_gc_preserve(ctx, u, __sexp_gc_preserver6)

#define sexp_gc_release1(ctx) sexp_gc_release(ctx, NULL, __sexp_gc_preserver1)
#define sexp_gc_release2(ctx) sexp_gc_release(ctx, NULL, __sexp_gc_preserver1)
#define sexp_gc_release3(ctx) sexp_gc_release(ctx, NULL, __sexp_gc_preserver1)
#define sexp_gc_release4(ctx) sexp_gc_release(ctx, NULL, __sexp_gc_preserver1)
#define sexp_gc_release5(ctx) sexp_gc_release(ctx, NULL, __sexp_gc_preserver1)
#define sexp_gc_release6(ctx) sexp_gc_release(ctx, NULL, __sexp_gc_preserver1)

#define sexp_align(n, bits) (((n)+(1<<(bits))-1)&(((sexp_uint_t)-1)-((1<<(bits))-1)))

#if SEXP_64_BIT
#define sexp_word_align(n) sexp_align((n), 3)
#else
#define sexp_word_align(n) sexp_align((n), 2)
#endif

#define sexp_sizeof(x) (offsetof(struct sexp_struct, value) \
                         + sizeof(((sexp)0)->value.x))

#define sexp_offsetof(type, f) (offsetof(struct sexp_struct, value.type.f))

#define sexp_offsetof_slot0 (offsetof(struct sexp_struct, value))

#define sexp_sizeof_header (sexp_sizeof(flonum) - sizeof(double))

#define sexp_alloc_type(ctx, type, tag) sexp_alloc_tagged(ctx, sexp_sizeof(type), tag)
#define sexp_alloc_bytecode(ctx, i) sexp_alloc_tagged(ctx, sexp_sizeof(bytecode) + i, SEXP_BYTECODE)

#if SEXP_USE_BIGNUMS
#include "chibi/bignum.h"
#endif

/***************************** predicates *****************************/

#define sexp_truep(x)    ((x) != SEXP_FALSE)
#define sexp_not(x)      ((x) == SEXP_FALSE)

#define sexp_nullp(x)    ((x) == SEXP_NULL)
#define sexp_pointerp(x) (((sexp_uint_t)(x) & SEXP_FIXNUM_MASK) == SEXP_POINTER_TAG)
#define sexp_fixnump(x)  (((sexp_uint_t)(x) & SEXP_FIXNUM_MASK) == SEXP_FIXNUM_TAG)
#define sexp_isymbolp(x) (((sexp_uint_t)(x) & SEXP_IMMEDIATE_MASK) == SEXP_ISYMBOL_TAG)
#define sexp_charp(x)    (((sexp_uint_t)(x) & SEXP_EXTENDED_MASK) == SEXP_CHAR_TAG)
#define sexp_booleanp(x) (((x) == SEXP_TRUE) || ((x) == SEXP_FALSE))

#define sexp_pointer_tag(x)      ((x)->tag)
#define sexp_gc_mark(x)          ((x)->gc_mark)
#define sexp_flags(x)            ((x)->flags)
#define sexp_immutablep(x)       ((x)->immutablep)
#define sexp_freep(x)            ((x)->freep)

#define sexp_check_tag(x,t)  (sexp_pointerp(x) && (sexp_pointer_tag(x) == (t)))

#define sexp_slot_ref(x,i)   (((sexp*)&((x)->value))[i])
#define sexp_slot_set(x,i,v) (((sexp*)&((x)->value))[i] = (v))

#if SEXP_USE_IMMEDIATE_FLONUMS
union sexp_flonum_conv {
  float flonum;
  unsigned int bits;
};
#define sexp_flonump(x)      (((sexp_uint_t)(x) & SEXP_IMMEDIATE_MASK) == SEXP_IFLONUM_TAG)
SEXP_API sexp sexp_flonum_predicate (sexp ctx, sexp x);
#if SEXP_64_BIT
SEXP_API float sexp_flonum_value (sexp x);
SEXP_API sexp sexp_make_flonum(sexp ctx, float f);
#else
#define sexp_make_flonum(ctx, x)  ((sexp) ((((union sexp_flonum_conv)((float)(x))).bits & ~SEXP_IMMEDIATE_MASK) + SEXP_IFLONUM_TAG))
#define sexp_flonum_value(x) (((union sexp_flonum_conv)(((unsigned int)(x)) & ~SEXP_IMMEDIATE_MASK)).flonum)
#endif
#else
#define sexp_flonump(x)      (sexp_check_tag(x, SEXP_FLONUM))
#define sexp_flonum_value(f) ((f)->value.flonum)
sexp sexp_make_flonum(sexp ctx, double f);
#endif

#define sexp_typep(x)       (sexp_check_tag(x, SEXP_TYPE))
#define sexp_pairp(x)       (sexp_check_tag(x, SEXP_PAIR))
#define sexp_stringp(x)     (sexp_check_tag(x, SEXP_STRING))
#define sexp_lsymbolp(x)    (sexp_check_tag(x, SEXP_SYMBOL))
#define sexp_vectorp(x)     (sexp_check_tag(x, SEXP_VECTOR))
#define sexp_iportp(x)      (sexp_check_tag(x, SEXP_IPORT))
#define sexp_oportp(x)      (sexp_check_tag(x, SEXP_OPORT))
#define sexp_bignump(x)     (sexp_check_tag(x, SEXP_BIGNUM))
#define sexp_cpointerp(x)   (sexp_check_tag(x, SEXP_CPOINTER))
#define sexp_exceptionp(x)  (sexp_check_tag(x, SEXP_EXCEPTION))
#define sexp_procedurep(x)  (sexp_check_tag(x, SEXP_PROCEDURE))
#define sexp_envp(x)        (sexp_check_tag(x, SEXP_ENV))
#define sexp_bytecodep(x)   (sexp_check_tag(x, SEXP_BYTECODE))
#define sexp_corep(x)       (sexp_check_tag(x, SEXP_CORE))
#define sexp_opcodep(x)     (sexp_check_tag(x, SEXP_OPCODE))
#define sexp_macrop(x)      (sexp_check_tag(x, SEXP_MACRO))
#define sexp_synclop(x)     (sexp_check_tag(x, SEXP_SYNCLO))
#define sexp_lambdap(x)     (sexp_check_tag(x, SEXP_LAMBDA))
#define sexp_cndp(x)        (sexp_check_tag(x, SEXP_CND))
#define sexp_refp(x)        (sexp_check_tag(x, SEXP_REF))
#define sexp_setp(x)        (sexp_check_tag(x, SEXP_SET))
#define sexp_seqp(x)        (sexp_check_tag(x, SEXP_SEQ))
#define sexp_litp(x)        (sexp_check_tag(x, SEXP_LIT))
#define sexp_contextp(x)    (sexp_check_tag(x, SEXP_CONTEXT))

#if SEXP_USE_HUFF_SYMS
#define sexp_symbolp(x)     (sexp_isymbolp(x) || sexp_lsymbolp(x))
#else
#define sexp_symbolp(x)     (sexp_lsymbolp(x))
#endif

#define sexp_idp(x) \
  (sexp_symbolp(x) || (sexp_synclop(x) && sexp_symbolp(sexp_synclo_expr(x))))

#define sexp_portp(x) (sexp_iportp(x) || sexp_oportp(x))

/***************************** constructors ****************************/

#define sexp_make_boolean(x) ((x) ? SEXP_TRUE : SEXP_FALSE)
#define sexp_unbox_boolean(x) (((x) == SEXP_FALSE) ? 0 : 1)

#define sexp_make_fixnum(n)    ((sexp) ((((sexp_sint_t)(n))<<SEXP_FIXNUM_BITS) + SEXP_FIXNUM_TAG))
#define sexp_unbox_fixnum(n)   (((sexp_sint_t)(n))>>SEXP_FIXNUM_BITS)

#define SEXP_NEG_ONE sexp_make_fixnum(-1)
#define SEXP_ZERO    sexp_make_fixnum(0)
#define SEXP_ONE     sexp_make_fixnum(1)
#define SEXP_TWO     sexp_make_fixnum(2)
#define SEXP_THREE   sexp_make_fixnum(3)
#define SEXP_FOUR    sexp_make_fixnum(4)
#define SEXP_FIVE    sexp_make_fixnum(5)
#define SEXP_SIX     sexp_make_fixnum(6)
#define SEXP_SEVEN   sexp_make_fixnum(7)
#define SEXP_EIGHT   sexp_make_fixnum(8)
#define SEXP_NINE    sexp_make_fixnum(9)

#define sexp_make_character(n)  ((sexp) ((((sexp_sint_t)(n))<<SEXP_EXTENDED_BITS) + SEXP_CHAR_TAG))
#define sexp_unbox_character(n) ((int) (((sexp_sint_t)(n))>>SEXP_EXTENDED_BITS))

#define sexp_fixnum_to_double(x) ((double)sexp_unbox_fixnum(x))

#if SEXP_USE_FLONUMS
#define sexp_fp_integerp(x) (sexp_flonum_value(x) == trunc(sexp_flonum_value(x)))
#define _or_integer_flonump(x) || (sexp_flonump(x) && sexp_fp_integerp(x))
#else
#define _or_integer_flonump(x)
#endif

#if SEXP_USE_BIGNUMS
SEXP_API sexp sexp_make_integer(sexp ctx, sexp_lsint_t x);
SEXP_API sexp sexp_make_unsigned_integer(sexp ctx, sexp_luint_t x);
#define sexp_exact_integerp(x) (sexp_fixnump(x) || sexp_bignump(x))
#else
#define sexp_make_integer(ctx, x) sexp_make_fixnum(x)
#define sexp_make_unsigned_integer(ctx, x) sexp_make_fixnum(x)
#define sexp_exact_integerp(x) sexp_fixnump(x)
#endif

#define sexp_integerp(x) (sexp_exact_integerp(x) _or_integer_flonump(x))

#if SEXP_USE_FLONUMS
#define sexp_fixnum_to_flonum(ctx, x) (sexp_make_flonum(ctx, sexp_unbox_fixnum(x)))
#else
#define sexp_fixnum_to_flonum(ctx, x) (x)
#endif

#if SEXP_USE_FLONUMS || SEXP_USE_BIGNUMS
#define sexp_uint_value(x) ((sexp_uint_t)(sexp_fixnump(x) ? sexp_unbox_fixnum(x) : sexp_bignum_data(x)[0]))
#define sexp_sint_value(x) ((sexp_sint_t)(sexp_fixnump(x) ? sexp_unbox_fixnum(x) : sexp_bignum_sign(x)*sexp_bignum_data(x)[0]))
#else
#define sexp_uint_value(x) ((sexp_uint_t)sexp_unbox_fixnum(x))
#define sexp_sint_value(x) ((sexp_sint_t)sexp_unbox_fixnum(x))
#endif

#define sexp_shift_epoch(x) ((x)-SEXP_EPOCH_OFFSET)
#define sexp_unshift_epoch(x) ((x)+SEXP_EPOCH_OFFSET)

/*************************** field accessors **************************/

#define sexp_vector_length(x) ((x)->value.vector.length)
#define sexp_vector_data(x)   ((x)->value.vector.data)

#define sexp_vector_ref(x,i)   (sexp_vector_data(x)[sexp_unbox_fixnum(i)])
#define sexp_vector_set(x,i,v) (sexp_vector_data(x)[sexp_unbox_fixnum(i)]=(v))

#define sexp_procedure_num_args(x) ((x)->value.procedure.num_args)
#define sexp_procedure_flags(x) ((x)->value.procedure.flags)
#define sexp_procedure_variadic_p(x) (sexp_unbox_fixnum(sexp_procedure_flags(x)) & 1)
#define sexp_procedure_code(x) ((x)->value.procedure.bc)
#define sexp_procedure_vars(x) ((x)->value.procedure.vars)

#define sexp_string_length(x) ((x)->value.string.length)
#define sexp_string_data(x)   ((x)->value.string.data)

#define sexp_string_ref(x, i) (sexp_make_character((unsigned char)sexp_string_data(x)[sexp_unbox_fixnum(i)]))
#define sexp_string_set(x, i, v) (sexp_string_data(x)[sexp_unbox_fixnum(i)] = sexp_unbox_character(v))

#define sexp_symbol_string(x)  ((x)->value.symbol.string)

#define sexp_port_stream(p)    ((p)->value.port.stream)
#define sexp_port_name(p)      ((p)->value.port.name)
#define sexp_port_line(p)      ((p)->value.port.line)
#define sexp_port_openp(p)     ((p)->value.port.openp)
#define sexp_port_no_closep(p) ((p)->value.port.no_closep)
#define sexp_port_sourcep(p)   ((p)->value.port.sourcep)
#define sexp_port_cookie(p)    ((p)->value.port.cookie)
#define sexp_port_buf(p)       ((p)->value.port.buf)
#define sexp_port_size(p)      ((p)->value.port.size)
#define sexp_port_offset(p)    ((p)->value.port.offset)

#define sexp_exception_kind(p)      ((p)->value.exception.kind)
#define sexp_exception_message(p)   ((p)->value.exception.message)
#define sexp_exception_irritants(p) ((p)->value.exception.irritants)
#define sexp_exception_procedure(p) ((p)->value.exception.procedure)
#define sexp_exception_source(p)    ((p)->value.exception.source)

#define sexp_cpointer_freep(p)      (sexp_freep(p))
#define sexp_cpointer_length(p)     ((p)->value.cpointer.length)
#define sexp_cpointer_body(p)       ((p)->value.cpointer.body)
#define sexp_cpointer_parent(p)     ((p)->value.cpointer.parent)
#define sexp_cpointer_value(p)      ((p)->value.cpointer.value)
#define sexp_cpointer_maybe_null_value(p) (sexp_not(p) ? NULL : sexp_cpointer_value(p))

#define sexp_bytecode_length(x)   ((x)->value.bytecode.length)
#define sexp_bytecode_name(x)     ((x)->value.bytecode.name)
#define sexp_bytecode_literals(x) ((x)->value.bytecode.literals)
#define sexp_bytecode_data(x)     ((x)->value.bytecode.data)

#define sexp_env_syntactic_p(x)   ((x)->value.env.syntacticp)
#define sexp_env_parent(x)        ((x)->value.env.parent)
#define sexp_env_bindings(x)      ((x)->value.env.bindings)
#define sexp_env_local_p(x)       (sexp_env_parent(x))
#define sexp_env_global_p(x)      (! sexp_env_local_p(x))
#define sexp_env_lambda(x)        ((x)->value.env.lambda)

#define sexp_macro_proc(x)        ((x)->value.macro.proc)
#define sexp_macro_env(x)         ((x)->value.macro.env)

#define sexp_synclo_env(x)        ((x)->value.synclo.env)
#define sexp_synclo_free_vars(x)  ((x)->value.synclo.free_vars)
#define sexp_synclo_expr(x)       ((x)->value.synclo.expr)

#define sexp_core_code(x)         ((x)->value.core.code)
#define sexp_core_name(x)         ((x)->value.core.name)

#define sexp_opcode_class(x)      ((x)->value.opcode.op_class)
#define sexp_opcode_code(x)       ((x)->value.opcode.code)
#define sexp_opcode_num_args(x)   ((x)->value.opcode.num_args)
#define sexp_opcode_flags(x)      ((x)->value.opcode.flags)
#define sexp_opcode_arg1_type(x)  ((x)->value.opcode.arg1_type)
#define sexp_opcode_arg2_type(x)  ((x)->value.opcode.arg2_type)
#define sexp_opcode_inverse(x)    ((x)->value.opcode.inverse)
#define sexp_opcode_name(x)       ((x)->value.opcode.name)
#define sexp_opcode_data(x)       ((x)->value.opcode.data)
#define sexp_opcode_data2(x)      ((x)->value.opcode.data2)
#define sexp_opcode_proc(x)       ((x)->value.opcode.proc)
#define sexp_opcode_func(x)       ((x)->value.opcode.func)

#define sexp_opcode_variadic_p(x)  (sexp_opcode_flags(x) & 1)
#define sexp_opcode_opt_param_p(x) (sexp_opcode_flags(x) & 2)
#define sexp_opcode_ref_trans_p(x) (sexp_opcode_flags(x) & 4)

#define sexp_lambda_name(x)   ((x)->value.lambda.name)
#define sexp_lambda_params(x) ((x)->value.lambda.params)
#define sexp_lambda_locals(x) ((x)->value.lambda.locals)
#define sexp_lambda_defs(x)   ((x)->value.lambda.defs)
#define sexp_lambda_flags(x)  ((x)->value.lambda.flags)
#define sexp_lambda_body(x)   ((x)->value.lambda.body)
#define sexp_lambda_fv(x)     ((x)->value.lambda.fv)
#define sexp_lambda_sv(x)     ((x)->value.lambda.sv)

#define sexp_cnd_test(x)      ((x)->value.cnd.test)
#define sexp_cnd_pass(x)      ((x)->value.cnd.pass)
#define sexp_cnd_fail(x)      ((x)->value.cnd.fail)

#define sexp_set_var(x)       ((x)->value.set.var)
#define sexp_set_value(x)     ((x)->value.set.value)

#define sexp_ref_name(x)      ((x)->value.ref.name)
#define sexp_ref_cell(x)      ((x)->value.ref.cell)
#define sexp_ref_loc(x)       (sexp_cdr(sexp_ref_cell(x)))

#define sexp_seq_ls(x)        ((x)->value.seq.ls)

#define sexp_lit_value(x)     ((x)->value.lit.value)

#define sexp_stack_length(x)  ((x)->value.stack.length)
#define sexp_stack_top(x)     ((x)->value.stack.top)
#define sexp_stack_data(x)    ((x)->value.stack.data)

#define sexp_context_env(x)     ((x)->value.context.env)
#define sexp_context_stack(x)   ((x)->value.context.stack)
#define sexp_context_depth(x)   ((x)->value.context.depth)
#define sexp_context_bc(x)      ((x)->value.context.bc)
#define sexp_context_fv(x)      ((x)->value.context.fv)
#define sexp_context_pos(x)     ((x)->value.context.pos)
#define sexp_context_lambda(x)  ((x)->value.context.lambda)
#define sexp_context_parent(x)  ((x)->value.context.parent)
#define sexp_context_saves(x)   ((x)->value.context.saves)
#define sexp_context_tailp(x)   ((x)->value.context.tailp)
#define sexp_context_tracep(x)  ((x)->value.context.tailp)
#define sexp_context_globals(x) ((x)->value.context.globals)

#if SEXP_USE_ALIGNED_BYTECODE
#define sexp_context_align_pos(ctx) sexp_context_pos(ctx) = sexp_word_align(sexp_context_pos(ctx))
#else
#define sexp_context_align_pos(ctx)
#endif

#define sexp_global(ctx,x)      (sexp_vector_data(sexp_context_globals(ctx))[x])

#if SEXP_USE_GLOBAL_HEAP
#if ! SEXP_USE_BOEHM
SEXP_API sexp_heap sexp_global_heap;
#endif
#define sexp_context_heap(ctx)  sexp_global_heap
#else
#define sexp_context_heap(ctx)  ((ctx)->value.context.heap)
#endif

#if SEXP_USE_GLOBAL_SYMBOLS
#define sexp_context_symbols(ctx) sexp_symbol_table
#else
#define sexp_context_symbols(ctx) sexp_vector_data(sexp_global(ctx, SEXP_G_SYMBOLS))
#endif

#if SEXP_USE_GLOBAL_TYPES
SEXP_API struct sexp_struct *sexp_type_specs;
#define sexp_context_types(ctx)    sexp_type_specs
#define sexp_type_by_index(ctx,i)  (&(sexp_context_types(ctx)[i]))
#define sexp_context_num_types(ctx) sexp_num_types
#define sexp_context_type_array_size(ctx) sexp_type_array_size
#else
#define sexp_context_types(ctx)    sexp_vector_data(sexp_global(ctx, SEXP_G_TYPES))
#define sexp_type_by_index(ctx,i)  (sexp_context_types(ctx)[i])
#define sexp_context_num_types(ctx)             \
  sexp_unbox_fixnum(sexp_global(ctx, SEXP_G_NUM_TYPES))
#define sexp_context_type_array_size(ctx)                               \
  sexp_vector_length(sexp_global(ctx, SEXP_G_TYPES))
#endif

#define sexp_object_type(ctx,x)        (sexp_type_by_index(ctx, ((x)->tag)))
#define sexp_object_type_name(ctx,x)   (sexp_type_name(sexp_object_type(ctx, x)))
#define sexp_type_name_by_index(ctx,i) (sexp_type_name(sexp_type_by_index(ctx,i)))

#define sexp_type_size_of_object(t, x)                                  \
  (((sexp_uint_t*)((char*)x + sexp_type_size_off(t)))[0]                \
   * sexp_type_size_scale(t)                                            \
   + sexp_type_size_base(t))
#define sexp_type_num_slots_of_object(t, x)                             \
  (((sexp_uint_t*)((char*)x + sexp_type_field_len_off(t)))[0]           \
   * sexp_type_field_len_scale(t)                                       \
   + sexp_type_field_len_base(t))
#define sexp_type_num_eq_slots_of_object(t, x)                          \
  (((sexp_uint_t*)((char*)x + sexp_type_field_len_off(t)))[0]           \
   * sexp_type_field_len_scale(t)                                       \
   + sexp_type_field_eq_len_base(t))

#define sexp_context_top(x)     (sexp_stack_top(sexp_context_stack(x)))

#define sexp_type_tag(x)               ((x)->value.type.tag)
#define sexp_type_field_base(x)        ((x)->value.type.field_base)
#define sexp_type_field_eq_len_base(x) ((x)->value.type.field_eq_len_base)
#define sexp_type_field_len_base(x)    ((x)->value.type.field_len_base)
#define sexp_type_field_len_off(x)     ((x)->value.type.field_len_off)
#define sexp_type_field_len_scale(x)   ((x)->value.type.field_len_scale)
#define sexp_type_size_base(x)         ((x)->value.type.size_base)
#define sexp_type_size_off(x)          ((x)->value.type.size_off)
#define sexp_type_size_scale(x)        ((x)->value.type.size_scale)
#define sexp_type_name(x)              ((x)->value.type.name)
#define sexp_type_finalize(x)          ((x)->value.type.finalize)

#define sexp_bignum_sign(x)           ((x)->value.bignum.sign)
#define sexp_bignum_length(x)         ((x)->value.bignum.length)
#define sexp_bignum_data(x)           ((x)->value.bignum.data)

/****************************** arithmetic ****************************/

#define sexp_fx_add(a, b) ((sexp)(((sexp_sint_t)a)+((sexp_sint_t)b)-SEXP_FIXNUM_TAG))
#define sexp_fx_sub(a, b) ((sexp)(((sexp_sint_t)a)-((sexp_sint_t)b)+SEXP_FIXNUM_TAG))
#define sexp_fx_mul(a, b) ((sexp)((((((sexp_sint_t)a)-SEXP_FIXNUM_TAG)*(((sexp_sint_t)b)>>SEXP_FIXNUM_BITS))+SEXP_FIXNUM_TAG)))
#define sexp_fx_div(a, b) (sexp_make_fixnum(sexp_unbox_fixnum(a) / sexp_unbox_fixnum(b)))
#define sexp_fx_rem(a, b) (sexp_make_fixnum(sexp_unbox_fixnum(a) % sexp_unbox_fixnum(b)))
#define sexp_fx_sign(a)   (+1 | (((sexp_sint_t)(a)) >> (sizeof(sexp_sint_t)*8 - 1)))
#define sexp_fx_neg(a)    (sexp_make_fixnum(-(sexp_unbox_fixnum(a))))
#define sexp_fx_abs(a)    ((((sexp_sint_t)a) < 0) ? sexp_fx_neg(a) : a)

#define sexp_fp_add(x,a,b) (sexp_make_flonum(x, sexp_flonum_value(a) + sexp_flonum_value(b)))
#define sexp_fp_sub(x,a,b) (sexp_make_flonum(x, sexp_flonum_value(a) - sexp_flonum_value(b)))
#define sexp_fp_mul(x,a,b) (sexp_make_flonum(x, sexp_flonum_value(a) * sexp_flonum_value(b)))
#define sexp_fp_div(x,a,b) (sexp_make_flonum(x, sexp_flonum_value(a) / sexp_flonum_value(b)))

/****************************** utilities *****************************/

enum sexp_context_globals {
#if ! SEXP_USE_GLOBAL_SYMBOLS
  SEXP_G_SYMBOLS,
#endif
#if ! SEXP_USE_GLOBAL_TYPES
  SEXP_G_TYPES,
  SEXP_G_NUM_TYPES,
#endif
  SEXP_G_OOM_ERROR,             /* out of memory exception object */
  SEXP_G_OOS_ERROR,             /* out of stack exception object */
  SEXP_G_OPTIMIZATIONS,
  SEXP_G_SIGNAL_HANDLERS,
  SEXP_G_CONFIG_ENV,
  SEXP_G_MODULE_PATH,
  SEXP_G_QUOTE_SYMBOL,
  SEXP_G_QUASIQUOTE_SYMBOL,
  SEXP_G_UNQUOTE_SYMBOL,
  SEXP_G_UNQUOTE_SPLICING_SYMBOL,
  SEXP_G_EMPTY_VECTOR,
  SEXP_G_CUR_IN_SYMBOL,
  SEXP_G_CUR_OUT_SYMBOL,
  SEXP_G_CUR_ERR_SYMBOL,
  SEXP_G_INTERACTION_ENV_SYMBOL,
  SEXP_G_ERR_HANDLER,
  SEXP_G_RESUMECC_BYTECODE,
  SEXP_G_FINAL_RESUMER,
  SEXP_G_NUM_GLOBALS
};

#define sexp_list1(x,a)        sexp_cons((x), (a), SEXP_NULL)

#define sexp_push(ctx, ls, x)    ((ls) = sexp_cons((ctx), (x), (ls)))
#define sexp_insert(ctx, ls, x)  ((sexp_memq(ctx, (x), (ls)) != SEXP_FALSE) ? (ls) : sexp_push((ctx), (ls), (x)))

#define sexp_pair_source(x) ((x)->value.pair.source)

#define sexp_car(x)       ((x)->value.pair.car)
#define sexp_cdr(x)       ((x)->value.pair.cdr)

#define sexp_caar(x)      (sexp_car(sexp_car(x)))
#define sexp_cadr(x)      (sexp_car(sexp_cdr(x)))
#define sexp_cdar(x)      (sexp_cdr(sexp_car(x)))
#define sexp_cddr(x)      (sexp_cdr(sexp_cdr(x)))
#define sexp_caaar(x)     (sexp_car(sexp_caar(x)))
#define sexp_caadr(x)     (sexp_car(sexp_cadr(x)))
#define sexp_cadar(x)     (sexp_car(sexp_cdar(x)))
#define sexp_caddr(x)     (sexp_car(sexp_cddr(x)))
#define sexp_cdaar(x)     (sexp_cdr(sexp_caar(x)))
#define sexp_cdadr(x)     (sexp_cdr(sexp_cadr(x)))
#define sexp_cddar(x)     (sexp_cdr(sexp_cdar(x)))
#define sexp_cdddr(x)     (sexp_cdr(sexp_cddr(x)))
#define sexp_cadddr(x)    (sexp_cadr(sexp_cddr(x))) /* just these two */
#define sexp_cddddr(x)    (sexp_cddr(sexp_cddr(x)))

/***************************** general API ****************************/

#if SEXP_USE_STRING_STREAMS

#define sexp_read_char(x, p) (getc(sexp_port_stream(p)))
#define sexp_push_char(x, c, p) (ungetc(c, sexp_port_stream(p)))
#define sexp_write_char(x, c, p) (putc(c, sexp_port_stream(p)))
#define sexp_write_string(x, s, p) (fputs(s, sexp_port_stream(p)))
#define sexp_printf(x, p, ...) (fprintf(sexp_port_stream(p), __VA_ARGS__))
#define sexp_flush(x, p) (fflush(sexp_port_stream(p)))

#else

#define sexp_read_char(x, p) (sexp_port_buf(p) ? ((sexp_port_offset(p) < sexp_port_size(p)) ? sexp_port_buf(p)[sexp_port_offset(p)++] : sexp_buffered_read_char(x, p)) : getc(sexp_port_stream(p)))
#define sexp_push_char(x, c, p) (sexp_port_buf(p) ? (sexp_port_buf(p)[--sexp_port_offset(p)] = ((char)(c))) : ungetc(c, sexp_port_stream(p)))
#define sexp_write_char(x, c, p) (sexp_port_buf(p) ? ((sexp_port_offset(p) < sexp_port_size(p)) ? ((((sexp_port_buf(p))[sexp_port_offset(p)++]) = (char)(c)), SEXP_VOID) : sexp_buffered_write_char(x, c, p)) : (putc(c, sexp_port_stream(p)), SEXP_VOID))
#define sexp_write_string(x, s, p) (sexp_port_buf(p) ? sexp_buffered_write_string(x, s, p) : (fputs(s, sexp_port_stream(p)), SEXP_VOID))
#define sexp_flush(x, p) (sexp_port_buf(p) ? sexp_buffered_flush(x, p) : (fflush(sexp_port_stream(p)), SEXP_VOID))

SEXP_API int sexp_buffered_read_char (sexp ctx, sexp p);
SEXP_API sexp sexp_buffered_write_char (sexp ctx, int c, sexp p);
SEXP_API sexp sexp_buffered_write_string_n (sexp ctx, char *str, sexp_uint_t len, sexp p);
SEXP_API sexp sexp_buffered_write_string (sexp ctx, char *str, sexp p);
SEXP_API sexp sexp_buffered_flush (sexp ctx, sexp p);

#endif

#define sexp_newline(ctx, p) sexp_write_char(ctx, '\n', (p))

SEXP_API sexp sexp_make_context(sexp ctx, sexp_uint_t size);
SEXP_API sexp sexp_alloc_tagged(sexp ctx, size_t size, sexp_uint_t tag);
SEXP_API sexp sexp_cons(sexp ctx, sexp head, sexp tail);
SEXP_API sexp sexp_list2(sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_equalp (sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_listp(sexp ctx, sexp obj);
SEXP_API sexp sexp_reverse(sexp ctx, sexp ls);
SEXP_API sexp sexp_nreverse(sexp ctx, sexp ls);
SEXP_API sexp sexp_append2(sexp ctx, sexp a, sexp b);
SEXP_API sexp sexp_memq(sexp ctx, sexp x, sexp ls);
SEXP_API sexp sexp_assq(sexp ctx, sexp x, sexp ls);
SEXP_API sexp sexp_length(sexp ctx, sexp ls);
SEXP_API sexp sexp_c_string(sexp ctx, const char *str, sexp_sint_t slen);
SEXP_API sexp sexp_make_string(sexp ctx, sexp len, sexp ch);
SEXP_API sexp sexp_substring (sexp ctx, sexp str, sexp start, sexp end);
SEXP_API sexp sexp_string_concatenate (sexp ctx, sexp str_ls, sexp sep);
SEXP_API sexp sexp_intern(sexp ctx, char *str);
SEXP_API sexp sexp_string_to_symbol(sexp ctx, sexp str);
SEXP_API sexp sexp_make_vector(sexp ctx, sexp len, sexp dflt);
SEXP_API sexp sexp_list_to_vector(sexp ctx, sexp ls);
SEXP_API sexp sexp_make_cpointer(sexp ctx, sexp_uint_t type_id, void* value, sexp parent, int freep);
SEXP_API sexp sexp_write(sexp ctx, sexp obj, sexp out);
SEXP_API sexp sexp_display(sexp ctx, sexp obj, sexp out);
SEXP_API sexp sexp_flush_output(sexp ctx, sexp out);
SEXP_API sexp sexp_read_string(sexp ctx, sexp in);
SEXP_API sexp sexp_read_symbol(sexp ctx, sexp in, int init, int internp);
SEXP_API sexp sexp_read_number(sexp ctx, sexp in, int base);
SEXP_API sexp sexp_read_raw(sexp ctx, sexp in);
SEXP_API sexp sexp_read(sexp ctx, sexp in);
SEXP_API sexp sexp_read_from_string(sexp ctx, char *str);
SEXP_API sexp sexp_write_to_string(sexp ctx, sexp obj);
SEXP_API sexp sexp_finalize_port (sexp ctx, sexp port);
SEXP_API sexp sexp_make_input_port(sexp ctx, FILE* in, sexp name);
SEXP_API sexp sexp_make_output_port(sexp ctx, FILE* out, sexp name);
SEXP_API sexp sexp_make_input_string_port(sexp ctx, sexp str);
SEXP_API sexp sexp_make_output_string_port(sexp ctx);
SEXP_API sexp sexp_get_output_string(sexp ctx, sexp port);
SEXP_API sexp sexp_make_exception(sexp ctx, sexp kind, sexp message, sexp irritants, sexp procedure, sexp source);
SEXP_API sexp sexp_user_exception(sexp ctx, sexp self, char *message, sexp obj);
SEXP_API sexp sexp_type_exception(sexp ctx, char *message, sexp obj);
SEXP_API sexp sexp_range_exception(sexp ctx, sexp obj, sexp start, sexp end);
SEXP_API sexp sexp_print_exception(sexp ctx, sexp exn, sexp out);
SEXP_API void sexp_init(void);

#define SEXP_COPY_DEFAULT SEXP_ZERO
#define SEXP_COPY_FREEP   SEXP_ONE

#if SEXP_USE_GLOBAL_HEAP
#define sexp_destroy_context(ctx)
#else
SEXP_API void sexp_destroy_context(sexp ctx);
SEXP_API sexp sexp_copy_context(sexp ctx, sexp dst, sexp flags);
#endif

#if SEXP_USE_TYPE_DEFS
SEXP_API sexp sexp_register_type (sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp_proc2);
SEXP_API sexp sexp_register_simple_type (sexp ctx, sexp name, sexp slots);
SEXP_API sexp sexp_register_c_type (sexp ctx, sexp name);
SEXP_API sexp sexp_finalize_c_type (sexp ctx, sexp obj);
#define sexp_register_c_type(ctx, name, finalizer)                      \
  sexp_register_type(ctx, name, SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, \
                     SEXP_ZERO, sexp_make_fixnum(sexp_sizeof(cpointer)), \
                     SEXP_ZERO, SEXP_ZERO, finalizer)
#endif

#define sexp_current_error_port(ctx) sexp_env_global_ref(sexp_context_env(ctx),sexp_global(ctx,SEXP_G_CUR_ERR_SYMBOL),SEXP_FALSE)
#define sexp_debug(ctx, msg, obj) (sexp_write_string(ctx, msg, sexp_current_error_port(ctx)), sexp_write(ctx, obj, sexp_current_error_port(ctx)), sexp_write_char(ctx, '\n', sexp_current_error_port(ctx)))

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* ! SEXP_H */

