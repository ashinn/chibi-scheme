/*  sexp.h -- header for sexp library                    */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifndef SEXP_H
#define SEXP_H

#include "config.h"
#include "defaults.h"

#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <sysexits.h>
#include <sys/types.h>
#include <math.h>

/* tagging system
 *   bits end in  00:  pointer
 *                01:  fixnum
 *               011:  <unused>
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
#define SEXP_ISYMBOL_TAG 7
#define SEXP_CHAR_TAG 6
#define SEXP_EXTENDED_TAG 14

#define SEXP_MAX_INT ((1<<29)-1)
#define SEXP_MIN_INT (-(1<<29))

#if USE_HASH_SYMS
#define SEXP_SYMBOL_TABLE_SIZE 389
#else
#define SEXP_SYMBOL_TABLE_SIZE 1
#endif

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
  SEXP_EXCEPTION,
  /* the following are used only by the evaluator */
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
  SEXP_CONTEXT,
};

typedef unsigned long sexp_uint_t;
typedef long sexp_sint_t;
typedef char sexp_tag_t;
typedef struct sexp_struct *sexp;

struct sexp_gc_var_t {
  sexp *var;
  struct sexp_gc_var_t *next;
};

struct sexp_struct {
  sexp_tag_t tag;
  char immutablep;
  char gc_mark;
  union {
    /* basic types */
    double flonum;
    struct {
      sexp car, cdr;
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
      char *name;
      sexp_uint_t line;
      sexp cookie;
    } port;
    struct {
      sexp kind, message, irritants, procedure, file, line;
    } exception;
    struct {
      char sign;
      sexp_uint_t length;
      sexp_uint_t *data;
    } bignum;
    /* runtime types */
    struct {
      char flags;
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
      sexp dflt, data, proc;
    } opcode;
    struct {
      char code;
      char *name;
    } core;
    /* ast types */
    struct {
      sexp name, params, locals, defs, flags, body, fv, sv;
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
      sexp bc, lambda, *stack, env, fv, parent;
      struct sexp_gc_var_t *saves;
      sexp_uint_t pos, top, depth, tailp, tracep;
    } context;
  } value;
};

#if USE_BOEHM

#define sexp_gc_var(ctx, x, y)       sexp x;
#define sexp_gc_preserve(ctx, x, y)
#define sexp_gc_release(ctx, x, y)

#include "gc/include/gc.h"
#define sexp_alloc(ctx, size)        GC_malloc(size)
#define sexp_alloc_atomic(ctx, size) GC_malloc_atomic(size)
#define sexp_realloc(ctx, x, size)   GC_realloc(x, size)
#define sexp_free(ctx, x)
#define sexp_deep_free(ctx, x)

#else

#define sexp_gc_var(ctx, x, y) \
  sexp x = SEXP_FALSE;         \
  struct sexp_gc_var_t y;

#define sexp_gc_preserve(ctx, x, y)  ((y).var=&(x),                       \
                                      (y).next = sexp_context_saves(ctx), \
                                      sexp_context_saves(ctx) = &(y))
#define sexp_gc_release(ctx, x, y)   (sexp_context_saves(ctx) = y.next)

#define sexp_with_gc_var1(ctx, x, body)    \
  sexp_gc_var(ctx, x, _sexp_gcv1);         \
  sexp_gc_preserve(ctx, x, _sexp_gcv1);    \
  do {body} while (0);                     \
  sexp_gc_release(ctx, x, _sexp_gcv1);

#define sexp_with_gc_var2(ctx, x, y, body) \
  sexp_gc_var(ctx, x, _sexp_gcv1);         \
  sexp_gc_var(ctx, y, _sexp_gcv2);         \
  sexp_gc_preserve(ctx, x, _sexp_gcv1);    \
  sexp_gc_preserve(ctx, y, _sexp_gcv2);    \
  do {body} while (0);                     \
  sexp_gc_release(ctx, x, _sexp_gcv1);     \
  sexp_gc_release(ctx, y, _sexp_gcv2);

#if USE_MALLOC
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

#define sexp_align(n, bits) (((n)+(1<<(bits))-1)&(((sexp_uint_t)-1)-((1<<(bits))-1)))

#define sexp_sizeof(x) (offsetof(struct sexp_struct, value) \
                         + sizeof(((sexp)0)->value.x))

#define sexp_alloc_type(ctx, type, tag) sexp_alloc_tagged(ctx, sexp_sizeof(type), tag)

#define SEXP_MAKE_IMMEDIATE(n)  ((sexp) ((n<<SEXP_EXTENDED_BITS) \
                                          + SEXP_EXTENDED_TAG))

#define SEXP_NULL   SEXP_MAKE_IMMEDIATE(0)
#define SEXP_FALSE  SEXP_MAKE_IMMEDIATE(1)
#define SEXP_TRUE   SEXP_MAKE_IMMEDIATE(2)
#define SEXP_EOF    SEXP_MAKE_IMMEDIATE(3)
#define SEXP_VOID   SEXP_MAKE_IMMEDIATE(4) /* the unspecified value */
#define SEXP_UNDEF  SEXP_MAKE_IMMEDIATE(5) /* internal use */
#define SEXP_CLOSE  SEXP_MAKE_IMMEDIATE(6) /* internal use */
#define SEXP_RAWDOT SEXP_MAKE_IMMEDIATE(7) /* internal use */

/***************************** predicates *****************************/

#define sexp_nullp(x)    ((x) == SEXP_NULL)
#define sexp_pointerp(x) (((sexp_uint_t)(x) & SEXP_FIXNUM_MASK) == SEXP_POINTER_TAG)
#define sexp_integerp(x) (((sexp_uint_t)(x) & SEXP_FIXNUM_MASK) == SEXP_FIXNUM_TAG)
#define sexp_isymbolp(x) (((sexp_uint_t)(x) & SEXP_IMMEDIATE_MASK) == SEXP_ISYMBOL_TAG)
#define sexp_charp(x)    (((sexp_uint_t)(x) & SEXP_EXTENDED_MASK) == SEXP_CHAR_TAG)
#define sexp_booleanp(x) (((x) == SEXP_TRUE) || ((x) == SEXP_FALSE))

#define sexp_pointer_tag(x) ((x)->tag)
#define sexp_gc_mark(x)     ((x)->gc_mark)

#define sexp_check_tag(x,t) (sexp_pointerp(x) && (sexp_pointer_tag(x) == (t)))

#define sexp_pairp(x)       (sexp_check_tag(x, SEXP_PAIR))
#define sexp_stringp(x)     (sexp_check_tag(x, SEXP_STRING))
#define sexp_lsymbolp(x)    (sexp_check_tag(x, SEXP_SYMBOL))
#define sexp_vectorp(x)     (sexp_check_tag(x, SEXP_VECTOR))
#define sexp_flonump(x)     (sexp_check_tag(x, SEXP_FLONUM))
#define sexp_iportp(x)      (sexp_check_tag(x, SEXP_IPORT))
#define sexp_oportp(x)      (sexp_check_tag(x, SEXP_OPORT))
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
#define sexp_symbolp(x)     (sexp_isymbolp(x) || sexp_lsymbolp(x))

#define sexp_idp(x) \
  (sexp_symbolp(x) || (sexp_synclop(x) && sexp_symbolp(sexp_synclo_expr(x))))

/***************************** constructors ****************************/

#define sexp_make_boolean(x) ((x) ? SEXP_TRUE : SEXP_FALSE)
#define sexp_unbox_boolean(x) (((x) == SEXP_FALSE) ? 0 : 1)

#define sexp_make_integer(n)    ((sexp) ((((sexp_sint_t)n)<<SEXP_FIXNUM_BITS) + SEXP_FIXNUM_TAG))
#define sexp_unbox_integer(n)   (((sexp_sint_t)n)>>SEXP_FIXNUM_BITS)

#define sexp_make_character(n)  ((sexp) ((((sexp_sint_t)n)<<SEXP_EXTENDED_BITS) + SEXP_CHAR_TAG))
#define sexp_unbox_character(n) ((int) (((sexp_sint_t)n)>>SEXP_EXTENDED_BITS))

#define sexp_flonum_value(f) ((f)->value.flonum)

#if USE_FLONUMS
#define sexp_integer_to_flonum(ctx, x) (sexp_make_flonum(ctx, sexp_unbox_integer(x)))
#else
#define sexp_integer_to_flonum(ctx, x) (x)
#endif

/*************************** field accessors **************************/

#define sexp_vector_length(x) ((x)->value.vector.length)
#define sexp_vector_data(x)   ((x)->value.vector.data)

#define sexp_vector_ref(x,i)   (sexp_vector_data(x)[sexp_unbox_integer(i)])
#define sexp_vector_set(x,i,v) (sexp_vector_data(x)[sexp_unbox_integer(i)]=(v))

#define sexp_procedure_num_args(x) ((x)->value.procedure.num_args)
#define sexp_procedure_flags(x) ((x)->value.procedure.flags)
#define sexp_procedure_variadic_p(x) (sexp_unbox_integer(sexp_procedure_flags(x)) & 1)
#define sexp_procedure_code(x) ((x)->value.procedure.bc)
#define sexp_procedure_vars(x) ((x)->value.procedure.vars)

#define sexp_string_length(x) ((x)->value.string.length)
#define sexp_string_data(x)   ((x)->value.string.data)

#define sexp_string_ref(x, i) (sexp_make_character(sexp_string_data(x)[sexp_unbox_integer(i)]))
#define sexp_string_set(x, i, v) (sexp_string_data(x)[sexp_unbox_integer(i)] = sexp_unbox_character(v))

#define sexp_symbol_string(x)  ((x)->value.symbol.string)

#define sexp_port_stream(p)    ((p)->value.port.stream)
#define sexp_port_name(p)      ((p)->value.port.name)
#define sexp_port_line(p)      ((p)->value.port.line)
#define sexp_port_cookie(p)    ((p)->value.port.cookie)

#define sexp_exception_kind(p)      ((p)->value.exception.kind)
#define sexp_exception_message(p)   ((p)->value.exception.message)
#define sexp_exception_irritants(p) ((p)->value.exception.irritants)
#define sexp_exception_procedure(p) ((p)->value.exception.procedure)
#define sexp_exception_file(p)      ((p)->value.exception.file)
#define sexp_exception_line(p)      ((p)->value.exception.line)

#define sexp_bytecode_length(x)   ((x)->value.bytecode.length)
#define sexp_bytecode_name(x)     ((x)->value.bytecode.name)
#define sexp_bytecode_literals(x) ((x)->value.bytecode.literals)
#define sexp_bytecode_data(x)     ((x)->value.bytecode.data)

#define sexp_env_flags(x)         ((x)->value.env.flags)
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
#define sexp_opcode_default(x)    ((x)->value.opcode.dflt)
#define sexp_opcode_data(x)       ((x)->value.opcode.data)
#define sexp_opcode_proc(x)       ((x)->value.opcode.proc)

#define sexp_opcode_variadic_p(x)  (sexp_opcode_flags(x) & 1)
#define sexp_opcode_opt_param_p(x) (sexp_opcode_flags(x) & 2)

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

#define sexp_context_env(x)     ((x)->value.context.env)
#define sexp_context_stack(x)   ((x)->value.context.stack)
#define sexp_context_depth(x)   ((x)->value.context.depth)
#define sexp_context_bc(x)      ((x)->value.context.bc)
#define sexp_context_fv(x)      ((x)->value.context.fv)
#define sexp_context_pos(x)     ((x)->value.context.pos)
#define sexp_context_top(x)     ((x)->value.context.top)
#define sexp_context_lambda(x)  ((x)->value.context.lambda)
#define sexp_context_parent(x)  ((x)->value.context.parent)
#define sexp_context_saves(x)   ((x)->value.context.saves)
#define sexp_context_tailp(x)   ((x)->value.context.tailp)
#define sexp_context_tracep(x)  ((x)->value.context.tailp)

/****************************** arithmetic ****************************/

#define sexp_fx_add(a, b) ((sexp)(((sexp_sint_t)a)+((sexp_sint_t)b)-SEXP_FIXNUM_TAG))
#define sexp_fx_sub(a, b) ((sexp)(((sexp_sint_t)a)-((sexp_sint_t)b)+SEXP_FIXNUM_TAG))
#define sexp_fx_mul(a, b) ((sexp)((((((sexp_sint_t)a)-SEXP_FIXNUM_TAG)*(((sexp_sint_t)b)>>SEXP_FIXNUM_BITS))+SEXP_FIXNUM_TAG)))
#define sexp_fx_div(a, b) (sexp_make_integer(sexp_unbox_integer(a) / sexp_unbox_integer(b)))
#define sexp_fx_rem(a, b) (sexp_make_integer(sexp_unbox_integer(a) % sexp_unbox_integer(b)))
#define sexp_fx_sign(a)   (-((sexp_sint_t)(a) < 0)) /* -1 or 0 */

#define sexp_fp_add(x,a,b) (sexp_make_flonum(x, sexp_flonum_value(a) + sexp_flonum_value(b)))
#define sexp_fp_sub(x,a,b) (sexp_make_flonum(x, sexp_flonum_value(a) - sexp_flonum_value(b)))
#define sexp_fp_mul(x,a,b) (sexp_make_flonum(x, sexp_flonum_value(a) * sexp_flonum_value(b)))
#define sexp_fp_div(x,a,b) (sexp_make_flonum(x, sexp_flonum_value(a) / sexp_flonum_value(b)))

/****************************** utilities *****************************/

#define sexp_list1(x,a)        sexp_cons((x), (a), SEXP_NULL)
#define sexp_list2(x,a,b)      sexp_cons((x), (a), sexp_cons((x), (b), SEXP_NULL))
#define sexp_list3(x,a,b,c)    sexp_cons((x), (a), sexp_cons((x), (b), sexp_cons((x), (c), SEXP_NULL)))
#define sexp_list4(x,a,b,c,d)  sexp_cons((x), (a), sexp_cons((x), (b), sexp_cons((x), (c), sexp_cons((x), (d), SEXP_NULL))))

#define sexp_push(ctx, ls, x)    ((ls) = sexp_cons((ctx), (x), (ls)))
#define sexp_insert(ctx, ls, x)  ((sexp_memq(NULL, (x), (ls)) != SEXP_FALSE) ? (ls) : sexp_push((ctx), (ls), (x)))

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

#define sexp_read_char(p) (getc(sexp_port_stream(p)))
#define sexp_push_char(c, p) (ungetc(c, sexp_port_stream(p)))
#define sexp_write_char(c, p) (putc(c, sexp_port_stream(p)))
#define sexp_write_string(s, p) (fputs(s, sexp_port_stream(p)))
#define sexp_printf(p, ...) (fprintf(sexp_port_stream(p), __VA_ARGS__))
#define sexp_scanf(p, ...) (fscanf(sexp_port_stream(p), __VA_ARGS__))
#define sexp_flush(p) (fflush(sexp_port_stream(p)))

sexp sexp_alloc_tagged(sexp ctx, size_t size, sexp_uint_t tag);
sexp sexp_cons(sexp ctx, sexp head, sexp tail);
sexp sexp_equalp (sexp ctx, sexp a, sexp b);
sexp sexp_listp(sexp ctx, sexp obj);
sexp sexp_reverse(sexp ctx, sexp ls);
sexp sexp_nreverse(sexp ctx, sexp ls);
sexp sexp_append2(sexp ctx, sexp a, sexp b);
sexp sexp_memq(sexp ctx, sexp x, sexp ls);
sexp sexp_assq(sexp ctx, sexp x, sexp ls);
sexp sexp_length(sexp ctx, sexp ls);
sexp sexp_c_string(sexp ctx, char *str, sexp_sint_t slen);
sexp sexp_make_string(sexp ctx, sexp len, sexp ch);
sexp sexp_substring (sexp ctx, sexp str, sexp start, sexp end);
sexp sexp_make_flonum(sexp ctx, double f);
sexp sexp_intern(sexp ctx, char *str);
sexp sexp_string_to_symbol(sexp ctx, sexp str);
sexp sexp_make_vector(sexp ctx, sexp len, sexp dflt);
sexp sexp_list_to_vector(sexp ctx, sexp ls);
sexp sexp_vector(sexp ctx, int count, ...);
void sexp_write(sexp obj, sexp out);
sexp sexp_read_string(sexp ctx, sexp in);
sexp sexp_read_symbol(sexp ctx, sexp in, int init, int internp);
sexp sexp_read_number(sexp ctx, sexp in, int base);
sexp sexp_read_raw(sexp ctx, sexp in);
sexp sexp_read(sexp ctx, sexp in);
sexp sexp_read_from_string(sexp ctx, char *str);
sexp sexp_make_input_port(sexp ctx, FILE* in, char *path);
sexp sexp_make_output_port(sexp ctx, FILE* out, char *path);
sexp sexp_make_input_string_port(sexp ctx, sexp str);
sexp sexp_make_output_string_port(sexp ctx);
sexp sexp_get_output_string(sexp ctx, sexp port);
sexp sexp_make_exception(sexp ctx, sexp kind, sexp message, sexp irritants, sexp procedure, sexp file, sexp line);
sexp sexp_user_exception (sexp ctx, sexp self, char *message, sexp obj);
sexp sexp_type_exception (sexp ctx, char *message, sexp obj);
sexp sexp_range_exception (sexp ctx, sexp obj, sexp start, sexp end);
sexp sexp_print_exception(sexp ctx, sexp exn, sexp out);
void sexp_init();

#endif /* ! SEXP_H */

