/*  eval.h -- headers for eval library                        */
/*  Copyright (c) 2009-2011 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#ifndef SEXP_EVAL_H
#define SEXP_EVAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "chibi/sexp.h"

/************************* additional types ***************************/

#define sexp_init_file "init-"
#define sexp_init_file_suffix ".scm"
#define sexp_meta_file "meta.scm"
#define sexp_leap_seconds_file "leap.txt"

enum sexp_core_form_names {
  SEXP_CORE_DEFINE = 1,
  SEXP_CORE_SET,
  SEXP_CORE_LAMBDA,
  SEXP_CORE_IF,
  SEXP_CORE_BEGIN,
  SEXP_CORE_QUOTE,
  SEXP_CORE_SYNTAX_QUOTE,
  SEXP_CORE_DEFINE_SYNTAX,
  SEXP_CORE_LET_SYNTAX,
  SEXP_CORE_LETREC_SYNTAX
};

enum sexp_opcode_classes {
  SEXP_OPC_GENERIC = 1,
  SEXP_OPC_TYPE_PREDICATE,
  SEXP_OPC_PREDICATE,
  SEXP_OPC_ARITHMETIC,
  SEXP_OPC_ARITHMETIC_CMP,
  SEXP_OPC_IO,
  SEXP_OPC_CONSTRUCTOR,
  SEXP_OPC_GETTER,
  SEXP_OPC_SETTER,
  SEXP_OPC_PARAMETER,
  SEXP_OPC_FOREIGN,
  SEXP_OPC_NUM_OP_CLASSES
};

/**************************** prototypes ******************************/

SEXP_API void sexp_scheme_init (void);
SEXP_API sexp sexp_make_eval_context (sexp context, sexp stack, sexp env, sexp_uint_t size, sexp_uint_t max_size);
SEXP_API sexp sexp_make_child_context (sexp context, sexp lambda);
SEXP_API sexp sexp_compile_error (sexp ctx, const char *message, sexp obj);
SEXP_API sexp sexp_analyze (sexp context, sexp x);
SEXP_API void sexp_stack_trace (sexp ctx, sexp out);
SEXP_API sexp sexp_free_vars (sexp context, sexp x, sexp fv);
SEXP_API int sexp_param_index (sexp lambda, sexp name);
SEXP_API sexp sexp_compile_op (sexp context, sexp self, sexp_sint_t n, sexp obj, sexp env);
SEXP_API sexp sexp_eval_op (sexp context, sexp self, sexp_sint_t n, sexp obj, sexp env);
SEXP_API sexp sexp_eval_string (sexp context, const char *str, sexp_sint_t len, sexp env);
SEXP_API sexp sexp_load_op (sexp context, sexp self, sexp_sint_t n, sexp expr, sexp env);
SEXP_API sexp sexp_make_env_op (sexp context, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_make_null_env_op (sexp context, sexp self, sexp_sint_t n, sexp version);
SEXP_API sexp sexp_make_primitive_env (sexp context, sexp version);
SEXP_API sexp sexp_make_standard_env_op (sexp context, sexp self, sexp_sint_t n, sexp version);
SEXP_API void sexp_set_parameter (sexp ctx, sexp env, sexp name, sexp value);
SEXP_API sexp sexp_load_standard_ports (sexp context, sexp env, FILE* in, FILE* out, FILE* err, int no_close);
SEXP_API sexp sexp_load_standard_env (sexp context, sexp env, sexp version);
SEXP_API sexp sexp_find_module_file (sexp ctx, const char *file);
SEXP_API sexp sexp_load_module_file (sexp ctx, const char *file, sexp env);
SEXP_API sexp sexp_add_module_directory_op (sexp ctx, sexp self, sexp_sint_t n, sexp dir, sexp appendp);
SEXP_API sexp sexp_extend_env (sexp ctx, sexp env, sexp vars, sexp value);
SEXP_API sexp sexp_env_import_op (sexp ctx, sexp self, sexp_sint_t n, sexp to, sexp from, sexp ls, sexp immutp);
SEXP_API sexp sexp_identifier_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_syntactic_closure_expr(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_identifier_eq_op(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b, sexp c, sexp d);
SEXP_API sexp sexp_open_input_file_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_open_output_file_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_close_port_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_env_define (sexp ctx, sexp env, sexp sym, sexp val);
SEXP_API sexp sexp_env_cell (sexp env, sexp sym, int localp);
SEXP_API sexp sexp_env_ref (sexp env, sexp sym, sexp dflt);
SEXP_API sexp sexp_parameter_ref (sexp ctx, sexp param);
SEXP_API void sexp_warn_undefs (sexp ctx, sexp from, sexp to);
SEXP_API sexp sexp_make_lit (sexp ctx, sexp value);
SEXP_API sexp sexp_make_opcode (sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp_proc1);
SEXP_API sexp sexp_make_procedure_op (sexp ctx, sexp self, sexp_sint_t n, sexp flags, sexp num_args, sexp bc, sexp vars);
SEXP_API sexp sexp_define_foreign_aux (sexp ctx, sexp env, const char *name, int num_args, int flags, sexp_proc1 f, sexp data);
#if SEXP_USE_GREEN_THREADS
SEXP_API sexp sexp_dk (sexp ctx, sexp self, sexp_uint_t n, sexp val);
#endif
#if SEXP_USE_UTF8_STRINGS
SEXP_API int sexp_utf8_initial_byte_count (int c);
SEXP_API int sexp_utf8_char_byte_count (int c);
SEXP_API int sexp_string_utf8_length (unsigned char *p, int len);
SEXP_API char* sexp_string_utf8_prev (unsigned char *p);
SEXP_API sexp sexp_string_utf8_ref (sexp ctx, sexp str, sexp i);
#endif

#if SEXP_USE_NATIVE_X86
SEXP_API sexp sexp_write_char_op(sexp ctx, sexp self, sexp_sint_t n, sexp ch, sexp out);
SEXP_API sexp sexp_newline_op(sexp ctx, sexp self, sexp_sint_t n, sexp out);
SEXP_API sexp sexp_read_char_op(sexp ctx, sexp self, sexp_sint_t n, sexp in);
SEXP_API sexp sexp_peek_char_op(sexp ctx, sexp self, sexp_sint_t n, sexp in);
SEXP_API sexp sexp_exact_to_inexact(sexp ctx, sexp self, sexp_sint_t n, sexp i);
SEXP_API sexp sexp_inexact_to_exact(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_char_upcase(sexp ctx, sexp self, sexp_sint_t n, sexp ch);
SEXP_API sexp sexp_char_downcase(sexp ctx, sexp self, sexp_sint_t n, sexp ch);
#endif

#define sexp_define_foreign(c,e,s,n,f) sexp_define_foreign_aux(c,e,s,n,0,(sexp_proc1)f,NULL)
#define sexp_define_foreign_opt(c,e,s,n,f,d) sexp_define_foreign_aux(c,e,s,n,1,(sexp_proc1)f,d)

SEXP_API sexp sexp_define_foreign_param (sexp ctx, sexp env, const char *name, int num_args, sexp_proc1 f, const char *param);

#define sexp_env_key(x) sexp_car(x)
#define sexp_env_value(x) sexp_cdr(x)
#define sexp_env_next_cell(x) sexp_pair_source(x)
#define sexp_env_push(ctx, env, tmp, name, value) (tmp=sexp_cons(ctx,name,value), sexp_env_next_cell(tmp)=sexp_env_bindings(env), sexp_env_bindings(env)=tmp)
#define sexp_env_push_rename(ctx, env, tmp, name, value) (tmp=sexp_cons(ctx,name,value), sexp_env_next_cell(tmp)=sexp_env_renames(env), sexp_env_renames(env)=tmp)

#if SEXP_USE_TYPE_DEFS
SEXP_API sexp sexp_make_type_predicate_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp type);
SEXP_API sexp sexp_make_constructor_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp type);
SEXP_API sexp sexp_make_getter_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp type, sexp index);
SEXP_API sexp sexp_make_setter_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp type, sexp index);
#endif

/* simplify primitive API interface */
#define sexp_make_synclo(ctx, a, b, c) sexp_make_synclo_op(ctx, NULL, 3, a, b, c)
#define sexp_make_procedure(ctx, f, n, b, v) sexp_make_procedure_op(ctx, NULL, 4, f, n, b, v)
#define sexp_make_env(ctx) sexp_make_env_op(ctx, NULL, 0)
#define sexp_make_null_env(ctx, v) sexp_make_null_env_op(ctx, NULL, 0, v)
#define sexp_make_standard_env(ctx) sexp_make_standard_env_op(ctx, NULL, 0)
#define sexp_add_module_directory(ctx, d, a) sexp_add_module_directory_op(ctx, NULL, 1, d, a)
#define sexp_eval(ctx, x, e) sexp_eval_op(ctx, NULL, 2, x, e)
#define sexp_load(ctx, f, e) sexp_load_op(ctx, NULL, 2, f, e)
#define sexp_env_import(ctx, a, b, c, d) sexp_env_import_op(ctx, NULL, 4, a, b, c, d)
#define sexp_identifierp(ctx, x) sexp_identifierp_op(ctx, NULL, 1, x)
#define sexp_identifier_to_symbol(ctx, x) sexp_syntactic_closure_expr(ctx, NULL, 1, x)
#define sexp_identifier_eq(ctx, a, b, c, d) sexp_identifier_eq_op(ctx, NULL, 4, a, b, c, d)
#define sexp_open_input_file(ctx, x) sexp_open_input_file_op(ctx, NULL, 1, x)
#define sexp_open_output_file(ctx, x) sexp_open_output_file_op(ctx, NULL, 1, x)
#define sexp_close_port(ctx, x) sexp_close_port_op(ctx, NULL, 1, x)

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* ! SEXP_EVAL_H */
