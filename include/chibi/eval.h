/*  eval.h -- headers for eval library                        */
/*  Copyright (c) 2009-2015 Alex Shinn.  All rights reserved. */
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
#define sexp_meta_file "meta-7.scm"
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

#if SEXP_USE_DEBUG_VM || SEXP_USE_PROFILE_VM || SEXP_USE_STATIC_LIBS
SEXP_API const char** sexp_opcode_names;
#endif

/**************************** prototypes ******************************/

SEXP_API void sexp_warn (sexp ctx, const char *msg, sexp x);
SEXP_API void sexp_scheme_init (void);
SEXP_API sexp sexp_make_eval_context (sexp context, sexp stack, sexp env, sexp_uint_t size, sexp_uint_t max_size);
SEXP_API sexp sexp_make_child_context (sexp context, sexp lambda);
SEXP_API sexp sexp_compile_error (sexp ctx, const char *message, sexp obj);
SEXP_API sexp sexp_maybe_wrap_error (sexp ctx, sexp obj);
SEXP_API sexp sexp_analyze (sexp context, sexp x);
SEXP_API sexp sexp_simplify (sexp ctx, sexp self, sexp_sint_t n, sexp ast);
SEXP_API sexp sexp_make_lambda (sexp ctx, sexp params);
SEXP_API sexp sexp_make_ref (sexp ctx, sexp name, sexp cell);
SEXP_API void sexp_generate (sexp ctx, sexp name, sexp loc, sexp lam, sexp x);
SEXP_API void sexp_emit (sexp ctx, unsigned char c);
SEXP_API void sexp_emit_return (sexp ctx);
#if SEXP_USE_NATIVE_X86
SEXP_API void sexp_emit_enter (sexp ctx);
SEXP_API void sexp_bless_bytecode (sexp ctx, sexp bc);
#else
#define sexp_emit_enter(ctx)
#define sexp_bless_bytecode(ctx, bc)
#endif
SEXP_API sexp sexp_complete_bytecode (sexp ctx);
SEXP_API void sexp_shrink_bcode (sexp ctx, sexp_uint_t i);
SEXP_API void sexp_expand_bcode (sexp ctx, sexp_sint_t size);
SEXP_API void sexp_stack_trace (sexp ctx, sexp out);
SEXP_API sexp sexp_free_vars (sexp context, sexp x, sexp fv);
SEXP_API int sexp_param_index (sexp ctx, sexp lambda, sexp name);
SEXP_API sexp sexp_compile_op (sexp context, sexp self, sexp_sint_t n, sexp obj, sexp env);
SEXP_API sexp sexp_generate_op (sexp context, sexp self, sexp_sint_t n, sexp obj, sexp env);
SEXP_API sexp sexp_eval_op (sexp context, sexp self, sexp_sint_t n, sexp obj, sexp env);
SEXP_API sexp sexp_eval_string (sexp context, const char *str, sexp_sint_t len, sexp env);
SEXP_API sexp sexp_load_op (sexp context, sexp self, sexp_sint_t n, sexp expr, sexp env);
SEXP_API sexp sexp_exception_type_op (sexp ctx, sexp self, sexp_sint_t n, sexp exn);
SEXP_API sexp sexp_make_env_op (sexp context, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_make_null_env_op (sexp context, sexp self, sexp_sint_t n, sexp version);
SEXP_API sexp sexp_env_cell_define (sexp ctx, sexp env, sexp name, sexp value, sexp* varenv);
SEXP_API sexp sexp_make_primitive_env_op (sexp context, sexp self, sexp_sint_t n, sexp version);
SEXP_API sexp sexp_make_standard_env_op (sexp context, sexp self, sexp_sint_t n, sexp version);
SEXP_API void sexp_set_parameter (sexp ctx, sexp env, sexp name, sexp value);
SEXP_API sexp sexp_load_standard_ports (sexp context, sexp env, FILE* in, FILE* out, FILE* err, int no_close);
SEXP_API sexp sexp_load_standard_env (sexp context, sexp env, sexp version);
SEXP_API char* sexp_find_module_file_raw (sexp ctx, const char *file);
SEXP_API sexp sexp_find_module_file (sexp ctx, const char *file);
SEXP_API sexp sexp_load_module_file (sexp ctx, const char *file, sexp env);
SEXP_API sexp sexp_current_module_path_op (sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_find_module_file_op (sexp ctx, sexp self, sexp_sint_t n, sexp file);
SEXP_API sexp sexp_load_module_file_op (sexp ctx, sexp self, sexp_sint_t n, sexp file, sexp env);
SEXP_API sexp sexp_add_module_directory_op (sexp ctx, sexp self, sexp_sint_t n, sexp dir, sexp appendp);
SEXP_API sexp sexp_current_environment (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_set_current_environment (sexp ctx, sexp self, sexp_sint_t n, sexp env);
SEXP_API sexp sexp_meta_environment (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_extend_env (sexp ctx, sexp env, sexp vars, sexp value);
SEXP_API sexp sexp_env_import_op (sexp ctx, sexp self, sexp_sint_t n, sexp to, sexp from, sexp ls, sexp immutp);
SEXP_API sexp sexp_env_exports_op (sexp ctx, sexp self, sexp_sint_t n, sexp env);
SEXP_API sexp sexp_identifierp_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_identifier_eq_op(sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b, sexp c, sexp d);
SEXP_API sexp sexp_make_synclo_op(sexp ctx, sexp self, sexp_sint_t n, sexp env, sexp fv, sexp expr);
SEXP_API sexp sexp_strip_synclos(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_syntactic_closure_expr_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_open_input_file_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_open_output_file_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_open_binary_input_file(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_open_binary_output_file(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_close_port_op(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_set_port_line_op (sexp ctx, sexp self, sexp_sint_t n, sexp port, sexp line);
SEXP_API sexp sexp_env_define (sexp ctx, sexp env, sexp sym, sexp val);
SEXP_API sexp sexp_env_cell (sexp ctx, sexp env, sexp sym, int localp);
SEXP_API sexp sexp_env_ref (sexp ctx, sexp env, sexp sym, sexp dflt);
SEXP_API sexp sexp_env_parent_op (sexp ctx, sexp self, sexp_sint_t n, sexp env);
SEXP_API sexp sexp_parameter_ref (sexp ctx, sexp param);
#if SEXP_USE_RENAME_BINDINGS
SEXP_API sexp sexp_env_rename (sexp ctx, sexp env, sexp key, sexp value);
#endif
SEXP_API sexp sexp_warn_undefs_op (sexp ctx, sexp self, sexp_sint_t n, sexp from, sexp to, sexp res);
SEXP_API sexp sexp_make_lit (sexp ctx, sexp value);
SEXP_API sexp sexp_make_opcode (sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp_proc1);
SEXP_API sexp sexp_make_procedure_op (sexp ctx, sexp self, sexp_sint_t n, sexp flags, sexp num_args, sexp bc, sexp vars);
SEXP_API sexp sexp_define_foreign_aux (sexp ctx, sexp env, const char *name, int num_args, int flags, const char *fname, sexp_proc1 f, sexp data);
SEXP_API sexp sexp_register_optimization(sexp ctx, sexp self, sexp_sint_t n, sexp f, sexp i);
#if SEXP_USE_AUTO_FORCE
SEXP_API sexp sexp_make_promise (sexp ctx, sexp self, sexp_sint_t n, sexp done, sexp val);
#endif
#if SEXP_USE_UTF8_STRINGS
SEXP_API sexp sexp_read_utf8_char (sexp ctx, sexp port, int i);
SEXP_API void sexp_push_utf8_char (sexp ctx, int i, sexp port);
SEXP_API void sexp_string_utf8_set (sexp ctx, sexp str, sexp index, sexp ch);
SEXP_API sexp sexp_string_utf8_index_ref (sexp ctx, sexp self, sexp_sint_t n, sexp str, sexp i);
SEXP_API sexp sexp_string_utf8_index_set (sexp ctx, sexp self, sexp_sint_t n, sexp str, sexp i, sexp ch);
#endif
#if SEXP_USE_GREEN_THREADS
SEXP_API sexp sexp_dk (sexp ctx, sexp self, sexp_sint_t n, sexp val);
#endif
SEXP_API sexp sexp_thread_parameters (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_thread_parameters_set (sexp ctx, sexp self, sexp_sint_t n, sexp val);
SEXP_API sexp sexp_string_cmp_op (sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b, sexp ci);
#if SEXP_USE_RATIOS
SEXP_API sexp sexp_ratio_numerator_op (sexp ctx, sexp self, sexp_sint_t n, sexp rat);
SEXP_API sexp sexp_ratio_denominator_op (sexp ctx, sexp self, sexp_sint_t n, sexp rat);
#endif
#if SEXP_USE_COMPLEX
SEXP_API sexp sexp_complex_real_op (sexp ctx, sexp self, sexp_sint_t n, sexp rat);
SEXP_API sexp sexp_complex_imag_op (sexp ctx, sexp self, sexp_sint_t n, sexp rat);
#endif
#if SEXP_USE_PROFILE_VM
SEXP_API sexp sexp_reset_vm_profile (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_print_vm_profile (sexp ctx, sexp self, sexp_sint_t n);
#endif

#if SEXP_USE_MATH
SEXP_API sexp sexp_exp(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_log(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_sin(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_cos(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_tan(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_asin(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_acos(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_atan(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_sqrt(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_exact_sqrt(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_inexact_sqrt(sexp ctx, sexp self, sexp_sint_t n, sexp z);
SEXP_API sexp sexp_round(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_trunc(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_floor(sexp ctx, sexp self, sexp_sint_t n, sexp x);
SEXP_API sexp sexp_ceiling(sexp ctx, sexp self, sexp_sint_t n, sexp x);
#endif
SEXP_API sexp sexp_expt_op(sexp ctx, sexp self, sexp_sint_t n, sexp z1, sexp z2);
SEXP_API sexp sexp_exact_to_inexact(sexp ctx, sexp self, sexp_sint_t n, sexp i);
SEXP_API sexp sexp_inexact_to_exact(sexp ctx, sexp self, sexp_sint_t n, sexp x);

#if SEXP_USE_NATIVE_X86
SEXP_API sexp sexp_write_char_op(sexp ctx, sexp self, sexp_sint_t n, sexp ch, sexp out);
SEXP_API sexp sexp_newline_op(sexp ctx, sexp self, sexp_sint_t n, sexp out);
SEXP_API sexp sexp_read_char_op(sexp ctx, sexp self, sexp_sint_t n, sexp in);
SEXP_API sexp sexp_peek_char_op(sexp ctx, sexp self, sexp_sint_t n, sexp in);
SEXP_API sexp sexp_char_upcase(sexp ctx, sexp self, sexp_sint_t n, sexp ch);
SEXP_API sexp sexp_char_downcase(sexp ctx, sexp self, sexp_sint_t n, sexp ch);
#endif

SEXP_API sexp sexp_define_foreign_param_aux(sexp ctx, sexp env, const char *name, int num_args, const char *fname, sexp_proc1 f, const char *param);

#define sexp_define_foreign(c,e,s,n,f) sexp_define_foreign_aux(c,e,s,n,0,(const char*)#f,(sexp_proc1)f,NULL)
#define sexp_define_foreign_param(c,e,s,n,f,p) sexp_define_foreign_param_aux(c,e,s,n,(const char*)#f,(sexp_proc1)f,p)
#define sexp_define_foreign_opt(c,e,s,n,f,p) sexp_define_foreign_aux(c,e,s,n,1,(const char*)#f,(sexp_proc1)f,p)

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
SEXP_API sexp sexp_type_slot_offset_op (sexp ctx, sexp self, sexp_sint_t n, sexp type, sexp index);
#endif

#ifdef PLAN9
SEXP_API sexp sexp_rand (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_srand (sexp ctx, sexp self, sexp_sint_t n, sexp seed);
SEXP_API sexp sexp_file_exists_p (sexp ctx, sexp self, sexp_sint_t n, sexp path);
SEXP_API sexp sexp_fdopen (sexp ctx, sexp self, sexp_sint_t n, sexp fd, sexp mode);
SEXP_API sexp sexp_fileno (sexp ctx, sexp self, sexp_sint_t n, sexp port);
SEXP_API sexp sexp_fork (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_exec (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp args);
SEXP_API void sexp_exits (sexp ctx, sexp self, sexp_sint_t n, sexp msg);
SEXP_API sexp sexp_dup (sexp ctx, sexp self, sexp_sint_t n, sexp oldfd, sexp newfd);
SEXP_API sexp sexp_pipe (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_sleep (sexp ctx, sexp self, sexp_sint_t n, sexp msecs);
SEXP_API sexp sexp_getenv (sexp ctx, sexp self, sexp_sint_t n, sexp name);
SEXP_API sexp sexp_getwd (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_chdir (sexp ctx, sexp self, sexp_sint_t n, sexp path);
SEXP_API sexp sexp_getuser (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_sysname (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_wait (sexp ctx, sexp self, sexp_sint_t n);
SEXP_API sexp sexp_postnote (sexp ctx, sexp self, sexp_sint_t n, sexp pid, sexp note);
SEXP_API sexp sexp_postmountsrv (sexp ctx, sexp self, sexp_sint_t n, sexp ls, sexp name, sexp mtpt, sexp flags);
SEXP_API sexp sexp_9p_req_offset (sexp ctx, sexp self, sexp_sint_t n, sexp req);
SEXP_API sexp sexp_9p_req_count (sexp ctx, sexp self, sexp_sint_t n, sexp req);
SEXP_API sexp sexp_9p_req_fid (sexp ctx, sexp self, sexp_sint_t n, sexp req);
SEXP_API sexp sexp_9p_req_newfid (sexp ctx, sexp self, sexp_sint_t n, sexp req);
SEXP_API sexp sexp_9p_respond (sexp ctx, sexp self, sexp_sint_t n, sexp req, sexp err);
SEXP_API sexp sexp_9p_responderror (sexp ctx, sexp self, sexp_sint_t n, sexp req);
#else
SEXP_API sexp sexp_get_port_fileno (sexp ctx, sexp self, sexp_sint_t n, sexp port);
SEXP_API sexp sexp_stream_portp_op (sexp ctx, sexp self, sexp_sint_t n, sexp port);
#endif

#if SEXP_USE_SIMPLIFY
SEXP_API int sexp_rest_unused_p (sexp lambda);
#else
#define sexp_rest_unused_p(lambda) 0
#endif

/* simplify primitive API interface */
#define sexp_make_synclo(ctx, a, b, c) sexp_make_synclo_op(ctx, NULL, 3, a, b, c)
#define sexp_make_procedure(ctx, f, n, b, v) sexp_make_procedure_op(ctx, NULL, 4, f, n, b, v)
#define sexp_make_env(ctx) sexp_make_env_op(ctx, NULL, 0)
#define sexp_make_null_env(ctx, v) sexp_make_null_env_op(ctx, NULL, 0, v)
#define sexp_make_primitive_env(ctx, v) sexp_make_primitive_env_op(ctx, NULL, 1, v)
#define sexp_make_standard_env(ctx, v) sexp_make_standard_env_op(ctx, NULL, 1, v)
#define sexp_add_module_directory(ctx, d, a) sexp_add_module_directory_op(ctx, NULL, 1, d, a)
#define sexp_eval(ctx, x, e) sexp_eval_op(ctx, NULL, 2, x, e)
#define sexp_load(ctx, f, e) sexp_load_op(ctx, NULL, 2, f, e)
#define sexp_env_import(ctx, a, b, c, d) sexp_env_import_op(ctx, NULL, 4, a, b, c, d)
#define sexp_identifierp(ctx, x) sexp_identifierp_op(ctx, NULL, 1, x)
#define sexp_identifier_to_symbol(ctx, x) sexp_syntactic_closure_expr_op(ctx, NULL, 1, x)
#define sexp_identifier_eq(ctx, a, b, c, d) sexp_identifier_eq_op(ctx, NULL, 4, a, b, c, d)
#define sexp_open_input_file(ctx, x) sexp_open_input_file_op(ctx, NULL, 1, x)
#define sexp_open_output_file(ctx, x) sexp_open_output_file_op(ctx, NULL, 1, x)
#define sexp_close_port(ctx, x) sexp_close_port_op(ctx, NULL, 1, x)
#define sexp_warn_undefs(ctx, from, to, res) sexp_warn_undefs_op(ctx, NULL, 3, from, to, res)
#define sexp_string_cmp(ctx, a, b, c) sexp_string_cmp_op(ctx, NULL, 3, a, b, c)

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* ! SEXP_EVAL_H */
