/*  eval.h -- headers for eval library                        */
/*  Copyright (c) 2009-2010 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#ifndef SEXP_EVAL_H
#define SEXP_EVAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "chibi/sexp.h"

/************************* additional types ***************************/

#define SEXP_INIT_BCODE_SIZE 128
#define SEXP_INIT_STACK_SIZE 8192

#define sexp_init_file "init.scm"
#define sexp_config_file "config.scm"

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

enum sexp_opcode_names {
  SEXP_OP_NOOP,
  SEXP_OP_RAISE,
  SEXP_OP_RESUMECC,
  SEXP_OP_CALLCC,
  SEXP_OP_APPLY1,
  SEXP_OP_TAIL_CALL,
  SEXP_OP_CALL,
  SEXP_OP_FCALL0,
  SEXP_OP_FCALL1,
  SEXP_OP_FCALL2,
  SEXP_OP_FCALL3,
  SEXP_OP_FCALL4,
  SEXP_OP_FCALLN,
  SEXP_OP_JUMP_UNLESS,
  SEXP_OP_JUMP,
  SEXP_OP_PUSH,
  SEXP_OP_DROP,
  SEXP_OP_GLOBAL_REF,
  SEXP_OP_GLOBAL_KNOWN_REF,
  SEXP_OP_STACK_REF,
  SEXP_OP_LOCAL_REF,
  SEXP_OP_LOCAL_SET,
  SEXP_OP_CLOSURE_REF,
  SEXP_OP_VECTOR_REF,
  SEXP_OP_VECTOR_SET,
  SEXP_OP_VECTOR_LENGTH,
  SEXP_OP_BYTES_REF,
  SEXP_OP_BYTES_SET,
  SEXP_OP_BYTES_LENGTH,
  SEXP_OP_STRING_REF,
  SEXP_OP_STRING_SET,
  SEXP_OP_STRING_LENGTH,
  SEXP_OP_MAKE_PROCEDURE,
  SEXP_OP_MAKE_VECTOR,
  SEXP_OP_MAKE_EXCEPTION,
  SEXP_OP_AND,
  SEXP_OP_NULLP,
  SEXP_OP_FIXNUMP,
  SEXP_OP_SYMBOLP,
  SEXP_OP_CHARP,
  SEXP_OP_EOFP,
  SEXP_OP_TYPEP,
  SEXP_OP_MAKE,
  SEXP_OP_SLOT_REF,
  SEXP_OP_SLOT_SET,
  SEXP_OP_ISA,
  SEXP_OP_SLOTN_REF,
  SEXP_OP_SLOTN_SET,
  SEXP_OP_CAR,
  SEXP_OP_CDR,
  SEXP_OP_SET_CAR,
  SEXP_OP_SET_CDR,
  SEXP_OP_CONS,
  SEXP_OP_ADD,
  SEXP_OP_SUB,
  SEXP_OP_MUL,
  SEXP_OP_DIV,
  SEXP_OP_QUOTIENT,
  SEXP_OP_REMAINDER,
  SEXP_OP_LT,
  SEXP_OP_LE,
  SEXP_OP_EQN,
  SEXP_OP_EQ,
  SEXP_OP_FIX2FLO,
  SEXP_OP_FLO2FIX,
  SEXP_OP_CHAR2INT,
  SEXP_OP_INT2CHAR,
  SEXP_OP_CHAR_UPCASE,
  SEXP_OP_CHAR_DOWNCASE,
  SEXP_OP_WRITE_CHAR,
  SEXP_OP_NEWLINE,
  SEXP_OP_READ_CHAR,
  SEXP_OP_PEEK_CHAR,
  SEXP_OP_YIELD,
  SEXP_OP_RET,
  SEXP_OP_DONE,
  SEXP_OP_NUM_OPCODES
};

/**************************** prototypes ******************************/

SEXP_API void sexp_scheme_init (void);
SEXP_API sexp sexp_make_eval_context (sexp context, sexp stack, sexp env, sexp_uint_t size);
SEXP_API sexp sexp_make_child_context (sexp context, sexp lambda);
SEXP_API sexp sexp_compile_error (sexp ctx, const char *message, sexp obj);
SEXP_API sexp sexp_analyze (sexp context, sexp x);
SEXP_API void sexp_stack_trace (sexp ctx, sexp out);
SEXP_API sexp sexp_apply (sexp context, sexp proc, sexp args);
SEXP_API sexp sexp_apply1 (sexp ctx, sexp f, sexp x);
SEXP_API sexp sexp_free_vars (sexp context, sexp x, sexp fv);
SEXP_API int sexp_param_index (sexp lambda, sexp name);
SEXP_API sexp sexp_eval_op (sexp context sexp_api_params(self, n), sexp obj, sexp env);
SEXP_API sexp sexp_eval_string (sexp context, const char *str, sexp_sint_t len, sexp env);
SEXP_API sexp sexp_load_op (sexp context sexp_api_params(self, n), sexp expr, sexp env);
SEXP_API sexp sexp_make_env_op (sexp context sexp_api_params(self, n));
SEXP_API sexp sexp_make_null_env_op (sexp context sexp_api_params(self, n), sexp version);
SEXP_API sexp sexp_make_primitive_env (sexp context, sexp version);
SEXP_API sexp sexp_make_standard_env_op (sexp context sexp_api_params(self, n), sexp version);
SEXP_API sexp sexp_load_standard_parameters (sexp context, sexp env);
SEXP_API sexp sexp_load_standard_env (sexp context, sexp env, sexp version);
SEXP_API sexp sexp_find_module_file (sexp ctx, const char *file);
SEXP_API sexp sexp_load_module_file (sexp ctx, const char *file, sexp env);
SEXP_API sexp sexp_add_module_directory_op (sexp ctx sexp_api_params(self, n), sexp dir, sexp appendp);
SEXP_API sexp sexp_extend_env (sexp context, sexp env, sexp vars, sexp value);
SEXP_API sexp sexp_env_copy_op (sexp context sexp_api_params(self, n), sexp to, sexp from, sexp ls, sexp immutp);
SEXP_API sexp sexp_env_define (sexp context, sexp env, sexp sym, sexp val);
SEXP_API sexp sexp_env_cell (sexp env, sexp sym);
SEXP_API sexp sexp_env_ref (sexp env, sexp sym, sexp dflt);
SEXP_API sexp sexp_env_global_ref (sexp env, sexp sym, sexp dflt);
SEXP_API void sexp_warn_undefs (sexp ctx, sexp from, sexp to);
SEXP_API sexp sexp_make_opcode (sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp_proc1);
SEXP_API sexp sexp_make_procedure_op (sexp ctx sexp_api_params(self, n), sexp flags, sexp num_args, sexp bc, sexp vars);
SEXP_API sexp sexp_make_foreign (sexp ctx, const char *name, int num_args, int flags, sexp_proc1 f, sexp data);
SEXP_API sexp sexp_define_foreign_aux (sexp ctx, sexp env, const char *name, int num_args, int flags, sexp_proc1 f, sexp data);

#define sexp_define_foreign(c,e,s,n,f) sexp_define_foreign_aux(c,e,s,n,0,(sexp_proc1)f,NULL)
#define sexp_define_foreign_opt(c,e,s,n,f,d) sexp_define_foreign_aux(c,e,s,n,1,(sexp_proc1)f,d)

SEXP_API sexp sexp_define_foreign_param (sexp ctx, sexp env, const char *name, int num_args, sexp_proc1 f, const char *param);

#define sexp_env_next_cell(x) sexp_pair_source(x)
#define sexp_env_push(ctx, env, tmp, name, value) (tmp=sexp_cons(ctx,name,value), sexp_env_next_cell(tmp)=sexp_env_bindings(env), sexp_env_bindings(env)=tmp)

#if SEXP_USE_TYPE_DEFS
SEXP_API sexp sexp_make_type_predicate_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type);
SEXP_API sexp sexp_make_constructor_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type);
SEXP_API sexp sexp_make_getter_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type, sexp index);
SEXP_API sexp sexp_make_setter_op (sexp ctx sexp_api_params(self, n), sexp name, sexp type, sexp index);
#endif

/* simplify primitive API interface */
#define sexp_make_synclo(ctx, a, b, c) sexp_make_synclo_op(ctx sexp_api_pass(NULL, 3) a, b, c)
#define sexp_make_procedure(ctx, f, n, b, v) sexp_make_procedure_op(ctx sexp_api_pass(NULL, 4), f, n, b, v)
#define sexp_make_env(ctx) sexp_make_env_op(ctx sexp_api_pass(NULL, 0))
#define sexp_make_null_env(ctx, v) sexp_make_null_env_op(ctx sexp_api_pass(NULL, 0), v)
#define sexp_make_standard_env(ctx) sexp_make_standard_env_op(ctx sexp_api_pass(NULL, 0))
#define sexp_add_module_directory(ctx, d, a) sexp_add_module_directory_op(ctx sexp_api_pass(NULL, 1), d, a)
#define sexp_eval(ctx, x, e) sexp_eval_op(ctx sexp_api_pass(NULL, 2), x, e)
#define sexp_load(ctx, f, e) sexp_load_op(ctx sexp_api_pass(NULL, 2), f, e)
#define sexp_env_copy(ctx, a, b, c, d) sexp_env_copy_op(ctx sexp_api_pass(NULL, 4), a, b, c, d)
#define sexp_identifierp(ctx, x) sexp_identifier_op(ctx sexp_api_pass(NULL, 1), x)
#define sexp_identifier_to_symbol(ctx, x) sexp_syntactic_closure_expr(ctx sexp_api_pass(NULL, 1), x)
#define sexp_identifier_eq(ctx, a, b, c, d) sexp_identifier_eq_op(ctx sexp_api_pass(NULL, 4), a, b, c, d)
#define sexp_open_input_file(ctx, x) sexp_open_input_file_op(ctx sexp_api_pass(NULL, 1), x)
#define sexp_open_output_file(ctx, x) sexp_open_output_file_op(ctx sexp_api_pass(NULL, 1), x)
#define sexp_close_port(ctx, x) sexp_close_port_op(ctx sexp_api_pass(NULL, 1), x)

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* ! SEXP_EVAL_H */

