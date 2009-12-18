/*  eval.h -- headers for eval library                   */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifndef SEXP_EVAL_H
#define SEXP_EVAL_H

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
  SEXP_OPC_ARITHMETIC_INV,
  SEXP_OPC_ARITHMETIC_CMP,
  SEXP_OPC_IO,
  SEXP_OPC_CONSTRUCTOR,
  SEXP_OPC_ACCESSOR,
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
  SEXP_OP_FCALL5,
  SEXP_OP_FCALL6,
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
  SEXP_OP_STRING_REF,
  SEXP_OP_STRING_SET,
  SEXP_OP_STRING_LENGTH,
  SEXP_OP_MAKE_PROCEDURE,
  SEXP_OP_MAKE_VECTOR,
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
  SEXP_OP_NEGATIVE,
  SEXP_OP_INVERSE,
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
  SEXP_OP_RET,
  SEXP_OP_DONE,
  SEXP_OP_NUM_OPCODES
};

/**************************** prototypes ******************************/

SEXP_API void sexp_scheme_init (void);
SEXP_API sexp sexp_make_eval_context (sexp context, sexp stack, sexp env);
SEXP_API sexp sexp_make_child_context (sexp context, sexp lambda);
SEXP_API sexp sexp_analyze (sexp context, sexp x);
SEXP_API sexp sexp_apply (sexp context, sexp proc, sexp args);
SEXP_API sexp sexp_eval (sexp context, sexp obj, sexp env);
SEXP_API sexp sexp_eval_string (sexp context, char *str, sexp env);
SEXP_API sexp sexp_load (sexp context, sexp expr, sexp env);
SEXP_API sexp sexp_make_env (sexp context);
SEXP_API sexp sexp_make_null_env (sexp context, sexp version);
SEXP_API sexp sexp_make_primitive_env (sexp context, sexp version);
SEXP_API sexp sexp_make_standard_env (sexp context, sexp version);
SEXP_API sexp sexp_load_standard_env (sexp context, sexp env, sexp version);
SEXP_API sexp sexp_find_module_file (sexp ctx, char *file);
SEXP_API sexp sexp_load_module_file (sexp ctx, char *file, sexp env);
SEXP_API sexp sexp_add_module_directory (sexp ctx, sexp dir, sexp appendp);
SEXP_API sexp sexp_extend_env (sexp context, sexp env, sexp vars, sexp value);
SEXP_API sexp sexp_env_copy (sexp context, sexp to, sexp from, sexp ls);
SEXP_API void sexp_env_define (sexp context, sexp env, sexp sym, sexp val);
SEXP_API sexp sexp_env_cell (sexp env, sexp sym);
SEXP_API sexp sexp_env_global_ref (sexp env, sexp sym, sexp dflt);
SEXP_API void sexp_warn_undefs (sexp ctx, sexp from, sexp to, sexp out);
SEXP_API sexp sexp_make_opcode (sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp_proc1);
SEXP_API sexp sexp_make_foreign (sexp ctx, char *name, int num_args, int flags, sexp_proc1 f, sexp data);
SEXP_API sexp sexp_define_foreign_aux (sexp ctx, sexp env, char *name, int num_args, int flags, sexp_proc1 f, sexp data);

#define sexp_define_foreign(c,e,s,n,f) sexp_define_foreign_aux(c,e,s,n,0,(sexp_proc1)f,NULL)
#define sexp_define_foreign_opt(c,e,s,n,f,d) sexp_define_foreign_aux(c,e,s,n,1,(sexp_proc1)f,d)

#if SEXP_USE_TYPE_DEFS
SEXP_API sexp sexp_make_type_predicate (sexp ctx, sexp name, sexp type);
SEXP_API sexp sexp_make_constructor (sexp ctx, sexp name, sexp type);
SEXP_API sexp sexp_make_getter (sexp ctx, sexp name, sexp type, sexp index);
SEXP_API sexp sexp_make_setter (sexp ctx, sexp name, sexp type, sexp index);
#endif

#endif /* ! SEXP_EVAL_H */

