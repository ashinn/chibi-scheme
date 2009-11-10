/*  eval.h -- headers for eval library                   */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifndef SEXP_EVAL_H
#define SEXP_EVAL_H

#include "chibi/sexp.h"

/************************* additional types ***************************/

#define INIT_BCODE_SIZE 128
#define INIT_STACK_SIZE 8192

#define sexp_init_file "init.scm"
#define sexp_config_file "config.scm"

enum core_form_names {
  CORE_DEFINE = 1,
  CORE_SET,
  CORE_LAMBDA,
  CORE_IF,
  CORE_BEGIN,
  CORE_QUOTE,
  CORE_DEFINE_SYNTAX,
  CORE_LET_SYNTAX,
  CORE_LETREC_SYNTAX
};

enum opcode_classes {
  OPC_GENERIC = 1,
  OPC_TYPE_PREDICATE,
  OPC_PREDICATE,
  OPC_ARITHMETIC,
  OPC_ARITHMETIC_INV,
  OPC_ARITHMETIC_CMP,
  OPC_IO,
  OPC_CONSTRUCTOR,
  OPC_ACCESSOR,
  OPC_PARAMETER,
  OPC_FOREIGN,
  OPC_NUM_OP_CLASSES
};

enum opcode_names {
  OP_NOOP,
  OP_RAISE,
  OP_RESUMECC,
  OP_CALLCC,
  OP_APPLY1,
  OP_TAIL_CALL,
  OP_CALL,
  OP_FCALL0,
  OP_FCALL1,
  OP_FCALL2,
  OP_FCALL3,
  OP_FCALL4,
  OP_FCALL5,
  OP_FCALL6,
  OP_EVAL,
  OP_JUMP_UNLESS,
  OP_JUMP,
  OP_PUSH,
  OP_DROP,
  OP_GLOBAL_REF,
  OP_GLOBAL_KNOWN_REF,
  OP_STACK_REF,
  OP_LOCAL_REF,
  OP_LOCAL_SET,
  OP_CLOSURE_REF,
  OP_VECTOR_REF,
  OP_VECTOR_SET,
  OP_VECTOR_LENGTH,
  OP_STRING_REF,
  OP_STRING_SET,
  OP_STRING_LENGTH,
  OP_MAKE_PROCEDURE,
  OP_MAKE_VECTOR,
  OP_AND,
  OP_NULLP,
  OP_INTEGERP,
  OP_SYMBOLP,
  OP_CHARP,
  OP_EOFP,
  OP_TYPEP,
  OP_MAKE,
  OP_SLOT_REF,
  OP_SLOT_SET,
  OP_CAR,
  OP_CDR,
  OP_SET_CAR,
  OP_SET_CDR,
  OP_CONS,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_QUOTIENT,
  OP_REMAINDER,
  OP_NEGATIVE,
  OP_INVERSE,
  OP_LT,
  OP_LE,
  OP_EQN,
  OP_EQ,
  OP_FIX2FLO,
  OP_FLO2FIX,
  OP_CHAR2INT,
  OP_INT2CHAR,
  OP_CHAR_UPCASE,
  OP_CHAR_DOWNCASE,
  OP_DISPLAY,
  OP_WRITE,
  OP_WRITE_CHAR,
  OP_NEWLINE,
  OP_FLUSH_OUTPUT,
  OP_READ,
  OP_READ_CHAR,
  OP_PEEK_CHAR,
  OP_RET,
  OP_DONE,
  OP_NUM_OPCODES
};

/**************************** prototypes ******************************/

SEXP_API void sexp_scheme_init(void);
SEXP_API sexp sexp_apply(sexp context, sexp proc, sexp args);
SEXP_API sexp sexp_eval(sexp context, sexp obj, sexp env);
SEXP_API sexp sexp_eval_string(sexp context, char *str, sexp env);
SEXP_API sexp sexp_load(sexp context, sexp expr, sexp env);
SEXP_API sexp sexp_make_env(sexp context);
SEXP_API sexp sexp_env_copy(sexp context, sexp to, sexp from, sexp ls);
SEXP_API void sexp_env_define(sexp context, sexp env, sexp sym, sexp val);
SEXP_API sexp sexp_make_context(sexp context, sexp stack, sexp env);
SEXP_API void sexp_warn_undefs(sexp ctx, sexp from, sexp to, sexp out);
SEXP_API sexp sexp_make_opcode (sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp_proc0);

#if USE_TYPE_DEFS
SEXP_API sexp sexp_make_type_predicate (sexp ctx, sexp name, sexp type);
SEXP_API sexp sexp_make_constructor (sexp ctx, sexp name, sexp type);
SEXP_API sexp sexp_make_getter (sexp ctx, sexp name, sexp type, sexp index);
SEXP_API sexp sexp_make_setter (sexp ctx, sexp name, sexp type, sexp index);
#endif

#endif /* ! SEXP_EVAL_H */

