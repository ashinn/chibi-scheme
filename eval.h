/*  eval.h -- headers for eval library                   */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifndef SEXP_EVAL_H
#define SEXP_EVAL_H

#include "sexp.h"

/************************* additional types ***************************/

#define INIT_BCODE_SIZE 128
#define INIT_STACK_SIZE 1024

#define sexp_init_file "init.scm"

#define sexp_debug(msg, obj, ctx) (sexp_write_string(msg,env_global_ref(sexp_context_env(ctx), the_cur_err_symbol, SEXP_FALSE)), sexp_write(obj, env_global_ref(sexp_context_env(ctx), the_cur_err_symbol, SEXP_FALSE)), sexp_write_char('\n',env_global_ref(sexp_context_env(ctx), the_cur_err_symbol, SEXP_FALSE)))

/* procedure types */
typedef sexp (*sexp_proc0) ();
typedef sexp (*sexp_proc1) (sexp);
typedef sexp (*sexp_proc2) (sexp, sexp);
typedef sexp (*sexp_proc3) (sexp, sexp, sexp);
typedef sexp (*sexp_proc4) (sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc5) (sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc6) (sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc7) (sexp, sexp, sexp, sexp, sexp, sexp, sexp);

enum core_form_names {
  CORE_DEFINE = 1,
  CORE_SET,
  CORE_LAMBDA,
  CORE_IF,
  CORE_BEGIN,
  CORE_QUOTE,
  CORE_DEFINE_SYNTAX,
  CORE_LET_SYNTAX,
  CORE_LETREC_SYNTAX,
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
};

enum opcode_names {
  OP_NOOP,
  OP_ERROR,
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
  OP_EVAL,
  OP_JUMP_UNLESS,
  OP_JUMP,
  OP_PUSH,
  OP_DROP,
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
  OP_CAR,
  OP_CDR,
  OP_SET_CAR,
  OP_SET_CDR,
  OP_CONS,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_QUOT,
  OP_MOD,
  OP_NEG,
  OP_INV,
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
};

/**************************** prototypes ******************************/

sexp apply(sexp proc, sexp args, sexp context);
sexp eval_in_context(sexp expr, sexp context);
sexp eval(sexp expr, sexp env);

#endif /* ! SEXP_EVAL_H */

