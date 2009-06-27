/*  eval.h -- headers for eval library                   */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifndef SEXP_EVAL_H
#define SEXP_EVAL_H

#include "chibi/sexp.h"

/************************* additional types ***************************/

#define INIT_BCODE_SIZE 128
#define INIT_STACK_SIZE 1024

#define sexp_init_file "init.scm"

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
};

/**************************** prototypes ******************************/

void sexp_scheme_init();
sexp sexp_apply(sexp context, sexp proc, sexp args);
sexp sexp_eval(sexp context, sexp obj);
sexp sexp_eval_string(sexp context, char *str);
sexp sexp_load(sexp context, sexp expr, sexp env);
sexp sexp_make_context(sexp context, sexp stack, sexp env);
void sexp_warn_undefs (sexp from, sexp to, sexp out);

#endif /* ! SEXP_EVAL_H */

