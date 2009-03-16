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

#define sexp_debug(msg, obj) (sexp_write_string(msg,cur_error_port), sexp_write(obj, cur_error_port), sexp_write_char('\n',cur_error_port))

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
  OP_TAIL_CALL,
  OP_CALL,
  OP_APPLY1,
  OP_CALLCC,
  OP_RESUMECC,
  OP_ERROR,
  OP_FCALL0,
  OP_FCALL1,
  OP_FCALL2,
  OP_FCALL3,
  OP_FCALLN,
  OP_JUMP_UNLESS,
  OP_JUMP,
  OP_RET,
  OP_DONE,
  OP_PARAMETER,
  OP_STACK_REF,
  OP_STACK_SET,
  OP_CLOSURE_REF,
  OP_VECTOR_REF,
  OP_VECTOR_SET,
  OP_STRING_REF,
  OP_STRING_SET,
  OP_MAKE_PROCEDURE,
  OP_MAKE_VECTOR,
  OP_PUSH,
  OP_DROP,
  OP_PAIRP,
  OP_NULLP,
  OP_VECTORP,
  OP_INTEGERP,
  OP_SYMBOLP,
  OP_STRINGP,
  OP_CHARP,
  OP_EOFP,
  OP_PROCEDUREP,
  OP_IPORTP,
  OP_OPORTP,
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
  OP_GT,
  OP_GE,
  OP_EQN,
  OP_EQ,
  OP_DISPLAY,
  OP_WRITE,
  OP_WRITE_CHAR,
  OP_NEWLINE,
  OP_FLUSH_OUTPUT,
  OP_READ,
  OP_READ_CHAR,
};

/**************************** prototypes ******************************/

sexp compile(sexp params, sexp obj, sexp e, sexp fv, sexp sv, int done_p);

sexp analyze_app(sexp obj, sexp *bc, sexp_uint_t *i,
                 sexp e, sexp params, sexp fv, sexp sv,
                 sexp_uint_t *d, int tailp);
sexp analyze_lambda(sexp name, sexp formals, sexp body,
                    sexp *bc, sexp_uint_t *i, sexp e,
                    sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp);
void analyze_var_ref(sexp name, sexp *bc, sexp_uint_t *i, sexp e,
                     sexp params, sexp fv, sexp sv, sexp_uint_t *d);
sexp analyze_opcode(sexp op, sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
                    sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp);
sexp analyze(sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
             sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp);
sexp analyze_sequence(sexp ls, sexp *bc, sexp_uint_t *i, sexp e,
                      sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp);
sexp vm(sexp bc, sexp e, sexp* stack, sexp_sint_t top);

sexp eval_in_stack(sexp expr, sexp e, sexp* stack, sexp_sint_t top);
sexp eval(sexp expr, sexp e);

#endif /* ! SEXP_EVAL_H */

