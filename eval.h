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

typedef sexp (*sexp_proc0) ();
typedef sexp (*sexp_proc1) (sexp);
typedef sexp (*sexp_proc2) (sexp, sexp);
typedef sexp (*sexp_proc3) (sexp, sexp, sexp);
typedef sexp (*sexp_proc4) (sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc5) (sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc6) (sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc7) (sexp, sexp, sexp, sexp, sexp, sexp, sexp);

typedef struct bytecode {
  char tag;
  unsigned int len;
  unsigned char data[];
} *bytecode;

typedef struct procedure {
  char tag;
  char flags;
  unsigned short num_args;
  bytecode bc;
  sexp vars;
} *procedure;

typedef struct env {
  char tag;
  struct env *parent;
  sexp bindings;
} *env;

typedef struct macro {
  char tag;
  procedure proc;
  env e;
} *macro;

typedef struct opcode {
  char tag;
  char op_class;
  char op_name;
  char num_args;
  char var_args_p;
  char arg1_type;
  char arg2_type;
  char op_inverse;
  char* name;
  sexp data;
  sexp proc;
} *opcode;

typedef struct core_form {
  char tag;
  char code;
  char* name;
} *core_form;

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
/*   OP_FCALL4, */
/*   OP_FCALL5, */
/*   OP_FCALL6, */
/*   OP_FCALL7, */
  OP_FCALLN,
  OP_JUMP_UNLESS,
  OP_JUMP,
  OP_RET,
  OP_DONE,
  OP_PARAMETER,
  OP_STACK_REF,
  OP_STACK_SET,
  OP_GLOBAL_REF,
  OP_GLOBAL_SET,
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

bytecode compile(sexp params, sexp obj, env e, sexp fv, sexp sv, int done_p);

void analyze_app (sexp obj, bytecode *bc, sexp_uint_t *i,
                  env e, sexp params, sexp fv, sexp sv,
                  sexp_uint_t *d, int tailp);
void analyze_lambda (sexp name, sexp formals, sexp body,
                     bytecode *bc, sexp_uint_t *i, env e,
                     sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp);
void analyze_var_ref (sexp name, bytecode *bc, sexp_uint_t *i, env e,
                      sexp params, sexp fv, sexp sv, sexp_uint_t *d);
void analyze_opcode (opcode op, sexp obj, bytecode *bc, sexp_uint_t *i, env e,
                     sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp);
sexp vm(bytecode bc, env e, sexp* stack, sexp_sint_t top);

sexp eval_in_stack(sexp obj, env e, sexp* stack, sexp_sint_t top);
sexp eval(sexp obj, env e);

#endif /* ! SEXP_EVAL_H */

