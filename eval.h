/*  eval.h -- headers for eval library */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt */

#ifndef SCM_EVAL_H
#define SCM_EVAL_H

#include "sexp.h"

/************************* additional types ***************************/

#define INIT_BCODE_SIZE 128
#define INIT_STACK_SIZE 1024

typedef struct bytecode {
  char tag;
  unsigned int len;
  unsigned char data[];
} *bytecode;

/* env binding: #(id chain offset flags) */
/* chain is the index into the closure parent list (0 for current lambda) */
/* macros/constants have a value instead of chain */
typedef struct env {
  char tag;
  struct env *parent;
  sexp bindings;
} *env;

typedef struct opcode {
  char tag;
  char op_class;
  char op_name;
  char num_args;
  char var_args_p;
  char arg1_type;
  char arg2_type;
  char* name;
  char op_inverse;
  sexp proc;
} *opcode;

typedef struct core_form {
  char tag;
  char* name;
  char code;
} *core_form;

enum core_form_names {
  CORE_DEFINE,
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
  OPC_GENERIC,
  OPC_TYPE_PREDICATE,
  OPC_PREDICATE,
  OPC_ARITHMETIC,
  OPC_ARITHMETIC_INV,
  OPC_ARITHMETIC_CMP,
  OPC_CONSTRUCTOR,
};

enum opcode_names {
  OP_NOOP,                      /* 0 */
  OP_STACK_REF,                 /* 1 */
  OP_STACK_SET,                 /* 2 */
  OP_GLOBAL_REF,                /* 3 */
  OP_GLOBAL_SET,                /* 4 */
  OP_CLOSURE_REF,               /* 5 */
  OP_CLOSURE_SET,               /* 6 */
  OP_VECTOR_REF,                /* 7 */
  OP_VECTOR_SET,                /* 8 */
  OP_MAKE_PROCEDURE,
  OP_MAKE_VECTOR,
  OP_PUSH,
  OP_DUP,                       /* C */
  OP_DROP,
  OP_SWAP,
  OP_CAR,
  OP_CDR,                       /* 10 */
  OP_SET_CAR,                   /* 11 */
  OP_SET_CDR,                   /* 12 */
  OP_CONS,
  OP_ADD,                       /* 14 */
  OP_SUB,
  OP_MUL,                       /* 16 */
  OP_DIV,
  OP_MOD,                       /* 18 */
  OP_NEG,
  OP_INV,                       /* 1A */
  OP_LT,
  OP_CALL,                      /* 1C */
  OP_JUMP_UNLESS,
  OP_JUMP,                      /* 1E */
  OP_RET,
  OP_DONE,
};

/**************************** prototypes ******************************/

bytecode compile(sexp params, sexp obj, env e, sexp fv, sexp sv, int done_p);

void analyze_app (sexp obj, bytecode *bc, unsigned int *i,
                  env e, sexp params, sexp fv, sexp sv, unsigned int *d);
void analyze_lambda (sexp name, sexp formals, sexp body,
                     bytecode *bc, unsigned int *i, env e,
                     sexp params, sexp fv, sexp sv, unsigned int *d);
void analyze_var_ref (sexp name, bytecode *bc, unsigned int *i, env e,
                      sexp params, sexp fv, sexp sv, unsigned int *d);

sexp vm(bytecode bc, env e, sexp* stack, unsigned int top);

sexp eval_in_stack(sexp obj, env e, sexp* stack, unsigned int top);
sexp eval(sexp obj, env e);

#endif /* ! SCM_EVAL_H */

