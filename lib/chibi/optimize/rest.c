/*  rest.c -- low-level utilities for VM rest optimization    */
/*  Copyright (c) 2011 Alex Shinn.  All rights reserved.      */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

static sexp sexp_num_parameters (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_stack_data(sexp_context_stack(ctx))[sexp_context_last_fp(ctx)];
}

struct sexp_opcode_struct local_ref_op =
  {SEXP_OPC_GENERIC, SEXP_OP_LOCAL_REF, 1, 8, 0, (sexp)"local-ref", SEXP_VOID,
   NULL, NULL, sexp_make_fixnum(SEXP_OBJECT), sexp_make_fixnum(SEXP_FIXNUM),
   0, 0, NULL};

static sexp copy_opcode (sexp ctx, struct sexp_opcode_struct *op) {
  sexp res = sexp_alloc_type(ctx, opcode, SEXP_OPCODE);
  memcpy(&(res->value), op, sizeof(op[0]));
  return res;
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  sexp_gc_var2(name, op);
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_gc_preserve2(ctx, name, op);
  sexp_define_foreign(ctx, env, "num-parameters", 0, sexp_num_parameters);
  op = copy_opcode(ctx, &local_ref_op);
  sexp_opcode_name(op) = sexp_c_string(ctx, (char*)sexp_opcode_name(op), -1);
  name = sexp_string_to_symbol(ctx, sexp_opcode_name(op));
  sexp_env_define(ctx, env, name, op);
  sexp_gc_release2(ctx);
  return SEXP_VOID;
}
