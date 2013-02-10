/*  param.c -- low-level parameter utilities                  */
/*  Copyright (c) 2010-2011 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

#define _I(x) sexp_make_fixnum(x)

static sexp sexp_make_parameter (sexp ctx, sexp self, sexp_sint_t n, sexp init, sexp conv) {
  sexp res;
  sexp_gc_var1(cell);
  sexp_gc_preserve1(ctx, cell);
  cell = sexp_cons(ctx, SEXP_FALSE, init);
  res = sexp_make_opcode(ctx, self, SEXP_FALSE, _I(SEXP_OPC_PARAMETER),
                         _I(SEXP_OP_PARAMETER_REF), SEXP_ZERO, SEXP_ONE,
                         _I(SEXP_OBJECT), _I(SEXP_OBJECT), SEXP_ZERO,
                         cell, conv, NULL);
  sexp_gc_release1(ctx);
  return res;
}

static sexp sexp_parameter_converter (sexp ctx, sexp self, sexp_sint_t n, sexp param) {
  sexp res;
  sexp_assert_type(ctx, sexp_opcodep, SEXP_OPCODE, param);
  res = sexp_opcode_data2(param);
  return res ? res : SEXP_FALSE;
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;

  sexp_define_foreign(ctx, env, "%make-parameter", 2, sexp_make_parameter);
  sexp_define_foreign(ctx, env, "parameter-converter", 1, sexp_parameter_converter);

  return SEXP_VOID;
}

#undef _I
