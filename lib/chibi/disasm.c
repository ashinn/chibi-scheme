/*  disasm.c -- optional debugging utilities                  */
/*  Copyright (c) 2009-2012 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"
#if ! SEXP_USE_STATIC_LIBS
#include "../../opt/opcode_names.h"
#endif

#define SEXP_DISASM_MAX_DEPTH 16
#define SEXP_DISASM_PAD_WIDTH 4

#if SEXP_64_BIT
#define SEXP_PRId "%ld"
#else
#define SEXP_PRId "%d"
#endif

static void sexp_write_pointer (sexp ctx, void *p, sexp out) {
  char buf[32];
  sprintf(buf, "%p", p);
  sexp_write_string(ctx, buf, out);
}

static void sexp_write_integer (sexp ctx, sexp_sint_t n, sexp out) {
  char buf[32];
  sprintf(buf, SEXP_PRId, n);
  sexp_write_string(ctx, buf, out);
}

static sexp disasm (sexp ctx, sexp self, sexp bc, sexp out, int depth) {
  unsigned char *ip, opcode, i;
  sexp tmp=NULL, src;
  sexp_sint_t *labels, label=1, off;
#if SEXP_USE_FULL_SOURCE_INFO
  sexp src_here=NULL;
  sexp_sint_t src_off=0;
#endif

  if (sexp_procedurep(bc)) {
    bc = sexp_procedure_code(bc);
  } else if (sexp_opcodep(bc)) {
    sexp_write(ctx, sexp_opcode_name(bc), out);
    sexp_write_string(ctx, " is a primitive\n", out);
    return SEXP_VOID;
  } else if (! sexp_bytecodep(bc)) {
    return sexp_type_exception(ctx, self, SEXP_BYTECODE, bc);
  }
  if (! sexp_oportp(out)) {
    return sexp_type_exception(ctx, self, SEXP_OPORT, out);
  }

  src = sexp_bytecode_source(bc);

  for (i=0; i<(depth*SEXP_DISASM_PAD_WIDTH); i++)
    sexp_write_char(ctx, ' ', out);
  sexp_write_string(ctx, "      -------------- ", out);
  if (sexp_truep(sexp_bytecode_name(bc))) {
    sexp_write(ctx, sexp_bytecode_name(bc), out);
    sexp_write_char(ctx, ' ', out);
  }
  sexp_write_pointer(ctx, bc, out);
#if SEXP_USE_FULL_SOURCE_INFO
  if (!(src && sexp_vectorp(src)))
    src_off = -1;
  /* if (src) sexp_write(ctx, src, out); */
#else
  if (src && sexp_pair(src)) {
    sexp_write(ctx, sexp_car(src), out);
    sexp_write_string(ctx, ":", out);
    sexp_write(ctx, sexp_cdr(src), out);
  }
#endif
  sexp_newline(ctx, out);

  /* build a table of labels that are jumped to */
  labels = calloc(sexp_bytecode_length(bc), sizeof(sexp_sint_t));
  ip = sexp_bytecode_data(bc);
  while (ip - sexp_bytecode_data(bc) < sexp_bytecode_length(bc)) {
    switch (*ip++) {
    case SEXP_OP_JUMP:
    case SEXP_OP_JUMP_UNLESS:
      off = ip - sexp_bytecode_data(bc) + ((sexp_sint_t*)ip)[0];
      if (off >= 0 && off < sexp_bytecode_length(bc) && labels[off] == 0)
        labels[off] = label++;
    case SEXP_OP_CALL:
    case SEXP_OP_CLOSURE_REF:
    case SEXP_OP_GLOBAL_KNOWN_REF:
    case SEXP_OP_GLOBAL_REF:
    case SEXP_OP_LOCAL_REF:
    case SEXP_OP_LOCAL_SET:
    case SEXP_OP_PARAMETER_REF:
    case SEXP_OP_PUSH:
    case SEXP_OP_RESERVE:
    case SEXP_OP_STACK_REF:
    case SEXP_OP_TAIL_CALL:
    case SEXP_OP_TYPEP:
      ip += sizeof(sexp);
      break;
    case SEXP_OP_SLOT_REF:
    case SEXP_OP_SLOT_SET:
    case SEXP_OP_MAKE:
      ip += sizeof(sexp)*2;
      break;
    case SEXP_OP_MAKE_PROCEDURE:
      ip += sizeof(sexp)*3;
      break;
    default:
      /* opcode takes no additional instruction args */
      break;
    }
  }

  ip = sexp_bytecode_data(bc);
 loop:
  for (i=0; i<(depth*SEXP_DISASM_PAD_WIDTH); i++)
    sexp_write_char(ctx, ' ', out);
  if (labels[ip - sexp_bytecode_data(bc)] == 0) {
    sexp_write_string(ctx, "     ", out);
  } else {
    sexp_write_char(ctx, 'L', out);
    sexp_write_integer(ctx, labels[ip - sexp_bytecode_data(bc)], out);
    sexp_write_string(ctx, ": ", out);
    if (labels[ip - sexp_bytecode_data(bc)] < 10)
      sexp_write_char(ctx, ' ', out);
  }
#if SEXP_USE_FULL_SOURCE_INFO
  if ((src_off >= 0)
      && ((ip-sexp_bytecode_data(bc))
          == sexp_unbox_fixnum(
               sexp_car(sexp_vector_ref(src, sexp_make_fixnum(src_off)))))) {
    src_here = sexp_cdr(sexp_vector_ref(src, sexp_make_fixnum(src_off)));
    src_off = src_off < sexp_vector_length(src)-1 ? src_off + 1 : -1;
  } else {
    src_here = NULL;
  }
#endif
  opcode = *ip++;
  if (opcode < SEXP_OP_NUM_OPCODES) {
    sexp_write_char(ctx, ' ', out);
    sexp_write_string(ctx, sexp_opcode_names[opcode], out);
    sexp_write_char(ctx, ' ', out);
  } else {
    sexp_write_string(ctx, "  <invalid> ", out);
    sexp_write(ctx, sexp_make_fixnum(opcode), out);
    sexp_write_char(ctx, ' ', out);
  }
  switch (opcode) {
  case SEXP_OP_STACK_REF:
  case SEXP_OP_LOCAL_REF:
  case SEXP_OP_LOCAL_SET:
  case SEXP_OP_CLOSURE_REF:
  case SEXP_OP_TYPEP:
  case SEXP_OP_RESERVE:
    sexp_write_integer(ctx, ((sexp_sint_t*)ip)[0], out);
    ip += sizeof(sexp);
    break;
  case SEXP_OP_JUMP:
  case SEXP_OP_JUMP_UNLESS:
    sexp_write_integer(ctx, ((sexp_sint_t*)ip)[0], out);
    off = ip - sexp_bytecode_data(bc) + ((sexp_sint_t*)ip)[0];
    if (off >= 0 && off < sexp_bytecode_length(bc) && labels[off] > 0) {
      sexp_write_string(ctx, " L", out);
      sexp_write_integer(ctx, labels[off], out);
    }
    ip += sizeof(sexp);
    break;
  case SEXP_OP_FCALL0:
  case SEXP_OP_FCALL1:
  case SEXP_OP_FCALL2:
  case SEXP_OP_FCALL3:
  case SEXP_OP_FCALL4:
    sexp_write_pointer(ctx, ((sexp*)ip)[0], out);
    sexp_write_char(ctx, ' ', out);
    sexp_write(ctx, sexp_opcode_name(((sexp*)ip)[0]), out);
    ip += sizeof(sexp);
    break;
  case SEXP_OP_SLOT_REF:
  case SEXP_OP_SLOT_SET:
  case SEXP_OP_MAKE:
    ip += sizeof(sexp)*2;
    break;
  case SEXP_OP_MAKE_PROCEDURE:
    sexp_write_integer(ctx, ((sexp_sint_t*)ip)[0], out);
    sexp_write_char(ctx, ' ', out);
    sexp_write_integer(ctx, ((sexp_sint_t*)ip)[1], out);
    tmp = ((sexp*)ip)[2];
    ip += sizeof(sexp)*3;
    break;
  case SEXP_OP_GLOBAL_REF:
  case SEXP_OP_GLOBAL_KNOWN_REF:
  case SEXP_OP_PARAMETER_REF:
  case SEXP_OP_TAIL_CALL:
  case SEXP_OP_CALL:
  case SEXP_OP_PUSH:
    tmp = ((sexp*)ip)[0];
    if (((opcode == SEXP_OP_GLOBAL_REF) || (opcode == SEXP_OP_GLOBAL_KNOWN_REF))
        && sexp_pairp(tmp))
      tmp = sexp_car(tmp);
    else if ((opcode == SEXP_OP_PARAMETER_REF)
             && sexp_opcodep(tmp) && sexp_opcode_data(tmp)
             && sexp_pairp(sexp_opcode_data(tmp)))
      tmp = sexp_car(sexp_opcode_data(tmp));
    else if ((opcode == SEXP_OP_PUSH) && (sexp_pairp(tmp) || sexp_idp(tmp)))
      sexp_write_char(ctx, '\'', out);
    sexp_write(ctx, tmp, out);
    ip += sizeof(sexp);
    break;
  }
#if SEXP_USE_FULL_SOURCE_INFO
  if (src_here && sexp_pairp(src_here)) {
    sexp_write_string(ctx, "    ; ", out);
    sexp_write(ctx, sexp_car(src_here), out);
    sexp_write_string(ctx, ":", out);
    sexp_write(ctx, sexp_cdr(src_here), out);
  }
#endif
  sexp_write_char(ctx, '\n', out);
  if ((opcode == SEXP_OP_PUSH || opcode == SEXP_OP_MAKE_PROCEDURE)
      && (depth < SEXP_DISASM_MAX_DEPTH)
      && tmp && (sexp_bytecodep(tmp) || sexp_procedurep(tmp)))
    disasm(ctx, self, tmp, out, depth+1);
  if (ip - sexp_bytecode_data(bc) < sexp_bytecode_length(bc))
    goto loop;

  free(labels);
  return SEXP_VOID;
}

static sexp sexp_disasm (sexp ctx, sexp self, sexp_sint_t n, sexp bc, sexp out) {
  return disasm(ctx, self, bc, out, 0);
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return sexp_global(ctx, SEXP_G_ABI_ERROR);
  sexp_define_foreign_param(ctx, env, "disasm", 2, (sexp_proc1)sexp_disasm, "current-output-port");
  return SEXP_VOID;
}
