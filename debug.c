/*  debug.c -- optional debugging utilities              */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

static const char* reverse_opcode_names[] =
  {"NOOP", "ERROR", "RESUMECC", "CALLCC", "APPLY1", "TAIL_CALL", "CALL",
   "FCALL0", "FCALL1", "FCALL2", "FCALL3", "EVAL", "JUMP_UNLESS", "JUMP",
   "PARAMETER", "PUSH", "DROP", "STACK_REF", "LOCAL_REF", "LOCAL_SET",
   "CLOSURE_REF", "VECTOR_REF", "VECTOR_SET", "STRING_REF", "STRING_SET",
   "MAKE_PROCEDURE", "MAKE_VECTOR", "PAIRP", "NULLP", "VECTORP", "INTEGERP",
   "SYMBOLP", "STRINGP", "CHARP", "EOFP", "PROCEDUREP", "IPORTP", "OPORTP",
   "CAR", "CDR", "SET_CAR", "SET_CDR", "CONS", "ADD", "SUB", "MUL", "DIV",
   "QUOT", "MOD", "NEG", "INV", "LT", "LE", "EQV", "EQ", "DISPLAY", "WRITE",
   "WRITE_CHAR", "NEWLINE", "FLUSH_OUTPUT", "READ", "READ_CHAR", "RET", "DONE",
  };

void disasm (sexp bc) {
  unsigned char *ip=sexp_bytecode_data(bc), opcode;
 loop:
  opcode = *ip++;
  if (opcode*sizeof(char*) < sizeof(reverse_opcode_names)) {
    fprintf(stderr, "  %s ", reverse_opcode_names[opcode]);
  } else {
    fprintf(stderr, "  <unknown> %d ", opcode);
  }
  switch (opcode) {
  case OP_STACK_REF:
  case OP_LOCAL_REF:
  case OP_LOCAL_SET:
  case OP_CLOSURE_REF:
  case OP_PARAMETER:
  case OP_JUMP:
  case OP_JUMP_UNLESS:
    fprintf(stderr, "%ld", (sexp_sint_t) ((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_TAIL_CALL:
  case OP_CALL:
  case OP_PUSH:
    sexp_write(((sexp*)ip)[0], cur_error_port);
    ip += sizeof(sexp);
    break;
  }
  fprintf(stderr, "\n");
  if (ip - sexp_bytecode_data(bc) < sexp_bytecode_length(bc))
    goto loop;
}

void print_bytecode (sexp bc) {
  int i;
  unsigned char *data = sexp_bytecode_data(bc);
  fprintf(stderr, "bytecode @ %p, data @ %p, length = %lu\n",
          bc, data, sexp_bytecode_length(bc));
  for (i=0; i+16 < sexp_bytecode_length(bc); i+=8) {
    fprintf(stderr, "%02x: %02x %02x %02x %02x %02x %02x %02x %02x   ", i,
            data[i], data[i+1], data[i+2], data[i+3],
            data[i+4], data[i+5], data[i+6], data[i+7]);
    i += 8;
    fprintf(stderr, "%02x %02x %02x %02x %02x %02x %02x %02x\n",
            data[i], data[i+1], data[i+2], data[i+3],
            data[i+4], data[i+5], data[i+6], data[i+7]);
  }
  if (i != sexp_bytecode_length(bc)) {
    fprintf(stderr, "%02x:", i);
    for ( ; i < sexp_bytecode_length(bc); i++) {
      if ((i % 8) == 0 && (i % 16) != 0)
        fprintf(stderr, "  ");
      fprintf(stderr, " %02x", data[i]);
    }
    fprintf(stderr, "\n");
  }
}

void print_stack (sexp *stack, int top, int fp) {
  int i;
  for (i=0; i<top; i++) {
    fprintf(stderr, "%s %02d: ", ((i==fp) ? "*" : " "), i);
    fflush(stderr);
    sexp_write(stack[i], cur_error_port);
    fprintf(stderr, "\n");
  }
}

