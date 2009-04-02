/*  debug.c -- optional debugging utilities              */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

static const char* reverse_opcode_names[] =
  {"NOOP", "ERROR", "RESUMECC", "CALLCC", "APPLY1", "TAIL-CALL", "CALL",
   "FCALL0", "FCALL1", "FCALL2", "FCALL3", "FCALL4", "EVAL", "JUMP-UNLESS",
   "JUMP", "PUSH", "DROP", "STACK-REF", "LOCAL-REF", "LOCAL-SET",
   "CLOSURE-REF", "VECTOR-REF", "VECTOR-SET", "VECTOR-LENGTH", "STRING-REF",
   "STRING-SET", "STRING-LENGTH", "MAKE-PROCEDURE", "MAKE-VECTOR", "NULL?",
   "FIXNUM?", "SYMBOL?", "CHAR?",
   "EOF?", "TYPEP", "CAR", "CDR", "SET-CAR", "SET-CDR", "CONS", "ADD", "SUB",
   "MUL", "DIV", "QUOT", "MOD", "NEG", "INV", "LT", "LE", "EQN", "EQ",
   "EXACT->INEXACT", "INEXACT->EXACT",
   "CHAR->INTEGER", "INTEGER->CHAR", "CHAR-UPCASE", "CHAR-DOWNCASE",
   "DISPLAY", "WRITE", "WRITE-CHAR",
   "NEWLINE", "FLUSH-OUTPUT", "READ", "READ-CHAR", "RET", "DONE",
  };

void disasm (sexp bc, sexp out) {
  unsigned char *ip=sexp_bytecode_data(bc), opcode;
 loop:
  opcode = *ip++;
  if (opcode*sizeof(char*) < sizeof(reverse_opcode_names)) {
    sexp_printf(out, "  %s ", reverse_opcode_names[opcode]);
  } else {
    sexp_printf(out, "  <unknown> %d ", opcode);
  }
  switch (opcode) {
  case OP_STACK_REF:
  case OP_LOCAL_REF:
  case OP_LOCAL_SET:
  case OP_CLOSURE_REF:
  case OP_JUMP:
  case OP_JUMP_UNLESS:
  case OP_FCALL0:
  case OP_FCALL1:
  case OP_FCALL2:
  case OP_FCALL3:
  case OP_TYPEP:
    sexp_printf(out, "%ld", (sexp_sint_t) ((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_TAIL_CALL:
  case OP_CALL:
  case OP_PUSH:
    sexp_write(((sexp*)ip)[0], out);
    ip += sizeof(sexp);
    break;
  }
  sexp_write_char('\n', out);
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

void print_stack (sexp *stack, int top, int fp, sexp out) {
  int i;
  for (i=0; i<top; i++) {
    sexp_printf(out, "%s %02d: ", ((i==fp) ? "*" : " "), i);
    sexp_write(stack[i], out);
    sexp_printf(out, "\n");
  }
}

