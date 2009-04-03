/*  debug.c -- optional debugging utilities              */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

static const char* reverse_opcode_names[] =
  {"NOOP", "ERROR", "RESUMECC", "CALLCC", "APPLY1", "TAIL-CALL", "CALL",
   "FCALL0", "FCALL1", "FCALL2", "FCALL3", "FCALL4", "EVAL", "JUMP-UNLESS",
   "JUMP", "PUSH", "DROP", "GLOBAL-REF", "GLOBAL-KNOWN-REF", "STACK-REF",
   "LOCAL-REF", "LOCAL-SET",
   "CLOSURE-REF", "VECTOR-REF", "VECTOR-SET", "VECTOR-LENGTH", "STRING-REF",
   "STRING-SET", "STRING-LENGTH", "MAKE-PROCEDURE", "MAKE-VECTOR", "AND",
   "NULL?", "FIXNUM?", "SYMBOL?", "CHAR?",
   "EOF?", "TYPEP", "CAR", "CDR", "SET-CAR", "SET-CDR", "CONS", "ADD", "SUB",
   "MUL", "DIV", "QUOTIENT", "REMAINDER", "NEGATIVE", "INVERSE",
   "LT", "LE", "EQN", "EQ",
   "EXACT->INEXACT", "INEXACT->EXACT",
   "CHAR->INTEGER", "INTEGER->CHAR", "CHAR-UPCASE", "CHAR-DOWNCASE",
   "DISPLAY", "WRITE", "WRITE-CHAR",
   "NEWLINE", "FLUSH-OUTPUT", "READ", "READ-CHAR", "PEEK-CHAR", "RET", "DONE",
  };

static sexp sexp_disasm (sexp bc, sexp out) {
  unsigned char *ip, opcode;
  if (sexp_procedurep(bc))
    bc = sexp_procedure_code(bc);
  ip = sexp_bytecode_data(bc);
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
  case OP_GLOBAL_REF:
  case OP_GLOBAL_KNOWN_REF:
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
  return SEXP_VOID;
}

static void sexp_print_stack (sexp *stack, int top, int fp, sexp out) {
  int i;
  for (i=0; i<top; i++) {
    sexp_printf(out, "%s %02d: ", ((i==fp) ? "*" : " "), i);
    sexp_write(stack[i], out);
    sexp_printf(out, "\n");
  }
}

