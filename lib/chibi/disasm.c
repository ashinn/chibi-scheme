/*  disasm.c -- optional debugging utilities                  */
/*  Copyright (c) 2009-2012 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include "chibi/eval.h"
#if ! SEXP_USE_STATIC_LIBS
#include "../../opt/opcode_names.h"
#endif

#define SEXP_DISASM_MAX_DEPTH 16
#define SEXP_DISASM_PAD_WIDTH 4

#if SEXP_64_BIT
#ifdef _WIN32
#define SEXP_PRId "%I64d"
#else
#define SEXP_PRId "%ld"
#endif
#else
#define SEXP_PRId "%d"
#endif

static void sexp_write_pointer (sexp ctx, void *p, sexp out) {
  char buf[32];
  snprintf(buf, sizeof(buf), "%p", p);
  sexp_write_string(ctx, buf, out);
}

#if ! SEXP_USE_NATIVE_X86
static void sexp_write_integer (sexp ctx, sexp_sint_t n, sexp out) {
  char buf[32];
  snprintf(buf, sizeof(buf), SEXP_PRId, n);
  sexp_write_string(ctx, buf, out);
}
#endif

#if SEXP_USE_NATIVE_X86

#ifdef __APPLE__
#include <mach-o/loader.h>

static int write_macho_file (const char *path, const unsigned char *code, size_t code_len) {
  FILE *f = fopen(path, "wb");
  if (!f) return -1;
  uint32_t header_len = sizeof(struct mach_header_64) + sizeof(struct segment_command_64) + sizeof(struct section_64);
  struct mach_header_64 mh;
  memset(&mh, 0, sizeof(mh));
  mh.magic = MH_MAGIC_64;
  mh.cputype = CPU_TYPE_X86_64;
  mh.cpusubtype = CPU_SUBTYPE_LIB64 | 3;
  mh.filetype = MH_OBJECT;
  mh.ncmds = 1;
  mh.sizeofcmds = sizeof(struct segment_command_64) + sizeof(struct section_64);

  struct segment_command_64 seg;
  memset(&seg, 0, sizeof(seg));
  seg.cmd = LC_SEGMENT_64;
  seg.cmdsize = sizeof(struct segment_command_64) + sizeof(struct section_64);
  seg.vmsize = code_len;
  seg.fileoff = header_len;
  seg.filesize = code_len;
  seg.maxprot = 7;
  seg.initprot = 7;
  seg.nsects = 1;

  struct section_64 sect;
  memset(&sect, 0, sizeof(sect));
  strncpy(sect.sectname, "__text", sizeof(sect.sectname));
  strncpy(sect.segname, "__TEXT", sizeof(sect.segname));
  sect.size = code_len;
  sect.offset = header_len;
  sect.align = 4;
  sect.flags = S_REGULAR | S_ATTR_SOME_INSTRUCTIONS | S_ATTR_PURE_INSTRUCTIONS;

  fwrite(&mh, 1, sizeof(mh), f);
  fwrite(&seg, 1, sizeof(seg), f);
  fwrite(&sect, 1, sizeof(sect), f);
  fwrite(code, 1, code_len, f);
  fclose(f);
  return 0;
}
#endif

static sexp find_matching_literal (sexp bc, uint64_t val) {
  sexp lits = sexp_bytecode_literals(bc);
  if (!lits || lits == SEXP_NULL) return SEXP_FALSE;
  if (sexp_vectorp(lits)) {
    for (int i = 0; i < sexp_vector_length(lits); i++) {
      sexp x = sexp_vector_ref(lits, sexp_make_fixnum(i));
      if ((uint64_t)x == val) return x;
    }
  } else if (sexp_pairp(lits)) {
    if ((uint64_t)sexp_car(lits) == val) return sexp_car(lits);
    if ((uint64_t)sexp_cdr(lits) == val) return sexp_cdr(lits);
  } else {
    if ((uint64_t)lits == val) return lits;
  }
  return SEXP_FALSE;
}

static void annotate_native_value (sexp ctx, sexp bc, uint64_t val, const char *line, sexp out) {
  if (val == (uint64_t)sexp_native_enter) {
    sexp_write_string(ctx, "   ; sexp_native_enter", out);
    return;
  }
  if (val == (uint64_t)sexp_native_exit) {
    sexp_write_string(ctx, "   ; sexp_native_exit", out);
    return;
  }
  if (val == (uint64_t)sexp_native_call) {
    sexp_write_string(ctx, "   ; sexp_native_call", out);
    return;
  }
  if (val == (uint64_t)sexp_native_dispatch_op) {
    sexp_write_string(ctx, "   ; sexp_native_dispatch_op", out);
    return;
  }
  if (val == (uint64_t)sexp_native_make_closure) {
    sexp_write_string(ctx, "   ; sexp_native_make_closure", out);
    return;
  }
  if (val == (uint64_t)sexp_native_make_trampoline) {
    sexp_write_string(ctx, "   ; sexp_native_make_trampoline", out);
    return;
  }
  if (val == (uint64_t)sexp_native_parameter_set) {
    sexp_write_string(ctx, "   ; sexp_native_parameter_set", out);
    return;
  }
  if (val == (uint64_t)sexp_parameter_ref) {
    sexp_write_string(ctx, "   ; sexp_parameter_ref", out);
    return;
  }
  if (val == (uint64_t)SEXP_FALSE) {
    sexp_write_string(ctx, "   ; #f", out);
    return;
  }
  if (val == (uint64_t)SEXP_TRUE) {
    sexp_write_string(ctx, "   ; #t", out);
    return;
  }
  if (val == (uint64_t)SEXP_NULL) {
    sexp_write_string(ctx, "   ; ()", out);
    return;
  }
  if (val == (uint64_t)SEXP_VOID) {
    sexp_write_string(ctx, "   ; #<void>", out);
    return;
  }
  if (val == (uint64_t)SEXP_UNDEF) {
    sexp_write_string(ctx, "   ; #<undef>", out);
    return;
  }
  if (sexp_charp((sexp)val)) {
    sexp_write_string(ctx, "   ; ", out);
    sexp_write(ctx, (sexp)val, out);
    return;
  }
  sexp item = find_matching_literal(bc, val);
  if (item != SEXP_FALSE) {
    if (sexp_pairp(item) && sexp_symbolp(sexp_car(item))) {
      sexp_write_string(ctx, "   ; global: ", out);
      sexp_write(ctx, sexp_car(item), out);
    } else if (sexp_opcodep(item)) {
      sexp_write_string(ctx, "   ; opcode: ", out);
      sexp_write(ctx, sexp_opcode_name(item), out);
    } else if (sexp_bytecodep(item) && sexp_truep(sexp_bytecode_name(item))) {
      sexp_write_string(ctx, "   ; bytecode: ", out);
      sexp_write(ctx, sexp_bytecode_name(item), out);
    } else if (sexp_procedurep(item) && sexp_bytecodep(sexp_procedure_code(item)) && sexp_truep(sexp_bytecode_name(sexp_procedure_code(item)))) {
      sexp_write_string(ctx, "   ; procedure: ", out);
      sexp_write(ctx, sexp_bytecode_name(sexp_procedure_code(item)), out);
    } else {
      sexp_write_string(ctx, "   ; '", out);
      sexp_write(ctx, item, out);
    }
    return;
  }
  if (strstr(line, "%r9")) {
    sexp_sint_t num_params = val & 0xFFFF;
    sexp_sint_t num_slots = (val >> 16) & 0xFFFF;
    char pbuf[64];
    snprintf(pbuf, sizeof(pbuf), "   ; slots: %ld, params: %ld", (long)num_slots, (long)num_params);
    sexp_write_string(ctx, pbuf, out);
    return;
  }
  if (strstr(line, "%r8d")) {
    char pbuf[64];
    snprintf(pbuf, sizeof(pbuf), "   ; num_fvs: %ld", (long)val);
    sexp_write_string(ctx, pbuf, out);
    return;
  }
  if (strstr(line, "%ecx") || strstr(line, "%edx")) {
    char pbuf[64];
    snprintf(pbuf, sizeof(pbuf), "   ; nargs: %ld", (long)val);
    sexp_write_string(ctx, pbuf, out);
    return;
  }
  if (sexp_fixnump((sexp)val) && ((val & 1) == 1)) {
    if ((strstr(line, "%rax") || strstr(line, "%rbx") || strstr(line, "%rcx") ||
         strstr(line, "%rdx") || strstr(line, "%rsi") || strstr(line, "%rdi") ||
         strstr(line, "(%rbp)") || strstr(line, "(%rsp)")) &&
        !strstr(line, "%e") && !strstr(line, "%r8d") && !strstr(line, "%r9d")) {
      sexp_sint_t num = sexp_unbox_fixnum((sexp)val);
      if (num >= -1000000 && num <= 1000000) {
        sexp_write_string(ctx, "   ; ", out);
        sexp_write(ctx, (sexp)val, out);
        return;
      }
    }
  }
}

static void annotate_native_line (sexp ctx, sexp bc, char *line, sexp out) {
  size_t len = strlen(line);
  while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
    line[--len] = '\0';
  }

  sexp_write_string(ctx, line, out);

  const char *p = strstr(line, "$0x");
  if (p && (strstr(line, "mov") || strstr(line, "call") || strstr(line, "cmp"))) {
    uint64_t val = strtoull(p + 3, NULL, 16);
    annotate_native_value(ctx, bc, val, line, out);
  }
  sexp_newline(ctx, out);
}

static sexp disasm (sexp ctx, sexp self, sexp bc, sexp out, int depth);

static sexp disasm_native (sexp ctx, sexp self, sexp bc, sexp out, int depth) {
  unsigned char *code = sexp_bytecode_data(bc);
  size_t len = sexp_bytecode_length(bc);
  const char *tmpdir = getenv("TMPDIR");
  if (!tmpdir) tmpdir = "/tmp";
  char temp_path[512];
  snprintf(temp_path, sizeof(temp_path), "%s/chibi_disasm_XXXXXX", tmpdir);
  int fd = mkstemp(temp_path);
  if (fd < 0) {
    sexp_write_string(ctx, "      [disasm: failed to create temporary file]\n", out);
    return SEXP_VOID;
  }
  close(fd);

  char cmd[1024];
#ifdef __APPLE__
  if (write_macho_file(temp_path, code, len) < 0) {
    unlink(temp_path);
    sexp_write_string(ctx, "      [disasm: failed to write macho file]\n", out);
    return SEXP_VOID;
  }
  snprintf(cmd, sizeof(cmd), "objdump -d %s 2>/dev/null || otool -tv %s 2>/dev/null", temp_path, temp_path);
#else
  FILE *f = fopen(temp_path, "wb");
  if (!f) {
    unlink(temp_path);
    sexp_write_string(ctx, "      [disasm: failed to write binary file]\n", out);
    return SEXP_VOID;
  }
  fwrite(code, 1, len, f);
  fclose(f);
  uintptr_t addr = (uintptr_t)code;
  snprintf(cmd, sizeof(cmd), "objdump -D -b binary -m i386:x86-64 --adjust-vma=0x%lx %s 2>/dev/null", (unsigned long)addr, temp_path);
#endif

  FILE *p = popen(cmd, "r");
  if (!p) {
    unlink(temp_path);
    sexp_write_string(ctx, "      [disasm: failed to execute disassembler command]\n", out);
    return SEXP_VOID;
  }

  char buf[512];
  int printed_any = 0;
  while (fgets(buf, sizeof(buf), p)) {
    if (strstr(buf, "file format") || strstr(buf, "Disassembly of section") ||
        strstr(buf, "<.data>:") || strstr(buf, "(__TEXT,__text) section"))
      continue;
    char *s = buf;
    while (*s && isspace((unsigned char)*s)) s++;
    if (!*s) continue;
    for (int i = 0; i < depth * SEXP_DISASM_PAD_WIDTH; i++)
      sexp_write_char(ctx, ' ', out);
    annotate_native_line(ctx, bc, buf, out);
    printed_any = 1;
  }
  pclose(p);
  unlink(temp_path);

  if (!printed_any) {
    sexp_write_string(ctx, "      [disasm: disassembler returned no output or command not found]\n", out);
  }

  if (depth < SEXP_DISASM_MAX_DEPTH && sexp_bytecode_literals(bc)) {
    sexp lits = sexp_bytecode_literals(bc);
    if (sexp_vectorp(lits)) {
      for (int k = 0; k < sexp_vector_length(lits); k++) {
        sexp item = sexp_vector_ref(lits, sexp_make_fixnum(k));
        if (sexp_procedurep(item)) item = sexp_procedure_code(item);
        if (sexp_bytecodep(item) && item != bc)
          disasm(ctx, self, item, out, depth + 1);
      }
    } else if (sexp_pairp(lits)) {
      sexp a = sexp_car(lits);
      sexp b = sexp_cdr(lits);
      if (sexp_procedurep(a)) a = sexp_procedure_code(a);
      if (sexp_procedurep(b)) b = sexp_procedure_code(b);
      if (sexp_bytecodep(a) && a != bc) disasm(ctx, self, a, out, depth + 1);
      if (sexp_bytecodep(b) && b != bc) disasm(ctx, self, b, out, depth + 1);
    } else {
      sexp item = lits;
      if (sexp_procedurep(item)) item = sexp_procedure_code(item);
      if (sexp_bytecodep(item) && item != bc)
        disasm(ctx, self, item, out, depth + 1);
    }
  }

  return SEXP_VOID;
}
#endif

static sexp disasm (sexp ctx, sexp self, sexp bc, sexp out, int depth) {
  unsigned char i;
  sexp src;
#if SEXP_USE_FULL_SOURCE_INFO && ! SEXP_USE_NATIVE_X86
  sexp src_here=NULL;
  sexp_sint_t src_off=0;
#endif
#if ! SEXP_USE_NATIVE_X86
  unsigned char *ip, opcode;
  sexp tmp=NULL;
  sexp_sint_t *labels, label=1, off;
#endif

  if (sexp_idp(bc))
    bc = sexp_env_ref(ctx, sexp_context_env(ctx), bc, SEXP_FALSE);
  if (sexp_macrop(bc))
    bc = sexp_macro_proc(bc);
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
#if SEXP_USE_FULL_SOURCE_INFO && ! SEXP_USE_NATIVE_X86
  if (!(src && sexp_vectorp(src)))
    src_off = -1;
  /* if (src) sexp_write(ctx, src, out); */
#else
  if (src && sexp_pairp(src)) {
    sexp_write(ctx, sexp_car(src), out);
    sexp_write_string(ctx, ":", out);
    sexp_write(ctx, sexp_cdr(src), out);
  }
#endif
  sexp_newline(ctx, out);

#if SEXP_USE_NATIVE_X86
  return disasm_native(ctx, self, bc, out, depth);
#else
  /* build a table of labels that are jumped to */
  labels = (sexp_sint_t*)calloc(sexp_bytecode_length(bc), sizeof(sexp_sint_t));
  ip = sexp_bytecode_data(bc);
  while (ip - sexp_bytecode_data(bc) < (int)sexp_bytecode_length(bc)) {
    switch (*ip++) {
    case SEXP_OP_JUMP:
    case SEXP_OP_JUMP_UNLESS:
      off = ip - sexp_bytecode_data(bc) + ((sexp_sint_t*)ip)[0];
      if (off >= 0 && off < (int)sexp_bytecode_length(bc) && labels[off] == 0)
        labels[off] = label++;
    case SEXP_OP_CALL:
    case SEXP_OP_FCALL0:
    case SEXP_OP_FCALL1:
    case SEXP_OP_FCALL2:
    case SEXP_OP_FCALL3:
    case SEXP_OP_FCALL4:
    case SEXP_OP_FCALLN:
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
    case SEXP_OP_UVECTOR_REF:
    case SEXP_OP_UVECTOR_SET:
    case SEXP_OP_UVECTOR_LENGTH:
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
    src_off = src_off < (sexp_sint_t)sexp_vector_length(src)-1 ? src_off + 1 : -1;
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
  case SEXP_OP_UVECTOR_REF:
  case SEXP_OP_UVECTOR_SET:
  case SEXP_OP_UVECTOR_LENGTH:
    sexp_write_integer(ctx, ((sexp_sint_t*)ip)[0], out);
    ip += sizeof(sexp);
    break;
  case SEXP_OP_JUMP:
  case SEXP_OP_JUMP_UNLESS:
    sexp_write_integer(ctx, ((sexp_sint_t*)ip)[0], out);
    off = ip - sexp_bytecode_data(bc) + ((sexp_sint_t*)ip)[0];
    if (off >= 0 && off < (sexp_sint_t)sexp_bytecode_length(bc) && labels[off] > 0) {
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
  case SEXP_OP_FCALLN:
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
  if (ip - sexp_bytecode_data(bc) < (int)sexp_bytecode_length(bc))
    goto loop;

  free(labels);
  return SEXP_VOID;
#endif
}

static sexp sexp_disasm (sexp ctx, sexp self, sexp_sint_t n, sexp bc, sexp out) {
  return disasm(ctx, self, bc, out, 0);
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible_strict(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_define_foreign_param(ctx, env, "disasm", 2, (sexp_proc1)sexp_disasm, "current-output-port");
  return SEXP_VOID;
}
