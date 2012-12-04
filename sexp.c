/*  sexp.c -- standalone sexp library implementation          */
/*  Copyright (c) 2009-2012 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/sexp.h"

/* optional huffman-compressed immediate symbols */
struct sexp_huff_entry {
  unsigned char len;
  unsigned short bits;
};

#if SEXP_USE_HUFF_SYMS
#include "opt/sexp-hufftabs.c"
static struct sexp_huff_entry huff_table[] = {
#include "opt/sexp-huff.c"
};
#endif

static int sexp_initialized_p = 0;

static const char sexp_separators[] = {
  /* 1  2  3  4  5  6  7  8  9  a  b  c  d  e  f         */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, /* x0_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x1_ */
  1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, /* x2_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, /* x3_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x4_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, /* x5_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x6_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, /* x7_ */
};

static int digit_value (int c) {
  return (((c)<='9') ? ((c) - '0') : ((sexp_toupper(c) - 'A') + 10));
}

static int hex_digit (int n) {
  return ((n<=9) ? ('0' + n) : ('A' + n - 10));
}

static int is_precision_indicator(int c) {
  return c=='d' || c=='D' || c=='e' || c=='E' || c=='f' || c=='F'
    || c=='l' || c=='L' || c=='s' || c=='S';
}

int sexp_is_separator(int c) {
  return 0<c && c<0x80 && sexp_separators[c];
}

#if SEXP_USE_GLOBAL_SYMBOLS
sexp sexp_symbol_table[SEXP_SYMBOL_TABLE_SIZE];
#endif

#if ! SEXP_USE_UNSAFE_PUSH
sexp sexp_push_op(sexp ctx, sexp* loc, sexp x) {
  sexp tmp = sexp_cons(ctx, x, *loc);
  if (sexp_exceptionp(tmp)) return *loc;
  *loc = tmp;
  return tmp;
}
#endif

sexp sexp_alloc_tagged_aux(sexp ctx, size_t size, sexp_uint_t tag sexp_current_source_param) {
  sexp res = (sexp) sexp_alloc(ctx, size);
  if (res && ! sexp_exceptionp(res)) {
    sexp_pointer_tag(res) = tag;
#if SEXP_USE_TRACK_ALLOC_SOURCE
    sexp_pointer_source(res) = source;
#endif
#if SEXP_USE_HEADER_MAGIC
    sexp_pointer_magic(res) = SEXP_POINTER_MAGIC;
#endif
  }
  return res;
}

#if SEXP_USE_OBJECT_BRACE_LITERALS
sexp sexp_write_simple_object (sexp ctx, sexp self, sexp_sint_t n, sexp obj, sexp writer, sexp out) {
  sexp t, x;
  sexp_gc_var1(args);
  sexp_sint_t i, len, nulls=0;
  i = sexp_pointer_tag(obj);
  sexp_write_char(ctx, '{', out);
  if (i >= sexp_context_num_types(ctx)) {
    sexp_write_string(ctx, "invalid", out);
  } else {
    sexp_gc_preserve1(ctx, args);
    t = sexp_object_type(ctx, obj);
    sexp_write_string(ctx, sexp_string_data(sexp_type_name(t)), out);
    sexp_write_char(ctx, ' ', out);
    if (sexp_type_id(t) && sexp_truep(sexp_type_id(t))) {
      sexp_write(ctx, sexp_type_id(t), out);
    } else {
      sexp_write_char(ctx, '#', out);
      sexp_write(ctx, sexp_make_fixnum(sexp_type_tag(t)), out);
    }
    len = sexp_type_num_slots_of_object(t, obj);
    args = sexp_list1(ctx, SEXP_FALSE);
    for (i=0; i<len; i++) {
      x = sexp_slot_ref(obj, i);
      if (x) {
        if (nulls)
          while (--nulls) sexp_write_string(ctx, " #<null>", out);
        sexp_write_char(ctx, ' ', out);
        if (writer && sexp_applicablep(writer)) {
          sexp_car(args) = x;
          x = sexp_apply(ctx, writer, args);
          if (sexp_exceptionp(x)) sexp_print_exception(ctx, x, out);
        } else {
          sexp_write(ctx, sexp_slot_ref(obj, i), out);
        }
      } else {
        nulls++;
      }
    }
    sexp_gc_release1(ctx);
  }
  sexp_write_char(ctx, '}', out);
  return SEXP_VOID;
}
#else
#define sexp_write_simple_object NULL
#endif

sexp sexp_finalize_port (sexp ctx, sexp self, sexp_sint_t n, sexp port) {
  sexp res = SEXP_VOID;
  if (sexp_port_openp(port)) {
    sexp_port_openp(port) = 0;
    if (sexp_oportp(port)) sexp_flush_forced(ctx, port);
#ifndef PLAN9
    if (sexp_filenop(sexp_port_fd(port))
        && sexp_fileno_openp(sexp_port_fd(port))) {
      if (sexp_port_shutdownp(port)) {
        /* shutdown the socket if requested */
        if (sexp_iportp(port))
          shutdown(sexp_port_fileno(port), sexp_oportp(port) ? SHUT_RDWR : SHUT_RD);
        if (sexp_oportp(port))
          shutdown(sexp_port_fileno(port), SHUT_WR);
      }
    }
#endif
    if (sexp_port_stream(port) && ! sexp_port_no_closep(port))
      /* close the stream */
      fclose(sexp_port_stream(port));
      /* free the buffer if allocated */
    if (sexp_port_buf(port) && sexp_oportp(port)
        /* output string ports have list cookies */
        && (sexp_nullp(sexp_port_cookie(port)) || sexp_pairp(sexp_port_cookie(port)))
#if !SEXP_USE_STRING_STREAMS
        && !sexp_port_customp(port)
#endif
        )
      free(sexp_port_buf(port));
  }
  return res;
}

sexp sexp_finalize_fileno (sexp ctx, sexp self, sexp_sint_t n, sexp fileno) {
  if (sexp_fileno_openp(fileno) && !sexp_fileno_no_closep(fileno)) {
    sexp_fileno_openp(fileno) = 0;
    close(sexp_fileno_fd(fileno));
  }
  return SEXP_VOID;
}

#if SEXP_USE_DL
sexp sexp_finalize_dl (sexp ctx, sexp self, sexp_sint_t n, sexp dl) {
  dlclose(sexp_dl_handle(dl));
  return SEXP_VOID;
}
#endif

static struct sexp_type_struct _sexp_type_specs[] = {
  {SEXP_OBJECT, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Object", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_TYPE, sexp_offsetof(type, name), 5+SEXP_USE_DL, 5+SEXP_USE_DL, 0, 0, sexp_sizeof(type), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Type", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_FIXNUM, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Integer", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_NUMBER, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Number", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_CHAR, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Char", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_BOOLEAN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Boolean", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_PAIR, sexp_offsetof(pair, car), 2, 3, 0, 0, sexp_sizeof(pair), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Pair", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_SYMBOL, 0, 0, 0, 0, 0, sexp_sizeof(symbol)+1, sexp_offsetof(symbol, length), 1, 0, 0, 0, 0, 0, 0, (sexp)"Symbol", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_BYTES, 0, 0, 0, 0, 0, sexp_sizeof(bytes)+1, sexp_offsetof(bytes, length), 1, 0, 0, 0, 0, 0, 0, (sexp)"Byte-Vector", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#if SEXP_USE_PACKED_STRINGS
  {SEXP_STRING, 0, 0, 0, 0, 0, sexp_sizeof(string)+1, sexp_offsetof(string, length), 1, 0, 0, 0, 0, 0, 0, (sexp)"String", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#else
  {SEXP_STRING, sexp_offsetof(string, bytes), 1, 1, 0, 0, sexp_sizeof(string), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"String", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#endif
  {SEXP_VECTOR, sexp_offsetof(vector, data), 0, 0, sexp_offsetof(vector, length), 1, sexp_sizeof(vector), sexp_offsetof(vector, length), sizeof(sexp), 0, 0, 0, 0, 0, 0, (sexp)"Vector", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_FLONUM, 0, 0, 0, 0, 0, sexp_sizeof(flonum), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Flonum", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_BIGNUM, 0, 0, 0, 0, 0, sexp_sizeof(bignum), sexp_offsetof(bignum, length), sizeof(sexp_uint_t), 0, 0, 0, 0, 0, 0, (sexp)"Bignum", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#if SEXP_USE_RATIOS
  {SEXP_RATIO, sexp_offsetof(ratio, numerator), 2, 2, 0, 0, sexp_sizeof(ratio), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Ratio", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#endif
#if SEXP_USE_COMPLEX
  {SEXP_COMPLEX, sexp_offsetof(complex, real), 2, 2, 0, 0, sexp_sizeof(complex), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Complex", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#endif
  {SEXP_IPORT, sexp_offsetof(port, name), 2, 2, 0, 0, sexp_sizeof(port), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Input-Port", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, SEXP_FINALIZE_PORT},
  {SEXP_OPORT, sexp_offsetof(port, name), 2, 2, 0, 0, sexp_sizeof(port), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Output-Port", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, SEXP_FINALIZE_PORT},
  {SEXP_FILENO, 0, 0, 0, 0, 0, sexp_sizeof(fileno), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"File-Descriptor", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, SEXP_FINALIZE_FILENO},
  {SEXP_EXCEPTION, sexp_offsetof(exception, kind), 6, 6, 0, 0, sexp_sizeof(exception), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Exception", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, (sexp)sexp_write_simple_object, NULL},
  {SEXP_PROCEDURE, sexp_offsetof(procedure, bc), 2, 2, 0, 0, sexp_sizeof(procedure), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Procedure", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_MACRO, sexp_offsetof(macro, proc), 3, 3, 0, 0, sexp_sizeof(macro), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Macro", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_SYNCLO, sexp_offsetof(synclo, env), 3, 3, 0, 0, sexp_sizeof(synclo), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Syntactic-Closure", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, (sexp)sexp_write_simple_object, NULL},
  {SEXP_ENV, sexp_offsetof(env, parent), 3+SEXP_USE_RENAME_BINDINGS, 3+SEXP_USE_RENAME_BINDINGS, 0, 0, sexp_sizeof(env), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Environment", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_BYTECODE, sexp_offsetof(bytecode, name), 3, 3, 0, 0, sexp_sizeof(bytecode), offsetof(struct sexp_struct, value.bytecode.length), 1, 0, 0, 0, 0, 0, 0, (sexp)"Bytecode", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_CORE, sexp_offsetof(core, name), 1, 1, 0, 0, sexp_sizeof(core), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Core-Form", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#if SEXP_USE_DL
  {SEXP_DL, sexp_offsetof(dl, file), 1, 1, 0, 0, sexp_sizeof(dl), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Dynamic-Library", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, SEXP_FINALIZE_DL},
#endif
  {SEXP_OPCODE, sexp_offsetof(opcode, name), 11, 11, 0, 0, sexp_sizeof(opcode), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Opcode", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_LAMBDA, sexp_offsetof(lambda, name), 11, 11, 0, 0, sexp_sizeof(lambda), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Lambda", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, (sexp)sexp_write_simple_object, NULL},
  {SEXP_CND, sexp_offsetof(cnd, test), 4, 4, 0, 0, sexp_sizeof(cnd), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Conditional", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, (sexp)sexp_write_simple_object, NULL},
  {SEXP_REF, sexp_offsetof(ref, name), 3, 3, 0, 0, sexp_sizeof(ref), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Reference", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, (sexp)sexp_write_simple_object, NULL},
  {SEXP_SET, sexp_offsetof(set, var), 3, 3, 0, 0, sexp_sizeof(set), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Set!", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, (sexp)sexp_write_simple_object, NULL},
  {SEXP_SEQ, sexp_offsetof(seq, ls), 2, 2, 0, 0, sexp_sizeof(seq), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Sequence", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, (sexp)sexp_write_simple_object, NULL},
  {SEXP_LIT, sexp_offsetof(lit, value), 2, 2, 0, 0, sexp_sizeof(lit), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Literal", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, (sexp)sexp_write_simple_object, NULL},
  {SEXP_STACK, sexp_offsetof(stack, data), 1, 1, sexp_offsetof(stack, top), 1, sexp_sizeof(stack), offsetof(struct sexp_struct, value.stack.length), sizeof(sexp), 0, 0, 0, 0, 0, 0, (sexp)"Stack", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_CONTEXT, sexp_offsetof(context, stack), 12+SEXP_USE_DL, 12+SEXP_USE_DL, 0, 0, sexp_sizeof(context), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Context", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
  {SEXP_CPOINTER, sexp_offsetof(cpointer, parent), 1, 0, 0, 0, sexp_sizeof(cpointer), sexp_offsetof(cpointer, length), 1, 0, 0, 0, 0, 0, 0, (sexp)"Cpointer", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#if SEXP_USE_AUTO_FORCE
  {SEXP_PROMISE, sexp_offsetof(promise, thunk), 2, 2, 0, 0, sexp_sizeof(promise), 0, 0, 0, 0, 0, 0, 0, 0, (sexp)"Promise", SEXP_FALSE, SEXP_FALSE, NULL, SEXP_FALSE, NULL, NULL},
#endif
};

#define SEXP_INIT_NUM_TYPES (SEXP_NUM_CORE_TYPES*2)

#if SEXP_USE_TYPE_DEFS

sexp sexp_register_type_op (sexp ctx, sexp self, sexp_sint_t n, sexp name,
                            sexp parent, sexp slots,
                            sexp fb, sexp felb, sexp flb, sexp flo, sexp fls,
                            sexp sb, sexp so, sexp sc, sexp w, sexp wb, sexp wo,
                            sexp ws, sexp we, sexp p, sexp_proc2 f) {
  sexp *v1, *v2;
  sexp_gc_var2(res, type);
  sexp_uint_t i, len, num_types=sexp_context_num_types(ctx),
    type_array_size=sexp_context_type_array_size(ctx);
  sexp_gc_preserve2(ctx, res, type);
  if (num_types >= SEXP_MAXIMUM_TYPES) {
    res = sexp_user_exception(ctx, self, "register-type: exceeded maximum type limit", name);
  } else if (! sexp_stringp(name)) {
    res = sexp_type_exception(ctx, self, SEXP_STRING, name);
  } else {
    if (num_types >= type_array_size) {
      len = type_array_size*2;
      if (len > SEXP_MAXIMUM_TYPES) len = SEXP_MAXIMUM_TYPES;
      res = sexp_make_vector(ctx, sexp_make_fixnum(len), SEXP_VOID);
      if (sexp_exceptionp(res)) {
        sexp_gc_release2(ctx);
        return res;
      }
      v1 = sexp_vector_data(res);
      v2 = sexp_vector_data(sexp_global(ctx, SEXP_G_TYPES));
      for (i=0; i<num_types; i++)
        v1[i] = v2[i];
      sexp_global(ctx, SEXP_G_TYPES) = res;
    }
    sexp_type_by_index(ctx, num_types) = sexp_alloc_type(ctx, type, SEXP_TYPE);
    type = sexp_type_by_index(ctx, num_types);
    if (!sexp_exceptionp(type)) {
      sexp_pointer_tag(type) = SEXP_TYPE;
      sexp_type_tag(type) = num_types;
      sexp_type_slots(type) = slots;
      sexp_type_field_base(type) = sexp_unbox_fixnum(fb);
      sexp_type_field_eq_len_base(type) = sexp_unbox_fixnum(felb);
      sexp_type_field_len_base(type) = sexp_unbox_fixnum(flb);
      sexp_type_field_len_off(type) = sexp_unbox_fixnum(flo);
      sexp_type_field_len_scale(type) = sexp_unbox_fixnum(fls);
      sexp_type_size_base(type) = sexp_unbox_fixnum(sb);
      sexp_type_size_off(type) = sexp_unbox_fixnum(so);
      sexp_type_size_scale(type) = sexp_unbox_fixnum(sc);
      sexp_type_weak_base(type) = sexp_unbox_fixnum(w);
      sexp_type_weak_len_base(type) = sexp_unbox_fixnum(wb);
      sexp_type_weak_len_off(type) = sexp_unbox_fixnum(wo);
      sexp_type_weak_len_scale(type) = sexp_unbox_fixnum(ws);
      sexp_type_weak_len_extra(type) = sexp_unbox_fixnum(we);
      sexp_type_name(type) = name;
      sexp_type_finalize(type) = f;
      sexp_type_id(type) = SEXP_FALSE;
#if SEXP_USE_DL
      if (f) sexp_type_dl(type) = sexp_context_dl(ctx);
#endif
      sexp_type_print(type) = p;
      if (sexp_typep(parent)) {
        len = sexp_vectorp(sexp_type_cpl(parent)) ? sexp_vector_length(sexp_type_cpl(parent)) : 1;
        sexp_type_cpl(type) = sexp_make_vector(ctx, sexp_make_fixnum(len+1), SEXP_VOID);
        if (sexp_vectorp(sexp_type_cpl(parent)))
          memcpy(sexp_vector_data(sexp_type_cpl(type)),
                 sexp_vector_data(sexp_type_cpl(parent)),
                 len * sizeof(sexp));
        else
          sexp_vector_data(sexp_type_cpl(type))[len-1] = parent;
      } else {
        len = 0;
        sexp_type_cpl(type) = sexp_make_vector(ctx, SEXP_ONE, SEXP_VOID);
      }
      sexp_vector_data(sexp_type_cpl(type))[len] = type;
      sexp_type_depth(type) = len;
      sexp_global(ctx, SEXP_G_NUM_TYPES) = sexp_make_fixnum(num_types + 1);
    }
    res = type;
  }
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_register_simple_type_op (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp parent, sexp slots) {
  short i, num_slots = sexp_unbox_fixnum(sexp_length(ctx, slots));
  sexp type_size, num_slots_obj, cpl, tmp;
  if (parent && sexp_typep(parent)) {
    num_slots += sexp_unbox_fixnum(sexp_length(ctx, sexp_type_slots(parent)));
    if (sexp_vectorp((cpl = sexp_type_cpl(parent))))
      for (i=sexp_vector_length(cpl)-1; i>=0; i--) {
        tmp = sexp_vector_ref(cpl, sexp_make_fixnum(i));
        num_slots += sexp_unbox_fixnum(sexp_length(ctx, sexp_type_slots(tmp)));
      }
  }
  num_slots_obj = sexp_make_fixnum(num_slots);
  type_size = sexp_make_fixnum(sexp_sizeof_header + sizeof(sexp)*num_slots);
  return
    sexp_register_type(ctx, name, parent, slots,
                       sexp_make_fixnum(sexp_offsetof_slot0),
                       num_slots_obj, num_slots_obj, SEXP_ZERO, SEXP_ZERO,
                       type_size, SEXP_ZERO, SEXP_ZERO, SEXP_ZERO, SEXP_ZERO,
                       SEXP_ZERO, SEXP_ZERO, SEXP_ZERO,
                       sexp_type_print(sexp_type_by_index(ctx, SEXP_EXCEPTION)),
                       NULL);
}

#if SEXP_USE_OBJECT_BRACE_LITERALS
sexp sexp_lookup_type_op(sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp id) {
  int i;
  sexp res;
  const char* str = sexp_string_data(name);
  if (sexp_fixnump(id)) {
    i = sexp_unbox_fixnum(id);
    if (i < sexp_context_num_types(ctx)
        && strcmp(str, sexp_string_data(sexp_type_name_by_index(ctx, i))) == 0)
      return sexp_type_by_index(ctx, i);
    else
      return SEXP_FALSE;
  }
  for (i=sexp_context_num_types(ctx)-1; i>=0; i--)
    if (strcmp(str, sexp_string_data(sexp_type_name_by_index(ctx, i))) == 0) {
      res = sexp_type_by_index(ctx, i);
      if (sexp_stringp(id)
          && !(sexp_stringp(sexp_type_id(res))
               && strcmp(sexp_string_data(id), sexp_string_data(sexp_type_id(res))) == 0))
        return SEXP_FALSE;
      return res;
    }
  return SEXP_FALSE;
}
#endif

sexp sexp_finalize_c_type (sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
  if (sexp_cpointer_freep(obj))
    free(sexp_cpointer_value(obj));
  return SEXP_VOID;
}

#else
#define sexp_num_types SEXP_NUM_CORE_TYPES
#endif

/****************************** contexts ******************************/

void sexp_init_context_globals (sexp ctx) {
  sexp type, *vec, print=NULL;
  int i;
  sexp_context_globals(ctx)
    = sexp_make_vector(ctx, sexp_make_fixnum(SEXP_G_NUM_GLOBALS), SEXP_VOID);
#if ! SEXP_USE_GLOBAL_SYMBOLS
  sexp_global(ctx, SEXP_G_SYMBOLS) = sexp_make_vector(ctx, sexp_make_fixnum(SEXP_SYMBOL_TABLE_SIZE), SEXP_NULL);
#endif
#if SEXP_USE_FOLD_CASE_SYMS
  sexp_global(ctx, SEXP_G_FOLD_CASE_P) = sexp_make_boolean(SEXP_DEFAULT_FOLD_CASE_SYMS);
#endif
#if ! SEXP_USE_BOEHM
  sexp_global(ctx, SEXP_G_PRESERVATIVES) = SEXP_NULL;
#endif
  sexp_global(ctx, SEXP_G_OOM_ERROR) = sexp_user_exception(ctx, SEXP_FALSE, "out of memory", SEXP_NULL);
  sexp_global(ctx, SEXP_G_OOS_ERROR) = sexp_user_exception(ctx, SEXP_FALSE, "out of stack space", SEXP_NULL);
  sexp_global(ctx, SEXP_G_ABI_ERROR) = sexp_user_exception(ctx, SEXP_FALSE, "incompatible ABI", SEXP_NULL);
  sexp_global(ctx, SEXP_G_QUOTE_SYMBOL) = sexp_intern(ctx, "quote", -1);
  sexp_global(ctx, SEXP_G_QUASIQUOTE_SYMBOL) = sexp_intern(ctx, "quasiquote", -1);
  sexp_global(ctx, SEXP_G_UNQUOTE_SYMBOL) = sexp_intern(ctx, "unquote", -1);
  sexp_global(ctx, SEXP_G_UNQUOTE_SPLICING_SYMBOL) = sexp_intern(ctx, "unquote-splicing", -1);
  sexp_global(ctx, SEXP_G_CUR_IN_SYMBOL) = sexp_intern(ctx, "current-input-port", -1);
  sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL) = sexp_intern(ctx, "current-output-port", -1);
  sexp_global(ctx, SEXP_G_CUR_ERR_SYMBOL) = sexp_intern(ctx, "current-error-port", -1);
  sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL) = sexp_intern(ctx, "interaction-environment", -1);
  sexp_global(ctx, SEXP_G_EMPTY_VECTOR) = sexp_alloc_type(ctx, vector, SEXP_VECTOR);
  sexp_vector_length(sexp_global(ctx, SEXP_G_EMPTY_VECTOR)) = 0;
  sexp_global(ctx, SEXP_G_NUM_TYPES) = sexp_make_fixnum(SEXP_NUM_CORE_TYPES);
  sexp_global(ctx, SEXP_G_TYPES)
    = sexp_make_vector(ctx, sexp_make_fixnum(SEXP_INIT_NUM_TYPES), SEXP_VOID);
  vec = sexp_vector_data(sexp_global(ctx, SEXP_G_TYPES));
  for (i=0; i<SEXP_NUM_CORE_TYPES; i++) {
    type = sexp_alloc_type(ctx, type, SEXP_TYPE);
    memcpy(&(type->value), &(_sexp_type_specs[i]), sizeof(_sexp_type_specs[0]));
    vec[i] = type;
    sexp_type_name(type) = sexp_c_string(ctx, (char*)sexp_type_name(type), -1);
    if (sexp_type_print(type)) {
      if (print && ((sexp_proc1)sexp_type_print(type) == sexp_opcode_func(print)))
        sexp_type_print(type) = print;
      else
        sexp_type_print(type) = print = sexp_make_foreign(ctx, "sexp_write_simple_object", 3, 0, (sexp_proc1)sexp_type_print(type), NULL);
    }
  }
}

#if ! SEXP_USE_GLOBAL_HEAP
sexp sexp_bootstrap_context (sexp_uint_t size, sexp_uint_t max_size) {
  sexp ctx;
  sexp_heap heap;
  struct sexp_struct dummy_ctx;
  if (size < SEXP_MINIMUM_HEAP_SIZE) size = SEXP_INITIAL_HEAP_SIZE;
  heap = sexp_make_heap(sexp_heap_align(size), sexp_heap_align(max_size));
  if (!heap) return 0;
  sexp_pointer_tag(&dummy_ctx) = SEXP_CONTEXT;
  sexp_context_saves(&dummy_ctx) = NULL;
  sexp_context_heap(&dummy_ctx) = heap;
  ctx = sexp_alloc_type(&dummy_ctx, context, SEXP_CONTEXT);
  if (!ctx || sexp_exceptionp(ctx)) {
    sexp_free_heap(heap);
  } else {
    sexp_context_heap(ctx) = heap;
  }
  return ctx;
}
#endif

sexp sexp_make_context (sexp ctx, size_t size, size_t max_size) {
  sexp_gc_var1(res);
  if (ctx) sexp_gc_preserve1(ctx, res);
#if ! SEXP_USE_GLOBAL_HEAP
  if (! ctx) {
    res = sexp_bootstrap_context(size, max_size);
    if (!res || sexp_exceptionp(res)) return res;
  } else
#endif
    {
      res = sexp_alloc_type(ctx, context, SEXP_CONTEXT);
#if ! SEXP_USE_BOEHM && ! SEXP_USE_MALLOC
      sexp_context_heap(res) = sexp_context_heap(ctx);
#endif
    }
  if (!res || sexp_exceptionp(res)) return res;
  sexp_context_parent(res) = ctx;
  sexp_context_name(res) = sexp_context_specific(res) = SEXP_FALSE;
  sexp_context_saves(res) = NULL;
  sexp_context_params(res) = SEXP_NULL;
  sexp_context_last_fp(res) = 0;
  sexp_context_tracep(res) = 0;
  sexp_context_timeoutp(res) = 0;
  sexp_context_tailp(res) = 1;
#if SEXP_USE_GREEN_THREADS
  sexp_context_errorp(res) = 0;
  sexp_context_event(res) = SEXP_FALSE;
  sexp_context_refuel(res) = SEXP_DEFAULT_QUANTUM;
#endif
#if SEXP_USE_DL
  sexp_context_dl(res) = ctx ? sexp_context_dl(ctx) : SEXP_FALSE;
#endif
  if (ctx) {
    sexp_context_globals(res) = sexp_context_globals(ctx);
    sexp_context_dk(res) = sexp_context_dk(ctx);
    sexp_gc_release1(ctx);
  } else {
    sexp_init_context_globals(res);
  }
  return res;
}

#if ! SEXP_USE_GLOBAL_HEAP
void sexp_destroy_context (sexp ctx) {
  sexp_heap heap, tmp;
  size_t sum_freed;
  if (sexp_context_heap(ctx)) {
    heap = sexp_context_heap(ctx);
    sexp_markedp(ctx) = 1;
    sexp_markedp(sexp_context_globals(ctx)) = 1;
    sexp_mark(ctx, sexp_global(ctx, SEXP_G_TYPES));
    sexp_finalize(ctx);
    sexp_sweep(ctx, &sum_freed);
    sexp_finalize(ctx);
    sexp_context_heap(ctx) = NULL;
    for ( ; heap; heap=tmp) {
      tmp = heap->next;
      sexp_free_heap(heap);
    }
  }
}
#endif

/***************************** exceptions *****************************/

sexp sexp_make_exception (sexp ctx, sexp kind, sexp message, sexp irritants,
                          sexp procedure, sexp source) {
  sexp exn = sexp_alloc_type(ctx, exception, SEXP_EXCEPTION);
  sexp_exception_kind(exn) = kind;
  sexp_exception_message(exn) = message;
  sexp_exception_irritants(exn) = irritants;
  sexp_exception_procedure(exn) = procedure;
  sexp_exception_source(exn) = source;
  return exn;
}

sexp sexp_string_cat3 (sexp ctx, char *pre, char *mid, char* suf) {
  int plen=strlen(pre), mlen=strlen(mid), slen=strlen(suf);
  char *s;
  sexp str;
  str = sexp_make_string(ctx, sexp_make_fixnum(plen+mlen+slen), SEXP_VOID);
  memcpy(s=sexp_string_data(str), pre, plen);
  memcpy(s+plen, mid, mlen);
  memcpy(s+plen+mlen, suf, slen);
  return str;
}

sexp sexp_user_exception (sexp ctx, sexp self, const char *ms, sexp ir) {
  sexp res;
  sexp_gc_var3(sym, str, irr);
  sexp_gc_preserve3(ctx, sym, str, irr);
  res = sexp_make_exception(ctx, sym = sexp_intern(ctx, "user", -1),
                            str = sexp_c_string(ctx, ms, -1),
                            ((sexp_pairp(ir) || sexp_nullp(ir))
                             ? ir : (irr = sexp_list1(ctx, ir))),
                            self, SEXP_FALSE);
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_file_exception (sexp ctx, sexp self, const char *ms, sexp ir) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_user_exception(ctx, self, ms, ir);
  sexp_exception_kind(res) = sexp_intern(ctx, "file", -1);
  sexp_gc_release1(ctx);
  return res;
}

static sexp type_exception (sexp ctx, sexp self, sexp str, sexp obj, sexp src) {
  sexp_gc_var2(res, sym);
  sexp_gc_preserve2(ctx, res, sym);
  sym = sexp_intern(ctx, "type", -1);
  res = sexp_make_exception(ctx, sym, str, obj, self, src);
  sexp_exception_irritants(res)=sexp_list1(ctx, sexp_exception_irritants(res));
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_xtype_exception (sexp ctx, sexp self, const char *msg, sexp obj) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_c_string(ctx, msg, -1);
  res = type_exception(ctx, self, res, obj, SEXP_FALSE);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_type_exception (sexp ctx, sexp self, sexp_uint_t type_id, sexp obj) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_string_cat3(ctx, "invalid type, expected ",
                         sexp_string_data(sexp_type_name_by_index(ctx, type_id)), "");
  res = type_exception(ctx, self, res, obj, SEXP_FALSE);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_range_exception (sexp ctx, sexp obj, sexp start, sexp end) {
  sexp_gc_var2(res, msg);
  sexp_gc_preserve2(ctx, res, msg);
  msg = sexp_c_string(ctx, "bad index range", -1);
  res = sexp_list2(ctx, start, end);
  res = sexp_cons(ctx, obj, res);
  res = sexp_make_exception(ctx, sexp_intern(ctx, "range", -1), msg, res,
                            SEXP_FALSE, SEXP_FALSE);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_print_exception_op (sexp ctx, sexp self, sexp_sint_t n, sexp exn, sexp out) {
  sexp_gc_var2(ls, tmp);
  sexp_gc_preserve2(ctx, ls, tmp);
  if (! sexp_oportp(out))
    out = tmp = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
  sexp_write_string(ctx, "ERROR", out);
  if (sexp_exceptionp(exn)) {
    if (sexp_exception_procedure(exn)) {
      if (sexp_procedurep(sexp_exception_procedure(exn))) {
        ls = sexp_bytecode_name(
              sexp_procedure_code(sexp_exception_procedure(exn)));
        if (ls && sexp_symbolp(ls)) {
          sexp_write_string(ctx, " in ", out);
          sexp_write(ctx, ls, out);
        }
      } else if (sexp_opcodep(sexp_exception_procedure(exn))) {
        sexp_write_string(ctx, " in ", out);
        sexp_write(ctx, sexp_opcode_name(sexp_exception_procedure(exn)), out);
      }
    }
    ls = sexp_exception_source(exn);
    if ((! (ls && sexp_pairp(ls)))
        && sexp_exception_procedure(exn)
        && sexp_procedurep(sexp_exception_procedure(exn)))
      ls = sexp_bytecode_source(sexp_procedure_code(sexp_exception_procedure(exn)));
    if (ls && sexp_pairp(ls)) {
      if (sexp_fixnump(sexp_cdr(ls)) && (sexp_cdr(ls) >= SEXP_ZERO)) {
        sexp_write_string(ctx, " on line ", out);
        sexp_write(ctx, sexp_cdr(ls), out);
      }
      if (sexp_stringp(sexp_car(ls))) {
        sexp_write_string(ctx, " of file ", out);
        sexp_write_string(ctx, sexp_string_data(sexp_car(ls)), out);
      }
    }
    sexp_write_string(ctx, ": ", out);
    if (sexp_stringp(sexp_exception_message(exn)))
      sexp_write_string(ctx, sexp_string_data(sexp_exception_message(exn)), out);
    else
      sexp_write(ctx, sexp_exception_message(exn), out);
    if (sexp_exception_irritants(exn)
        && sexp_pairp(sexp_exception_irritants(exn))) {
      if (sexp_nullp(sexp_cdr(sexp_exception_irritants(exn)))) {
        sexp_write_string(ctx, ": ", out);
        sexp_write(ctx, sexp_car(sexp_exception_irritants(exn)), out);
        sexp_write_string(ctx, "\n", out);
      } else {
        sexp_write_string(ctx, "\n", out);
        for (ls=sexp_exception_irritants(exn);
             sexp_pairp(ls); ls=sexp_cdr(ls)) {
          sexp_write_string(ctx, "    ", out);
          sexp_write(ctx, sexp_car(ls), out);
          sexp_write_char(ctx, '\n', out);
        }
      }
    } else {
      sexp_write_char(ctx, '\n', out);
    }
  } else {
    sexp_write_string(ctx, ": ", out);
    if (sexp_stringp(exn))
      sexp_write_string(ctx, sexp_string_data(exn), out);
    else
      sexp_write(ctx, exn, out);
    sexp_write_char(ctx, '\n', out);
  }
  sexp_gc_release2(ctx);
  return SEXP_VOID;
}

sexp sexp_read_error (sexp ctx, const char *msg, sexp ir, sexp port) {
  sexp res;
  sexp_gc_var4(sym, name, str, irr);
  sexp_gc_preserve4(ctx, sym, name, str, irr);
  name = (sexp_port_name(port) ? sexp_port_name(port) : SEXP_FALSE);
  name = sexp_cons(ctx, name, sexp_make_fixnum(sexp_port_line(port)));
  str = sexp_c_string(ctx, msg, -1);
  irr = ((sexp_pairp(ir) || sexp_nullp(ir)) ? ir : sexp_list1(ctx, ir));
  res = sexp_make_exception(ctx, sym = sexp_intern(ctx, "read", -1),
                            str, irr, SEXP_FALSE, name);
  sexp_gc_release4(ctx);
  return res;
}

/*************************** list utilities ***************************/

sexp sexp_cons_op (sexp ctx, sexp self, sexp_sint_t n, sexp head, sexp tail) {
  sexp pair = sexp_alloc_type(ctx, pair, SEXP_PAIR);
  if (sexp_exceptionp(pair)) return pair;
  sexp_car(pair) = head;
  sexp_cdr(pair) = tail;
  sexp_pair_source(pair) = SEXP_FALSE;
  return pair;
}

sexp sexp_list2 (sexp ctx, sexp a, sexp b) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_cons(ctx, b, SEXP_NULL);
  res = sexp_cons(ctx, a, res);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_listp_op (sexp ctx, sexp self, sexp_sint_t n, sexp hare) {
  sexp turtle;
  if (! sexp_pairp(hare))
    return sexp_make_boolean(sexp_nullp(hare));
  turtle = hare;
  hare = sexp_cdr(hare);
  for ( ; sexp_pairp(hare); turtle=sexp_cdr(turtle)) {
    if (hare == turtle) return SEXP_FALSE;
    hare = sexp_cdr(hare);
    if (sexp_pairp(hare)) hare = sexp_cdr(hare);
  }
  return sexp_make_boolean(sexp_nullp(hare));
}

sexp sexp_memq_op (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp ls) {
  while (sexp_pairp(ls))
    if (x == sexp_car(ls))
      return ls;
    else
      ls = sexp_cdr(ls);
  return SEXP_FALSE;
}

sexp sexp_assq_op (sexp ctx, sexp self, sexp_sint_t n, sexp x, sexp ls) {
  while (sexp_pairp(ls))
    if (sexp_pairp(sexp_car(ls)) && (x == sexp_caar(ls)))
      return sexp_car(ls);
    else
      ls = sexp_cdr(ls);
  return SEXP_FALSE;
}

sexp sexp_reverse_op (sexp ctx, sexp self, sexp_sint_t n, sexp ls) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  for (res=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls))
    res = sexp_cons(ctx, sexp_car(ls), res);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_nreverse_op (sexp ctx, sexp self, sexp_sint_t n, sexp ls) {
  sexp a, b, tmp;
  if (ls == SEXP_NULL) return ls;
  sexp_assert_type(ctx, sexp_pairp, SEXP_PAIR, ls);
  b = ls;
  a = sexp_cdr(ls);
  sexp_cdr(b) = SEXP_NULL;
  for ( ; sexp_pairp(a); b=a, a=tmp) {
    tmp = sexp_cdr(a);
    sexp_cdr(a) = b;
  }
  return b;
}

sexp sexp_copy_list_op (sexp ctx, sexp self, sexp_sint_t n, sexp ls) {
  sexp tmp;
  sexp_gc_var1(res);
  if (! sexp_pairp(ls)) return ls;
  sexp_gc_preserve1(ctx, res);
  tmp = res = sexp_cons(ctx, sexp_car(ls), sexp_cdr(ls));
  for (ls=sexp_cdr(ls); sexp_pairp(ls); ls=sexp_cdr(ls), tmp=sexp_cdr(tmp))
    sexp_cdr(tmp) = sexp_cons(ctx, sexp_car(ls), sexp_cdr(ls));
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_append2_op (sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
  sexp_gc_var2(a1, b1);
  sexp_gc_preserve2(ctx, a1, b1);
  b1 = b;
  for (a1=sexp_reverse(ctx, a); sexp_pairp(a1); a1=sexp_cdr(a1))
    b1 = sexp_cons(ctx, sexp_car(a1), b1);
  sexp_gc_release2(ctx);
  return b1;
}

sexp sexp_length_op (sexp ctx, sexp self, sexp_sint_t n, sexp ls1) {
  sexp ls2;
  sexp_uint_t res = 1;
  if (!sexp_pairp(ls1))
    return SEXP_ZERO;
  for (ls2=sexp_cdr(ls1); sexp_pairp(ls2) && sexp_pairp(sexp_cdr(ls2));
       res+=2, ls1=sexp_cdr(ls1), ls2=sexp_cddr(ls2))
    if (ls1 == ls2)
      return SEXP_FALSE;
  return sexp_make_fixnum(res + (sexp_pairp(ls2) ? 1 : 0));
}

sexp sexp_equalp_bound (sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b, sexp bound) {
  sexp_uint_t left_size, right_size;
  sexp_sint_t i, len;
  sexp t, *p, *q;
  char *p_left, *p_right, *q_left, *q_right;

 loop:
  if (a == b)
    return bound;
  else if ((!a || !sexp_pointerp(a)) || (!b || !sexp_pointerp(b))
           || (sexp_pointer_tag(a) != sexp_pointer_tag(b)))
    return SEXP_FALSE;

  /* a and b are both pointers of the same type */
#if SEXP_USE_BIGNUMS
  if (sexp_pointer_tag(a) == SEXP_BIGNUM)
    return !sexp_bignum_compare(a, b) ? bound : SEXP_FALSE;
#endif
#if SEXP_USE_FLONUMS && ! SEXP_USE_IMMEDIATE_FLONUMS
  if (sexp_pointer_tag(a) == SEXP_FLONUM)
    return sexp_flonum_eqv(a, b) ? bound : SEXP_FALSE;
#endif
  if (sexp_unbox_fixnum(bound) < 0) /* exceeded limit */
    return bound;
  bound = sexp_fx_sub(bound, SEXP_ONE);
  t = sexp_object_type(ctx, a);
  p_left = ((char*)a) + offsetof(struct sexp_struct, value);
  p = (sexp*) (((char*)a) + sexp_type_field_base(t));
  q_left = ((char*)b) + offsetof(struct sexp_struct, value);
  q = (sexp*) (((char*)b) + sexp_type_field_base(t));
  /* if no fields, the base is value (just past the header) */
  if ((sexp)p == a) {p=(sexp*)p_left; q=(sexp*)q_left;}
  /* check preliminary non-object data */
  left_size = (char*)p - p_left;
  if ((left_size > 0) && memcmp(p_left, q_left, left_size))
    return SEXP_FALSE;
  /* check trailing non-object data */
  p_right = ((char*)p + sexp_type_num_slots_of_object(t,a)*sizeof(sexp));
  right_size = ((char*)a + sexp_type_size_of_object(t, a)) - p_right;
  if (right_size > 0) {
    q_right = ((char*)q + sexp_type_num_slots_of_object(t,b)*sizeof(sexp));
    if (right_size != ((char*)b + sexp_type_size_of_object(t, b)) - q_right)
      return SEXP_FALSE;
    if (memcmp(p_right, q_right, right_size))
      return SEXP_FALSE;
  }
  /* left and right non-object data is the same, now check eq-object slots */
  len = sexp_type_num_eq_slots_of_object(t, a);
  if (len > 0) {
    for (; len > 1; len--) {
      a = p[len-1]; b = q[len-1];
      if (a != b) {
        if ((!a || !sexp_pointerp(a)) || (!b || !sexp_pointerp(b))
            || (sexp_pointer_tag(a) != sexp_pointer_tag(b)))
          return SEXP_FALSE;
        else break;
      }
    }
    for (i=0; i<len-1; i++) {
      bound = sexp_equalp_bound(ctx, self, n, p[i], q[i], bound);
      if (sexp_not(bound)) return SEXP_FALSE;
    }
    /* tail-recurse on the last value */
    a = p[len-1]; b = q[len-1]; goto loop;
  }
  return bound;
}

sexp sexp_equalp_op (sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b) {
  return sexp_make_boolean(
    sexp_truep(sexp_equalp_bound(ctx, self, n, a, b,
                                 sexp_make_fixnum(SEXP_DEFAULT_EQUAL_BOUND))));
}

/********************* strings, symbols, vectors **********************/

sexp sexp_flonump_op (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  return sexp_make_boolean(sexp_flonump(x));
}

#if ! SEXP_USE_IMMEDIATE_FLONUMS
sexp sexp_make_flonum (sexp ctx, double f) {
  sexp x = sexp_alloc_type(ctx, flonum, SEXP_FLONUM);
  if (sexp_exceptionp(x)) return x;
  sexp_flonum_value(x) = f;
  return x;
}
#else
#if SEXP_64_BIT
float sexp_flonum_value (sexp x) {
  union sexp_flonum_conv r;
  r.bits = (sexp_uint_t)x >> 32;
  return r.flonum;
}
sexp sexp_make_flonum (sexp ctx, float f) {
  union sexp_flonum_conv x;
  x.flonum = f;
  return (sexp)(((sexp_uint_t)(x.bits) << 32) + SEXP_IFLONUM_TAG);
}
#endif
#endif

sexp sexp_make_bytes_op (sexp ctx, sexp self, sexp_sint_t n, sexp len, sexp i) {
  sexp_sint_t clen = sexp_unbox_fixnum(len);
  sexp s;
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, len);
  if (clen < 0) return sexp_xtype_exception(ctx, self, "negative length", len);
  s = sexp_alloc_atomic(ctx, sexp_sizeof(bytes)+clen+1);
  if (sexp_exceptionp(s)) return s;
  sexp_pointer_tag(s) = SEXP_BYTES;
#if SEXP_USE_HEADER_MAGIC
  sexp_pointer_magic(s) = SEXP_POINTER_MAGIC;
#endif
  sexp_bytes_length(s) = clen;
  if (sexp_fixnump(i))
    memset(sexp_bytes_data(s), sexp_unbox_fixnum(i), clen);
  sexp_bytes_data(s)[clen] = '\0';
  return s;
}

#if SEXP_USE_UTF8_STRINGS

int sexp_utf8_initial_byte_count (int c) {
  if (c < 0xC0) return 1;
  if (c < 0xE0) return 2;
  return ((c>>4)&1)+3;
}

int sexp_utf8_char_byte_count (int c) {
  if (c < 0x80) return 1;
  if (c < 0x800) return 2;
  if (c < 0x10000) return 3;
  return 4;
}

int sexp_string_utf8_length (unsigned char *p, int len) {
  unsigned char *q = p+len;
  int i;
  for (i=0; p<q; i++)
    p += sexp_utf8_initial_byte_count(*p);
  return i;
}

char* sexp_string_utf8_prev (unsigned char *p) {
  while ((*--p)>>6 == 2)
    ;
  return (char*)p;
}

sexp sexp_string_utf8_ref (sexp ctx, sexp str, sexp i) {
  unsigned char *p=(unsigned char*)sexp_string_data(str) + sexp_unbox_fixnum(i);
  if (*p < 0x80)
    return sexp_make_character(*p);
  else if ((*p < 0xC0) || (*p > 0xF7))
    return sexp_user_exception(ctx, NULL, "string-ref: invalid utf8 byte", i);
  else if (*p < 0xE0)
    return sexp_make_character(((p[0]&0x3F)<<6) + (p[1]&0x3F));
  else if (*p < 0xF0)
    return sexp_make_character(((p[0]&0x1F)<<12) + ((p[1]&0x3F)<<6) + (p[2]&0x3F));
  else
    return sexp_make_character(((p[0]&0x0F)<<16) + ((p[1]&0x3F)<<6) + ((p[2]&0x3F)<<6) + (p[2]&0x3F));
}

void sexp_utf8_encode_char (unsigned char* p, int len, int c) {
  switch (len) {
  case 4:  *p++ = (0xF0 + ((c)>>18)); *p++ = (0x80 + ((c>>12)&0x3F));
    *p++ = (0x80 + ((c>>6)&0x3F));    *p = (0x80 + (c&0x3F)); break;
  case 3:  *p++ = (0xE0 + ((c)>>12)); *p++ = (0x80 + ((c>>6)&0x3F));
    *p = (0x80 + (c&0x3F)); break;
  case 2:  *p++ = (0xC0 + ((c)>>6));  *p = (0x80 + (c&0x3F)); break;
  default: *p = c; break;
  }
}

sexp sexp_string_index_to_offset (sexp ctx, sexp self, sexp_sint_t n, sexp str, sexp index) {
  sexp_sint_t i, j, limit;
  unsigned char *p;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, index);
  p = (unsigned char*)sexp_string_data(str);
  limit = sexp_string_length(str);
  for (j=0, i=sexp_unbox_fixnum(index); i>0 && j<limit; i--)
    j += sexp_utf8_initial_byte_count(p[j]);
  if (i != 0)
    return sexp_user_exception(ctx, self, "string-index->offset: index out of range", index);
  return sexp_make_fixnum(j);
}

#endif

sexp sexp_make_string_op (sexp ctx, sexp self, sexp_sint_t n, sexp len, sexp ch)
{
  sexp i = (sexp_charp(ch) ? sexp_make_fixnum(sexp_unbox_character(ch)) : ch);
#if SEXP_USE_PACKED_STRINGS
  sexp b;
#else
  sexp_gc_var2(b, s);
#endif
#if SEXP_USE_UTF8_STRINGS
  int j, clen;
  if (sexp_charp(ch) && (sexp_unbox_character(ch) >= 0x80)) {
    sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, len);
    clen = sexp_utf8_char_byte_count(sexp_unbox_character(ch));
    b = sexp_make_bytes_op(ctx, self, n,
                           sexp_fx_mul(len, sexp_make_fixnum(clen)), SEXP_VOID);
    if (sexp_exceptionp(b)) return b;
    for (j=0; j<sexp_unbox_fixnum(len); j++)
      sexp_utf8_encode_char((unsigned char*)sexp_bytes_data(b)+(j*clen), clen,
                            sexp_unbox_character(ch));
  } else
#endif
  b = sexp_make_bytes_op(ctx, self, n, len, i);
  if (sexp_exceptionp(b)) return b;
#if SEXP_USE_PACKED_STRINGS
  sexp_pointer_tag(b) = SEXP_STRING;
  return b;
#else
  sexp_gc_preserve2(ctx, b, s);
  s = sexp_alloc_type(ctx, string, SEXP_STRING);
  sexp_string_bytes(s) = b;
  sexp_string_offset(s) = 0;
  sexp_string_length(s) = sexp_bytes_length(b);
  sexp_gc_release2(ctx);
  return s;
#endif
}

sexp sexp_c_string (sexp ctx, const char *str, sexp_sint_t slen) {
  sexp_sint_t len = ((slen >= 0) ? slen : strlen(str));
  sexp s = sexp_make_string(ctx, sexp_make_fixnum(len), SEXP_VOID);
  if (sexp_exceptionp(s)) return s;
  memcpy(sexp_string_data(s), str, len);
  sexp_string_data(s)[len] = '\0';
  return s;
}

sexp sexp_substring_op (sexp ctx, sexp self, sexp_sint_t n, sexp str, sexp start, sexp end) {
  sexp res;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, start);
  if (sexp_not(end))
    end = sexp_make_fixnum(sexp_string_length(str));
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, end);
  if ((sexp_unbox_fixnum(start) < 0)
      || (sexp_unbox_fixnum(start) > sexp_string_length(str))
      || (sexp_unbox_fixnum(end) < 0)
      || (sexp_unbox_fixnum(end) > sexp_string_length(str))
      || (end < start))
    return sexp_range_exception(ctx, str, start, end);
  res = sexp_make_string(ctx, sexp_fx_sub(end, start), SEXP_VOID);
  memcpy(sexp_string_data(res),
         sexp_string_data(str)+sexp_unbox_fixnum(start),
         sexp_string_length(res));
  sexp_string_data(res)[sexp_string_length(res)] = '\0';
  return res;
}

sexp sexp_subbytes_op (sexp ctx, sexp self, sexp_sint_t n, sexp vec, sexp start, sexp end) {
  sexp res;
  sexp_gc_var1(str);
  sexp_assert_type(ctx, sexp_bytesp, SEXP_BYTES, vec);
  sexp_gc_preserve1(ctx, str);
#if SEXP_USE_PACKED_STRINGS
  str = sexp_c_string(ctx, sexp_bytes_data(vec), sexp_bytes_length(vec));
#else
  str = sexp_alloc_type(ctx, string, SEXP_STRING);
  sexp_string_bytes(str) = vec;
  sexp_string_offset(str) = 0;
  sexp_string_length(str) = sexp_bytes_length(vec);
#endif
  res = sexp_substring_op(ctx, self, n, str, start, end);
  if (!sexp_exceptionp(res))
    res = sexp_string_to_bytes(ctx, res);
  sexp_gc_release1(ctx);
  return res;
}

#if SEXP_USE_UTF8_STRINGS
sexp sexp_utf8_substring_op (sexp ctx, sexp self, sexp_sint_t n, sexp str, sexp start, sexp end) {
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, start);
  start = sexp_string_index_to_offset(ctx, self, n, str, start);
  if (sexp_exceptionp(start)) return start;
  if (sexp_fixnump(end)) {
    end = sexp_string_index_to_offset(ctx, self, n, str, end);
    if (sexp_exceptionp(end)) return end;
  }
  return sexp_substring_op(ctx, self, n, str, start, end);
}
#endif

sexp sexp_string_concatenate_op (sexp ctx, sexp self, sexp_sint_t n, sexp str_ls, sexp sep) {
  sexp res, ls;
  sexp_uint_t len=0, i=0, sep_len=0;
  char *p, *csep=NULL;
  for (ls=str_ls; sexp_pairp(ls); ls=sexp_cdr(ls), i++)
    if (! sexp_stringp(sexp_car(ls)))
      return sexp_type_exception(ctx, self, SEXP_STRING, sexp_car(ls));
    else
      len += sexp_string_length(sexp_car(ls));
  if ((i > 0) && sexp_stringp(sep) && ((sep_len=sexp_string_length(sep)) > 0)) {
    csep = sexp_string_data(sep);
    len += sep_len*(i-1);
  }
  res = sexp_make_string(ctx, sexp_make_fixnum(len), SEXP_VOID);
  p = sexp_string_data(res);
  for (ls=str_ls; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    len = sexp_string_length(sexp_car(ls));
    memcpy(p, sexp_string_data(sexp_car(ls)), len);
    p += len;
    if (sep_len && sexp_pairp(sexp_cdr(ls))) {
      memcpy(p, csep, sep_len);
      p += sep_len;
    }
  }
  *p = '\0';
  return res;
}

#define FNV_PRIME 16777619
#define FNV_OFFSET_BASIS 2166136261uL

#if SEXP_USE_HASH_SYMS

static sexp_uint_t sexp_string_hash(const char *str, sexp_sint_t len,
                                    sexp_uint_t acc) {
  for ( ; len; len--) {acc *= FNV_PRIME; acc ^= *str++;}
  return acc;
}

#endif

sexp sexp_intern(sexp ctx, const char *str, sexp_sint_t len) {
#if SEXP_USE_HUFF_SYMS
  struct sexp_huff_entry he;
  sexp_sint_t space, newbits;
  char c;
#endif
  sexp ls, tmp;
  sexp_gc_var1(sym);
  sexp_sint_t bucket=0;
#if (SEXP_USE_HASH_SYMS || SEXP_USE_HUFF_SYMS)
  sexp_sint_t i=0, res=FNV_OFFSET_BASIS;
  const char *p=str;
#endif

  if (len < 0) len = strlen(str);

#if SEXP_USE_HUFF_SYMS
  res = 0;
  space = 3;
  if (len == 0) goto normal_intern;
  for ( ; i<len; i++, p++) {
    c = *p;
    if ((unsigned char)c <= 32 || (unsigned char)c > 127 || c == '\\')
      goto normal_intern;
    he = huff_table[(unsigned char)c];
    newbits = he.len;
    if ((space+newbits) > (sizeof(sexp)*8))
      goto normal_intern;
    res |= (((sexp_uint_t) he.bits) << space);
    space += newbits;
  }
  return (sexp) (res + SEXP_ISYMBOL_TAG);

 normal_intern:
#endif
#if SEXP_USE_HASH_SYMS
  bucket = (sexp_string_hash(p, len-i, res) % SEXP_SYMBOL_TABLE_SIZE);
#endif
  for (ls=sexp_context_symbols(ctx)[bucket]; sexp_pairp(ls); ls=sexp_cdr(ls))
    if ((sexp_lsymbol_length(tmp=sexp_car(ls)) == len)
        && ! strncmp(str, sexp_lsymbol_data(tmp), len))
      return sexp_car(ls);

  /* not found, make a new symbol */
  sexp_gc_preserve1(ctx, sym);
  sym = sexp_c_string(ctx, str, len);
  if (sexp_exceptionp(sym)) return sym;
#if ! SEXP_USE_PACKED_STRINGS
  sym = sexp_string_bytes(sym);
#endif
  sexp_pointer_tag(sym) = SEXP_SYMBOL;
  sexp_push(ctx, sexp_context_symbols(ctx)[bucket], sym);
  sexp_gc_release1(ctx);
  return sym;
}

sexp sexp_string_to_symbol_op (sexp ctx, sexp self, sexp_sint_t n, sexp str) {
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  return sexp_intern(ctx, sexp_string_data(str), sexp_string_length(str));
}

sexp sexp_make_vector_op (sexp ctx, sexp self, sexp_sint_t n, sexp len, sexp dflt) {
  sexp vec, *x;
  int i, clen = sexp_unbox_fixnum(len);
  if (! clen) return sexp_global(ctx, SEXP_G_EMPTY_VECTOR);
  vec = sexp_alloc_tagged(ctx, sexp_sizeof(vector) + clen*sizeof(sexp),
                          SEXP_VECTOR);
  if (sexp_exceptionp(vec)) return vec;
  x = sexp_vector_data(vec);
  for (i=0; i<clen; i++)
    x[i] = dflt;
  sexp_vector_length(vec) = clen;
  return vec;
}

sexp sexp_list_to_vector_op (sexp ctx, sexp self, sexp_sint_t n, sexp ls) {
  int i;
  sexp x, *elts, vec = sexp_make_vector(ctx, sexp_length(ctx, ls), SEXP_VOID);
  if (sexp_exceptionp(vec)) return vec;
  elts = sexp_vector_data(vec);
  for (i=0, x=ls; sexp_pairp(x); i++, x=sexp_cdr(x))
    elts[i] = sexp_car(x);
  return vec;
}

sexp sexp_make_cpointer (sexp ctx, sexp_uint_t type_id, void *value,
                         sexp parent, int freep) {
  sexp ptr;
  if (! value) return SEXP_FALSE;
  ptr = sexp_alloc_type(ctx, cpointer, type_id);
  if (sexp_exceptionp(ptr)) return ptr;
  sexp_freep(ptr) = freep;
  sexp_cpointer_value(ptr) = value;
  sexp_cpointer_parent(ptr) = parent;
  sexp_cpointer_length(ptr) = 0;
  return ptr;
}

/************************ reading and writing *************************/

#if SEXP_USE_STRING_STREAMS

#define SEXP_INIT_STRING_PORT_SIZE 128

#if SEXP_BSD

#define sexp_streamp(vec) sexp_vectorp(vec)

#define sexp_stream_ctx(vec) sexp_vector_ref((sexp)vec, SEXP_ZERO)
#define sexp_stream_buf(vec) sexp_vector_ref((sexp)vec, SEXP_ONE)
#define sexp_stream_size(vec) sexp_vector_ref((sexp)vec, SEXP_TWO)
#define sexp_stream_pos(vec) sexp_vector_ref((sexp)vec, SEXP_THREE)

#define sexp_stream_ctx_set(vec, x) sexp_vector_set((sexp)vec, SEXP_ZERO, x)
#define sexp_stream_buf_set(vec, x) sexp_vector_set((sexp)vec, SEXP_ONE, x)
#define sexp_stream_size_set(vec, x) sexp_vector_set((sexp)vec, SEXP_TWO, x)
#define sexp_stream_pos_set(vec, x) sexp_vector_set((sexp)vec, SEXP_THREE, x)

static int sstream_read (void *vec, char *dst, int n) {
  sexp_uint_t len = sexp_unbox_fixnum(sexp_stream_size(vec));
  sexp_uint_t pos = sexp_unbox_fixnum(sexp_stream_pos(vec));
  if (pos >= len) return 0;
  if (n > (len - pos)) n = (len - pos);
  memcpy(dst, sexp_string_data(sexp_stream_buf(vec))+pos, n);
  sexp_stream_pos_set(vec, sexp_make_fixnum(pos + n));
  return n;
}

static int sstream_write (void *vec, const char *src, int n) {
  sexp_uint_t len, pos, newpos;
  sexp newbuf;
  len = sexp_unbox_fixnum(sexp_stream_size(vec));
  pos = sexp_unbox_fixnum(sexp_stream_pos(vec));
  newpos = pos+n;
  if (newpos >= len) {
    newbuf = sexp_make_string(sexp_stream_ctx(vec),
                              sexp_make_fixnum(newpos*2),
                              SEXP_VOID);
    memcpy(sexp_string_data(newbuf),
           sexp_string_data(sexp_stream_buf(vec)),
           pos);
    sexp_stream_buf_set(vec, newbuf);
    sexp_stream_size_set(vec, sexp_make_fixnum(newpos*2));
  }
  memcpy(sexp_string_data(sexp_stream_buf(vec))+pos, src, n);
  sexp_stream_pos_set(vec, sexp_make_fixnum(newpos));
  return n;
}

static off_t sstream_seek (void *vec, off_t offset, int whence) {
  sexp_sint_t pos;
  if (whence == SEEK_SET) {
    pos = offset;
  } else if (whence == SEEK_CUR) {
    pos = sexp_unbox_fixnum(sexp_stream_pos(vec)) + offset;
  } else {                      /* SEEK_END */
    pos = sexp_unbox_fixnum(sexp_stream_size(vec)) + offset;
  }
  sexp_stream_pos_set(vec, sexp_make_fixnum(pos));
  return pos;
}

sexp sexp_make_input_string_port_op (sexp ctx, sexp self, sexp_sint_t n, sexp str) {
  FILE *in;
  sexp res;
  sexp_gc_var1(cookie);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_gc_preserve1(ctx, cookie);
  cookie = sexp_make_vector(ctx, sexp_make_fixnum(4), SEXP_VOID);
  sexp_stream_ctx_set(cookie, ctx);
  sexp_stream_buf_set(cookie, str);
  sexp_stream_size_set(cookie, sexp_make_fixnum(sexp_string_length(str)));
  sexp_stream_pos_set(cookie, SEXP_ZERO);
  in = funopen(cookie, &sstream_read, NULL, &sstream_seek, NULL);
  res = sexp_make_input_port(ctx, in, SEXP_FALSE);
  sexp_port_cookie(res) = cookie;
  sexp_port_binaryp(res) = 0;
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_make_output_string_port_op (sexp ctx, sexp self, sexp_sint_t n) {
  FILE *out;
  sexp res, size;
  sexp_gc_var1(cookie);
  sexp_gc_preserve1(ctx, cookie);
  size = sexp_make_fixnum(SEXP_INIT_STRING_PORT_SIZE);
  cookie = sexp_make_vector(ctx, sexp_make_fixnum(4), SEXP_VOID);
  sexp_stream_ctx_set(cookie, ctx);
  sexp_stream_buf_set(cookie, sexp_make_string(ctx, size, SEXP_VOID));
  sexp_stream_size_set(cookie, size);
  sexp_stream_pos_set(cookie, SEXP_ZERO);
  out = funopen(cookie, NULL, &sstream_write, &sstream_seek, NULL);
  res = sexp_make_output_port(ctx, out, SEXP_FALSE);
  sexp_port_cookie(res) = cookie;
  sexp_port_binaryp(res) = 0;
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_get_output_string_op (sexp ctx, sexp self, sexp_sint_t n, sexp port) {
  sexp cookie;
  sexp_assert_type(ctx, sexp_oportp, SEXP_OPORT, port);
  cookie = sexp_port_cookie(port);
  if (!sexp_streamp(cookie))
    return sexp_xtype_exception(ctx, self, "not a string port", port);
  fflush(sexp_port_stream(port));
  return sexp_substring(ctx,
                        sexp_stream_buf(cookie),
                        SEXP_ZERO,
                        sexp_stream_pos(cookie));
}

#else  /* SEXP_USE_STRING_STREAMS && ! SEXP_BSD */

sexp sexp_make_input_string_port_op (sexp ctx, sexp self, sexp_sint_t n, sexp str) {
  FILE *in;
  sexp res;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  if (sexp_string_length(str) == 0)
    in = fopen("/dev/null", "r");
  else
    in = fmemopen(sexp_string_data(str), sexp_string_length(str), "r");
  if (in) {
    res = sexp_make_input_port(ctx, in, SEXP_FALSE);
    if (sexp_string_length(str) == 0)
      sexp_port_name(res) = sexp_c_string(ctx, "/dev/null", -1);
    sexp_port_cookie(res) = str;  /* for gc preservation */
    sexp_port_binaryp(res) = 0;
  } else {
    res = sexp_user_exception(ctx, SEXP_FALSE, "couldn't open string", str);
  }
  return res;
}

sexp sexp_make_output_string_port_op (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res = sexp_make_output_port(ctx, NULL, SEXP_FALSE);
  sexp_port_stream(res)
    = open_memstream(&sexp_port_buf(res), &sexp_port_size(res));
  sexp_port_binaryp(res) = 0;
  sexp_port_cookie(res) = SEXP_STRING_OPORT;
  return res;
}

sexp sexp_get_output_string_op (sexp ctx, sexp self, sexp_sint_t n, sexp port) {
  sexp_assert_type(ctx, sexp_oportp, SEXP_OPORT, port);
  if (sexp_port_cookie(port) != SEXP_STRING_OPORT)
    return sexp_xtype_exception(ctx, self, "not an output string port", port);
  fflush(sexp_port_stream(port));
  return sexp_c_string(ctx, sexp_port_buf(port), sexp_port_size(port));
}

#endif

sexp sexp_open_input_file_descriptor (sexp ctx, sexp self, sexp_sint_t n, sexp fileno, sexp shutdownp) {
  sexp res;
  FILE* in;
  sexp_assert_type(ctx, sexp_filenop, SEXP_FILENO, fileno);
  in = fdopen(sexp_fileno_fd(fileno), "r");
  if (!in) return sexp_file_exception(ctx, SEXP_FALSE, "couldn't open fileno", fileno);
  res = sexp_make_input_port(ctx, in, SEXP_FALSE);
  if (!sexp_exceptionp(res)) {
    sexp_port_binaryp(res) = 1;
    sexp_port_shutdownp(res) = sexp_truep(shutdownp);
  }
  return res;
}

sexp sexp_open_output_file_descriptor (sexp ctx, sexp self, sexp_sint_t n, sexp fileno, sexp shutdownp) {
  sexp res;
  FILE* out;
  sexp_assert_type(ctx, sexp_filenop, SEXP_FILENO, fileno);
  out = fdopen(sexp_fileno_fd(fileno), "w");
  if (!out) return sexp_user_exception(ctx, SEXP_FALSE, "couldn't open fileno", fileno);
  res = sexp_make_output_port(ctx, out, SEXP_FALSE);
  if (!sexp_exceptionp(res)) sexp_port_shutdownp(res) = sexp_truep(shutdownp);
  return res;
}

#else  /* ! SEXP_USE_STRING_STREAMS */

int sexp_buffered_read_char (sexp ctx, sexp p) {
  sexp_gc_var1(tmp);
  int res = 0;
  if (sexp_port_offset(p) < sexp_port_size(p)) {
    return ((unsigned char*)sexp_port_buf(p))[sexp_port_offset(p)++];
  } else if (sexp_port_stream(p)) {
    res = fread(sexp_port_buf(p), 1, SEXP_PORT_BUFFER_SIZE, sexp_port_stream(p));
    if (res >= 0) {
      sexp_port_offset(p) = 0;
      sexp_port_size(p) = res;
      res = ((sexp_port_offset(p) < sexp_port_size(p))
             ? ((unsigned char*)sexp_port_buf(p))[sexp_port_offset(p)++] : EOF);
    }
  } else if (sexp_filenop(sexp_port_fd(p))) {
    res = read(sexp_port_fileno(p), sexp_port_buf(p), SEXP_PORT_BUFFER_SIZE);
    if (res >= 0) {
      sexp_port_offset(p) = 0;
      sexp_port_size(p) = res;
      res = ((sexp_port_offset(p) < sexp_port_size(p))
             ? ((unsigned char*)sexp_port_buf(p))[sexp_port_offset(p)++] : EOF);
    }
  } else if (sexp_port_customp(p)) {
    sexp_gc_preserve1(ctx, tmp);
    tmp = sexp_list2(ctx, SEXP_ZERO, sexp_make_fixnum(SEXP_PORT_BUFFER_SIZE));
    tmp = sexp_cons(ctx, sexp_port_buffer(p), tmp);
    tmp = sexp_apply(ctx, sexp_port_reader(p), tmp);
    if (sexp_fixnump(tmp) && sexp_unbox_fixnum(tmp) > 0) {
      sexp_port_offset(p) = 0;
      sexp_port_size(p) = sexp_unbox_fixnum(tmp);
      res = ((sexp_port_offset(p) < sexp_port_size(p))
             ? ((unsigned char*)sexp_port_buf(p))[sexp_port_offset(p)++] : EOF);
    } else {
      res = EOF;
      sexp_port_size(p) = 0;
    }
    sexp_gc_release1(ctx);
  } else {
    res = EOF;
  }
  return res;
}

int sexp_buffered_write_char (sexp ctx, int c, sexp p) {
  int res;
  if (sexp_port_offset(p)+1 >= sexp_port_size(p))
    if ((res = sexp_buffered_flush(ctx, p, 0)))
      return res;
  sexp_port_buf(p)[sexp_port_offset(p)++] = c;
  return 0;
}

int sexp_buffered_write_string_n (sexp ctx, const char *str,
                                  sexp_uint_t len, sexp p) {
  int diff, res, written=0;
  while (sexp_port_offset(p)+len >= sexp_port_size(p)) {
    diff = sexp_port_size(p) - sexp_port_offset(p);
    memcpy(sexp_port_buf(p)+sexp_port_offset(p), str, diff);
    sexp_port_offset(p) = sexp_port_size(p);
    if ((res = sexp_buffered_flush(ctx, p, 0)))
      return written + diff;
    written += sexp_port_size(p);
    str += diff;
    len -= diff;
  }
  memcpy(sexp_port_buf(p)+sexp_port_offset(p), str, len);
  sexp_port_offset(p) += len;
  return written + len;
}

int sexp_buffered_write_string (sexp ctx, const char *str, sexp p) {
  return sexp_buffered_write_string_n(ctx, str, strlen(str), p);
}

int sexp_buffered_flush (sexp ctx, sexp p, int forcep) {
  long res = 0, off;
  sexp_gc_var1(tmp);
  if (!sexp_oportp(p) || (!forcep && !sexp_port_openp(p)))
    return -1;
  off = sexp_port_offset(p);
  if (sexp_port_stream(p)) {
    if (off > 0) fwrite(sexp_port_buf(p), 1, off, sexp_port_stream(p));
    res = fflush(sexp_port_stream(p));
  } else if (sexp_filenop(sexp_port_fd(p))) {
    if (off > 0)
      res = write(sexp_fileno_fd(sexp_port_fd(p)), sexp_port_buf(p), off);
    if (res < off) {
      if (res > 0) {
        memmove(sexp_port_buf(p), sexp_port_buf(p) + res, off - res);
        sexp_port_offset(p) = off - res;
      }
      res = -1;
    } else {
      sexp_port_offset(p) = 0;
      res = 0;
    }
  } else if (!sexp_port_openp(p)) {
    return -1;
  } else if (sexp_port_offset(p) > 0) {
    sexp_gc_preserve1(ctx, tmp);
    if (sexp_port_customp(p)) {   /* custom port */
      tmp = sexp_list2(ctx, SEXP_ZERO, sexp_make_fixnum(sexp_port_offset(p)));
      tmp = sexp_cons(ctx, sexp_port_buffer(p), tmp);
      tmp = sexp_apply(ctx, sexp_port_writer(p), tmp);
      res = (sexp_fixnump(tmp) && sexp_unbox_fixnum(tmp) > 0) ? 0 : -1;
    } else {                      /* string port */
      tmp = sexp_c_string(ctx, sexp_port_buf(p), off);
      if (tmp && sexp_stringp(tmp)) {
        sexp_push(ctx, sexp_port_cookie(p), tmp);
        sexp_port_offset(p) = 0;
        res = 0;
      } else {
        res = -1;
      }
    }
    sexp_gc_release1(ctx);
  }
  return res;
}

sexp sexp_make_input_string_port_op (sexp ctx, sexp self, sexp_sint_t n, sexp str) {
  sexp res;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  res = sexp_make_input_port(ctx, NULL, SEXP_FALSE);
  if (sexp_exceptionp(res)) return res;
  sexp_port_cookie(res) = str;
  sexp_port_buf(res) = sexp_string_data(str);
  sexp_port_offset(res) = 0;
  sexp_port_size(res) = sexp_string_length(str);
  sexp_port_binaryp(res) = 0;
  return res;
}

sexp sexp_make_output_string_port_op (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res = sexp_make_output_port(ctx, NULL, SEXP_FALSE);
  if (sexp_exceptionp(res)) return res;
  sexp_port_buf(res) = (char*) sexp_malloc(SEXP_PORT_BUFFER_SIZE);
  if (!sexp_port_buf(res)) {
    res = sexp_global(ctx, SEXP_G_OOM_ERROR);
  } else {
    sexp_port_size(res) = SEXP_PORT_BUFFER_SIZE;
    sexp_port_offset(res) = 0;
    sexp_port_cookie(res) = SEXP_NULL;
    sexp_port_binaryp(res) = 0;
  }
  return res;
}

sexp sexp_get_output_string_op (sexp ctx, sexp self, sexp_sint_t n, sexp out) {
  sexp res;
  sexp_gc_var3(ls, rev, tmp);
  sexp_assert_type(ctx, sexp_oportp, SEXP_OPORT, out);
  sexp_gc_preserve3(ctx, ls, rev, tmp);
  if (sexp_port_offset(out) > 0) {
    tmp = sexp_c_string(ctx, sexp_port_buf(out), sexp_port_offset(out));
    rev = sexp_cons(ctx, tmp, sexp_port_cookie(out));
  } else {
    rev = sexp_port_cookie(out);
  }
  ls = sexp_reverse(ctx, rev);
  res = SEXP_FALSE;
  for (tmp = ls; sexp_pairp(tmp); tmp = sexp_cdr(tmp))
    if (!sexp_stringp(sexp_car(tmp)))
      res = sexp_xtype_exception(ctx, self, "not an output string port", out);
  if (!sexp_nullp(tmp))
    res = sexp_xtype_exception(ctx, self, "not an output string port", out);
  if (!sexp_exceptionp(res))
    res = sexp_string_concatenate(ctx, ls, SEXP_FALSE);
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_open_input_file_descriptor (sexp ctx, sexp self, sexp_sint_t n, sexp fileno, sexp shutdownp) {
  sexp_gc_var2(res, str);
  sexp_assert_type(ctx, sexp_filenop, SEXP_FILENO, fileno);
  if (sexp_fileno_fd(fileno) < 0)
    return sexp_file_exception(ctx, self, "invalid file descriptor", fileno);
  sexp_gc_preserve2(ctx, res, str);
  str = sexp_make_string(ctx, sexp_make_fixnum(SEXP_PORT_BUFFER_SIZE), SEXP_VOID);
  res = sexp_make_input_string_port(ctx, str);
  if (!sexp_exceptionp(res)) {
    sexp_port_fd(res) = fileno;
    sexp_port_offset(res) = SEXP_PORT_BUFFER_SIZE;
    sexp_port_binaryp(res) = 1;
    sexp_port_shutdownp(res) = sexp_truep(shutdownp);
  }
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_open_output_file_descriptor (sexp ctx, sexp self, sexp_sint_t n, sexp fileno, sexp shutdownp) {
  sexp res = sexp_open_input_file_descriptor(ctx, self, n, fileno, shutdownp);
  if (!sexp_exceptionp(res)) {
    sexp_pointer_tag(res) = SEXP_OPORT;
    sexp_port_offset(res) = 0;
  }
  return res;
}

#endif  /* ! SEXP_USE_STRING_STREAMS */

sexp sexp_make_fileno_op (sexp ctx, sexp self, sexp_sint_t n, sexp fd, sexp no_closep) {
  sexp res;
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, fd);
  if (sexp_unbox_fixnum(fd) < 0) return SEXP_FALSE;
  res = sexp_alloc_type(ctx, fileno, SEXP_FILENO);
  if (!sexp_exceptionp(res)) {
    sexp_fileno_fd(res) = sexp_unbox_fixnum(fd);
    sexp_fileno_no_closep(res) = sexp_truep(no_closep);
  }
  return res;
}

sexp sexp_make_input_port (sexp ctx, FILE* in, sexp name) {
  sexp p = sexp_alloc_type(ctx, port, SEXP_IPORT);
  if (sexp_exceptionp(p)) return p;
  sexp_port_stream(p) = in;
  sexp_port_name(p) = name;
  sexp_port_line(p) = 1;
  sexp_port_flags(p) = SEXP_PORT_UNKNOWN_FLAGS;
  sexp_port_buf(p) = NULL;
  sexp_port_fd(p) = SEXP_FALSE;
  sexp_port_openp(p) = 1;
  sexp_port_bidirp(p) = 0;
  sexp_port_binaryp(p) = 1;
  sexp_port_shutdownp(p) = 0;
  sexp_port_no_closep(p) = 0;
  sexp_port_sourcep(p) = 0;
  sexp_port_blockedp(p) = 0;
#if SEXP_USE_FOLD_CASE_SYMS
  sexp_port_fold_casep(p) = sexp_truep(sexp_global(ctx, SEXP_G_FOLD_CASE_P));
#endif
  sexp_port_cookie(p) = SEXP_VOID;
  return p;
}

sexp sexp_make_output_port (sexp ctx, FILE* out, sexp name) {
  sexp p = sexp_make_input_port(ctx, out, name);
  if (sexp_exceptionp(p)) return p;
  sexp_pointer_tag(p) = SEXP_OPORT;
  return p;
}

sexp sexp_make_non_null_input_port (sexp ctx, FILE* in, sexp name) {
  if (!in) return sexp_user_exception(ctx, SEXP_FALSE, "null input-port", name);
  return sexp_make_input_port(ctx, in, name);
}

sexp sexp_make_non_null_output_port (sexp ctx, FILE* out, sexp name) {
  if (!out) return sexp_user_exception(ctx, SEXP_FALSE, "null output-port", name);
  return sexp_make_output_port(ctx, out, name);
}

#if SEXP_USE_BIDIRECTIONAL_PORTS
sexp sexp_make_non_null_input_output_port (sexp ctx, FILE* io, sexp name) {
  sexp res;
  if (!io) return sexp_user_exception(ctx, SEXP_FALSE, "null input-output-port", name);
  res = sexp_make_input_port(ctx, io, name);
  if (sexp_portp(res)) sexp_port_bidirp(res) = 1;
  return res;
}
#else
#define sexp_make_non_null_input_output_port sexp_make_non_null_input_port
#endif

sexp sexp_port_outputp_op (sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
  return sexp_make_boolean(sexp_oportp(obj));
}

sexp sexp_port_binaryp_op (sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
  return sexp_make_boolean(sexp_portp(obj) && sexp_port_binaryp(obj));
}

sexp sexp_port_openp_op (sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
  return sexp_make_boolean(sexp_portp(obj) && sexp_port_openp(obj));
}

#if SEXP_USE_FOLD_CASE_SYMS
sexp sexp_get_port_fold_case (sexp ctx, sexp self, sexp_sint_t n, sexp in) {
  sexp_assert_type(ctx, sexp_iportp, SEXP_IPORT, in);
  return sexp_make_boolean(sexp_port_fold_casep(in));
}
sexp sexp_set_port_fold_case (sexp ctx, sexp self, sexp_sint_t n, sexp in, sexp x) {
  sexp_assert_type(ctx, sexp_iportp, SEXP_IPORT, in);
  sexp_assert_type(ctx, sexp_booleanp, SEXP_BOOLEAN, x);
  sexp_port_fold_casep(in) = sexp_truep(x);
  return SEXP_VOID;
}
#endif

#if SEXP_USE_GREEN_THREADS
int sexp_maybe_block_port (sexp ctx, sexp in, int forcep) {
  sexp f;
  int c;
  if ((sexp_port_stream(in) || sexp_filenop(sexp_port_fd(in)))
      && sexp_port_fileno(in) >= 0) {
    if (sexp_port_flags(in) == SEXP_PORT_UNKNOWN_FLAGS)
      sexp_port_flags(in) = fcntl(sexp_port_fileno(in), F_GETFL);
    if (sexp_port_flags(in) & O_NONBLOCK) {
      if (!forcep
          && (((c = sexp_read_char(ctx, in)) == EOF)
              && sexp_port_stream(in)
              && (sexp_port_stream(in) ? ferror(sexp_port_stream(in)) : 1)
              && (errno == EAGAIN))) {
        if (sexp_port_stream(in))
          clearerr(sexp_port_stream(in));
        f = sexp_global(ctx, SEXP_G_THREADS_BLOCKER);
        if (sexp_opcodep(f)) {
          ((sexp_proc2)sexp_opcode_func(f))(ctx, f, 1, in);
          return 1;
        }
      }
      if (!forcep) sexp_push_char(ctx, c, in);
      sexp_port_blockedp(in) = 1;
      fcntl(sexp_port_fileno(in), F_SETFL, sexp_port_flags(in) & ~O_NONBLOCK);
    }
  }
  return 0;
}

int sexp_maybe_block_output_port (sexp ctx, sexp out) {
  if (sexp_port_stream(out) && sexp_port_fileno(out) >= 0) {
    if (sexp_port_flags(out) == SEXP_PORT_UNKNOWN_FLAGS)
      sexp_port_flags(out) = fcntl(sexp_port_fileno(out), F_GETFL);
    if (sexp_port_flags(out) & O_NONBLOCK) {
      sexp_port_blockedp(out) = 1;
      fcntl(sexp_port_fileno(out), F_SETFL, sexp_port_flags(out) & ~O_NONBLOCK);
      return 1;
    }
  }
  return 0;
}

void sexp_maybe_unblock_port (sexp ctx, sexp port) {
  if (sexp_port_blockedp(port)) {
    sexp_port_blockedp(port) = 0;
    fcntl(sexp_port_fileno(port), F_SETFL, sexp_port_flags(port));
  }
}
#endif

#if SEXP_USE_GREEN_THREADS
static int sexp_fileno_ready_p (int fd) {
  struct pollfd pfd;
  if (fd < 0) return -1;
  pfd.fd = fd;
  pfd.events = POLLIN;
  return poll(&pfd, 1, 0) == 1;
}

static int sexp_stream_ready_p (FILE* in) {
  int flags = fcntl(fileno(in), F_GETFL), res;
  if (! (flags & O_NONBLOCK)) fcntl(fileno(in), F_SETFL, flags & O_NONBLOCK);
  res = getc(in);
  if (! (flags & O_NONBLOCK)) fcntl(fileno(in), F_SETFL, flags);
  return (res == EOF) ? feof(in) : 1;
}
#endif

sexp sexp_char_ready_p (sexp ctx, sexp self, sexp_sint_t n, sexp in) {
  sexp_assert_type(ctx, sexp_iportp, SEXP_IPORT, in);
  if (!sexp_port_openp(in))
    return SEXP_FALSE;
#if !SEXP_USE_STRING_STREAMS
  if (sexp_port_buf(in))
    if (sexp_port_offset(in) < sexp_port_size(in)
        || (!sexp_filenop(sexp_port_fd(in)) && !sexp_port_stream(in)))
      return SEXP_TRUE;
#endif
#if SEXP_USE_GREEN_THREADS     /* maybe not just when threads are enabled */
  if (sexp_filenop(sexp_port_fd(in)))
    return sexp_make_boolean(sexp_fileno_ready_p(sexp_port_fileno(in)));
  else if (sexp_port_stream(in))
    return sexp_make_boolean(sexp_stream_ready_p(sexp_port_stream(in)));
#endif
  /* for custom ports and unthreaded compiles we just return true for now */
  return SEXP_TRUE;
}

#define NUMBUF_LEN 32

static struct {const char* name; char ch;} sexp_char_names[] = {
  {"newline", '\n'},
  {"return", '\r'},
  {"space", ' '},
  {"tab", '\t'},
#if SEXP_USE_EXTENDED_CHAR_NAMES
  {"alarm", '\a'},
  {"backspace", '\b'},
  {"delete", 127},
  {"escape", '\e'},
  {"null", 0},
#endif
};

#define sexp_num_char_names (sizeof(sexp_char_names)/sizeof(sexp_char_names[0]))

sexp sexp_apply_writer(sexp ctx, sexp writer, sexp obj, sexp out) {
  sexp res;
  sexp_gc_var1(args);
  sexp_gc_preserve1(ctx, args);
  args = sexp_list2(ctx, NULL, out);
  args = sexp_cons(ctx, obj, args);
  res = sexp_apply(ctx, writer, args);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_write_one (sexp ctx, sexp obj, sexp out) {
#if SEXP_USE_HUFF_SYMS
  unsigned long res;
#endif
  unsigned long len, c;
  long i=0;
#if SEXP_USE_FLONUMS
  double f;
#endif
  sexp x, *elts;
  char *str=NULL, numbuf[NUMBUF_LEN];

  if (! obj) {
    sexp_write_string(ctx, "#<null>", out); /* shouldn't happen */
  } else if (sexp_pointerp(obj)) {
    switch (sexp_pointer_tag(obj)) {
    case SEXP_PAIR:
      sexp_write_char(ctx, '(', out);
      sexp_write_one(ctx, sexp_car(obj), out);
      for (x=sexp_cdr(obj); sexp_pairp(x); x=sexp_cdr(x)) {
        sexp_write_char(ctx, ' ', out);
        sexp_write_one(ctx, sexp_car(x), out);
      }
      if (! sexp_nullp(x)) {
        sexp_write_string(ctx, " . ", out);
        sexp_write_one(ctx, x, out);
      }
      sexp_write_char(ctx, ')', out);
      break;
    case SEXP_VECTOR:
      len = sexp_vector_length(obj);
      elts = sexp_vector_data(obj);
      if (len == 0) {
        sexp_write_string(ctx, "#()", out);
      } else {
        sexp_write_string(ctx, "#(", out);
        sexp_write_one(ctx, elts[0], out);
        for (i=1; i<len; i++) {
          sexp_write_char(ctx, ' ', out);
          sexp_write_one(ctx, elts[i], out);
        }
        sexp_write_char(ctx, ')', out);
      }
      break;
#if SEXP_USE_FLONUMS
#if ! SEXP_USE_IMMEDIATE_FLONUMS
    case SEXP_FLONUM:
      f = sexp_flonum_value(obj);
#if SEXP_USE_INFINITIES
      if (isinf(f) || isnan(f)) {
        numbuf[0] = (isinf(f) && f < 0 ? '-' : '+');
        strcpy(numbuf+1, isinf(f) ? "inf.0" : "nan.0");
      } else
#endif
      {
        i = snprintf(numbuf, NUMBUF_LEN, "%.15g", f);
        if (f == trunc(f) && ! strchr(numbuf, '.')) {
          numbuf[i++] = '.'; numbuf[i++] = '0'; numbuf[i++] = '\0';
        }
      }
      sexp_write_string(ctx, numbuf, out);
      break;
#endif
#endif
    case SEXP_PROCEDURE:
      sexp_write_string(ctx, "#<procedure ", out);
      x = sexp_bytecode_name(sexp_procedure_code(obj));
      sexp_write_one(ctx, sexp_synclop(x) ? sexp_synclo_expr(x): x, out);
#if SEXP_USE_DEBUG_VM
      if (sexp_procedure_source(obj)) {
        sexp_write_string(ctx, " ", out);
        sexp_write(ctx, sexp_procedure_source(obj), out);
      }
#endif
      sexp_write_string(ctx, ">", out);
      break;
    case SEXP_TYPE:
      sexp_write_string(ctx, "#<type ", out);
      sexp_write(ctx, sexp_type_name(obj), out);
      sexp_write_string(ctx, ">", out);
      break;
    case SEXP_STRING:
      sexp_write_char(ctx, '"', out);
      i = sexp_string_length(obj);
      str = sexp_string_data(obj);
      for ( ; i>0; str++, i--) {
        switch (str[0]) {
        case '\\': sexp_write_string(ctx, "\\\\", out); break;
        case '"': sexp_write_string(ctx, "\\\"", out); break;
        case '\a': sexp_write_string(ctx, "\\a", out); break;
        case '\b': sexp_write_string(ctx, "\\b", out); break;
        case '\n': sexp_write_string(ctx, "\\n", out); break;
        case '\r': sexp_write_string(ctx, "\\r", out); break;
        case '\t': sexp_write_string(ctx, "\\t", out); break;
        default:
          if (str[0] < ' ' && str[0] >= 0) {
            sexp_write_string(ctx, "\\x", out);
            sexp_write_char(ctx, hex_digit(str[0]>>4), out);
            sexp_write_char(ctx, hex_digit(str[0]&0x0F), out);
            sexp_write_char(ctx, ';', out);
          } else {
            sexp_write_char(ctx, str[0], out);
          }
        }
      }
      sexp_write_char(ctx, '"', out);
      break;
    case SEXP_SYMBOL:
      str = sexp_lsymbol_data(obj);
      c = sexp_lsymbol_length(obj) > 0 ? EOF : '|';
      for (i=sexp_lsymbol_length(obj)-1; i>=0; i--)
        if (str[i] <= ' ' || str[i] == '\\' || sexp_is_separator(str[i])) c = '|';
      if (c!=EOF) sexp_write_char(ctx, c, out);
      for (i=sexp_lsymbol_length(obj); i>0; str++, i--) {
        if (str[0] == '\\') sexp_write_char(ctx, '\\', out);
        sexp_write_char(ctx, str[0], out);
      }
      if (c!=EOF) sexp_write_char(ctx, c, out);
      break;
#if SEXP_USE_BIGNUMS
    case SEXP_BIGNUM:
      sexp_write_bignum(ctx, obj, out, 10);
      break;
#endif
#if SEXP_USE_RATIOS
    case SEXP_RATIO:
      sexp_write(ctx, sexp_ratio_numerator(obj), out);
      sexp_write_char(ctx, '/', out);
      sexp_write(ctx, sexp_ratio_denominator(obj), out);
      break;
#endif
#if SEXP_USE_COMPLEX
    case SEXP_COMPLEX:
      sexp_write(ctx, sexp_complex_real(obj), out);
      if (!sexp_negativep(sexp_complex_imag(obj))
          && !sexp_infp(sexp_complex_imag(obj)))
        sexp_write_char(ctx, '+', out);
      if (sexp_complex_imag(obj) == SEXP_NEG_ONE)
        sexp_write_char(ctx, '-', out);
      else if (sexp_complex_imag(obj) != SEXP_ONE)
        sexp_write(ctx, sexp_complex_imag(obj), out);
      sexp_write_char(ctx, 'i', out);
      break;
#endif
    case SEXP_OPCODE:
      sexp_write_string(ctx, "#<opcode ", out);
      sexp_write(ctx, sexp_opcode_name(obj), out);
      sexp_write_char(ctx, '>', out);
      break;
#if SEXP_USE_BYTEVECTOR_LITERALS
    case SEXP_BYTES:
      sexp_write_string(ctx, "#u8(", out);
      str = sexp_bytes_data(obj);
      len = sexp_bytes_length(obj);
      for (i=0; i<len; i++) {
        if (i!=0) sexp_write_char(ctx, ' ', out);
        sexp_write(ctx, sexp_make_fixnum(((unsigned char*)str)[i]), out);
      }
      sexp_write_char(ctx, ')', out);
      break;
#endif
    case SEXP_FILENO:
      sexp_write_string(ctx, "#<fileno ", out);
      sexp_write(ctx, sexp_make_fixnum(sexp_fileno_fd(obj)), out);
      sexp_write_char(ctx, '>', out);
      break;
    default:
      i = sexp_pointer_tag(obj);
      if (i < 0 || i >= sexp_context_num_types(ctx)) {
        sexp_write_string(ctx, "#<invalid type tag: ", out);
        sexp_write(ctx, sexp_make_fixnum(i), out);
        sexp_write_char(ctx, '>', out);
      } else {
        x = sexp_type_by_index(ctx, i);
#if SEXP_USE_TYPE_PRINTERS
        if (sexp_type_print(x)) {
          x = sexp_apply_writer(ctx, sexp_type_print(x), obj, out);
          if (sexp_exceptionp(x)) return x;
        } else {
#endif
          sexp_write_string(ctx, "#<", out);
          if (sexp_stringp(sexp_type_name(x)))
            sexp_write_string(ctx, sexp_string_data(sexp_type_name(x)), out);
          else
            sexp_write(ctx, sexp_type_name(x), out);
          sexp_write_char(ctx, '>', out);
#if SEXP_USE_TYPE_PRINTERS
        }
#endif
      }
      break;
    }
  } else if (sexp_fixnump(obj)) {
    snprintf(numbuf, NUMBUF_LEN, "%ld", (long)sexp_unbox_fixnum(obj));
    sexp_write_string(ctx, numbuf, out);
#if SEXP_USE_IMMEDIATE_FLONUMS
  } else if (sexp_flonump(obj)) {
    f = sexp_flonum_value(obj);
#if SEXP_USE_INFINITIES
    if (isinf(f) || isnan(f)) {
      numbuf[0] = (isinf(f) && f < 0 ? '-' : '+');
      strcpy(numbuf+1, isinf(f) ? "inf.0" : "nan.0");
    } else
#endif
    {
      i = snprintf(numbuf, NUMBUF_LEN, "%.8g", f);
      if (f == trunc(f) && ! strchr(numbuf, '.')) {
        numbuf[i++] = '.'; numbuf[i++] = '0'; numbuf[i++] = '\0';
      }
    }
    sexp_write_string(ctx, numbuf, out);
#endif
  } else if (sexp_charp(obj)) {
    sexp_write_string(ctx, "#\\", out);
    for (i=0; i < sexp_num_char_names; i++) {
      if (sexp_unbox_character(obj) == sexp_char_names[i].ch) {
        sexp_write_string(ctx, sexp_char_names[i].name, out);
        break;
      }
    }
    if (i >= sexp_num_char_names) {
      if ((33 <= sexp_unbox_character(obj))
          && (sexp_unbox_character(obj) < 127)) {
        sexp_write_char(ctx, sexp_unbox_character(obj), out);
      } else {
        sexp_write_string(ctx, "x", out);
        c = sexp_unbox_character(obj);
        if (c >= 0x100) {
          if (c >= 0x10000) {
            sexp_write_char(ctx, hex_digit((c>>20)&0x0F), out);
            sexp_write_char(ctx, hex_digit((c>>16)&0x0F), out);
          }
          sexp_write_char(ctx, hex_digit((c>>12)&0x0F), out);
          sexp_write_char(ctx, hex_digit((c>>8)&0x0F), out);
        }
        sexp_write_char(ctx, hex_digit((c>>4)&0x0F), out);
        sexp_write_char(ctx, hex_digit(c&0x0F), out);
      }
    }
  } else if (sexp_symbolp(obj)) {

#if SEXP_USE_HUFF_SYMS
    if (sexp_isymbolp(obj)) {
      c = ((sexp_uint_t)obj)>>3;
      while (c) {
#include "opt/sexp-unhuff.c"
        sexp_write_char(ctx, res, out);
      }
    }
#endif

  } else {
    switch ((sexp_uint_t) obj) {
    case (sexp_uint_t) SEXP_NULL:
      sexp_write_string(ctx, "()", out); break;
    case (sexp_uint_t) SEXP_TRUE:
      sexp_write_string(ctx, "#t", out); break;
    case (sexp_uint_t) SEXP_FALSE:
      sexp_write_string(ctx, "#f", out); break;
    case (sexp_uint_t) SEXP_EOF:
      sexp_write_string(ctx, "#<eof>", out); break;
    case (sexp_uint_t) SEXP_UNDEF:
    case (sexp_uint_t) SEXP_VOID:
      sexp_write_string(ctx, "#<undef>", out); break;
    default:
      sexp_write_string(ctx, "#<invalid immediate: ", out);
      sexp_write(ctx, sexp_make_fixnum(obj), out);
      sexp_write_char(ctx, '>', out);
    }
  }
  return SEXP_VOID;
}

sexp sexp_write_op (sexp ctx, sexp self, sexp_sint_t n, sexp obj, sexp out) {
  sexp res;
  sexp_assert_type(ctx, sexp_oportp, SEXP_OPORT, out);
#if SEXP_USE_GREEN_THREADS
  sexp_maybe_block_output_port(ctx, out);
#endif
  res = sexp_write_one(ctx, obj, out);
#if SEXP_USE_GREEN_THREADS
  sexp_maybe_unblock_port(ctx, out);
#endif
  return res;
}

#if SEXP_USE_UTF8_STRINGS
int sexp_write_utf8_char (sexp ctx, int c, sexp out) {
  unsigned char buf[8];
  int len = sexp_utf8_char_byte_count(c), i;
  sexp_utf8_encode_char(buf, len, c);
  buf[len] = 0;
  i = sexp_write_char(ctx, buf[0], out);
  if (i == EOF) return EOF;
  sexp_write_string(ctx, (char*)buf+1, out);
  return len;
}
#endif

sexp sexp_flush_output_op (sexp ctx, sexp self, sexp_sint_t n, sexp out) {
  int res;
  sexp_assert_type(ctx, sexp_oportp, SEXP_OPORT, out);
  res = sexp_flush_forced(ctx, out);
  if (res == EOF) {
#if SEXP_USE_GREEN_THREADS
    if (sexp_port_stream(out) && ferror(sexp_port_stream(out)) && (errno == EAGAIN))
      return sexp_global(ctx, SEXP_G_IO_BLOCK_ERROR);
#endif
    return SEXP_FALSE;
  }
  return SEXP_TRUE;
}

#define INIT_STRING_BUFFER_SIZE 128

sexp sexp_read_string (sexp ctx, sexp in, int sentinel) {
#if SEXP_USE_UTF8_STRINGS
  int len;
#endif
  int c, i=0;
  sexp_sint_t size=INIT_STRING_BUFFER_SIZE;
  char initbuf[INIT_STRING_BUFFER_SIZE];
  char *buf=initbuf, *tmp;
  sexp res = SEXP_FALSE;

  for (c = sexp_read_char(ctx, in); c != sentinel; c = sexp_read_char(ctx, in)) {
    if (c == '\\') {
      c = sexp_read_char(ctx, in);
      switch (c) {
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break; 
      case 't': c = '\t'; break;
      case 'x':
        res = sexp_read_number(ctx, in, 16);
        if (sexp_fixnump(res)) {
          c = sexp_read_char(ctx, in);
          if (c != ';') {
#if SEXP_USE_ESCAPE_REQUIRES_TRAILING_SEMI_COLON
            res = sexp_read_error(ctx, "missing ; in \\x escape", SEXP_NULL, in);
#else
            sexp_push_char(ctx, c, in);
#endif
          }
          c = sexp_unbox_fixnum(res);
#if SEXP_USE_UTF8_STRINGS
          if ((unsigned)c > 0x80) {
            len = sexp_utf8_char_byte_count(c);
            sexp_utf8_encode_char((unsigned char*)buf + i, len, c);
            i += len;
            goto maybe_expand;
          }
#endif
        }
        break;
#if SEXP_USE_ESCAPE_NEWLINE
      default:
        if (isspace(c)) while (isspace(c) && c!=EOF) c=sexp_read_char(ctx, in);
#endif
      }
      if (sexp_exceptionp(res)) break;
    } else if (c == '\n') {
      sexp_port_line(in)++;
    } else if (c == EOF) {
      res = sexp_read_error(ctx, "premature end of string", SEXP_NULL, in);
      break;
    }
    buf[i++] = c;
  maybe_expand:
    if (i+4 >= size) {       /* expand buffer w/ malloc(), later free() it */
      tmp = (char*) sexp_malloc(size*2);
      if (!tmp) {res = sexp_global(ctx, SEXP_G_OOM_ERROR); break;}
      memcpy(tmp, buf, i);
      if (size != INIT_STRING_BUFFER_SIZE) free(buf);
      buf = tmp;
      size *= 2;
    }
  }

  if (!sexp_exceptionp(res)) {
    buf[i] = '\0';
    res = sexp_c_string(ctx, buf, i);
  }
  if (size != INIT_STRING_BUFFER_SIZE) free(buf);
  return res;
}

sexp sexp_read_symbol (sexp ctx, sexp in, int init, int internp) {
  int c, i=0, size=INIT_STRING_BUFFER_SIZE;
  char initbuf[INIT_STRING_BUFFER_SIZE];
  char *buf=initbuf, *tmp;
  sexp res=SEXP_VOID;
#if SEXP_USE_FOLD_CASE_SYMS
  int foldp = sexp_port_fold_casep(in);
  init = (foldp ? sexp_tolower(init) : init);
#endif

  if (init != EOF)
    buf[i++] = init;

  for (c = sexp_read_char(ctx, in); ; c = sexp_read_char(ctx, in)) {
#if SEXP_USE_FOLD_CASE_SYMS
    if (foldp) c = sexp_tolower(c);
#endif
    if (c == '\\') c = sexp_read_char(ctx, in);
    if (c == EOF || sexp_is_separator(c)) {
      sexp_push_char(ctx, c, in);
      break;
    }
    buf[i++] = c;
    if (i >= size) {       /* expand buffer w/ malloc(), later free() it */
      tmp = (char*) sexp_malloc(size*2);
      if (!tmp) {res = sexp_global(ctx, SEXP_G_OOM_ERROR); break;}
      memcpy(tmp, buf, i);
      if (size != INIT_STRING_BUFFER_SIZE) free(buf);
      buf = tmp;
      size *= 2;
    }
  }

  if (!sexp_exceptionp(res)) {
    buf[i] = '\0';
    res = (internp ? sexp_intern(ctx, buf, i) : sexp_c_string(ctx, buf, i));
  }
  if (size != INIT_STRING_BUFFER_SIZE) free(buf);
  return res;
}

#if SEXP_USE_COMPLEX
sexp sexp_make_complex (sexp ctx, sexp real, sexp image) {
  sexp res = sexp_alloc_type(ctx, complex, SEXP_COMPLEX);
  sexp_complex_real(res) = real;
  sexp_complex_imag(res) = image;
  return res;
}

sexp sexp_complex_normalize (sexp cpx) {
  return sexp_complexp(cpx)
    && (sexp_complex_imag(cpx) == SEXP_ZERO
        /* Only normalize for an exact zero imaginary part.  */
        /* || (sexp_flonump(sexp_complex_imag(cpx)) */
        /*     && sexp_flonum_value(sexp_complex_imag(cpx)) == 0.0) */
        )
    ? sexp_complex_real(cpx) : cpx;
}

sexp sexp_read_complex_tail (sexp ctx, sexp in, sexp real) {
  int c = sexp_read_char(ctx, in), c2;
  sexp default_real = SEXP_ZERO;
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_VOID;
  if (c=='i' || c=='I') {       /* trailing i, no sign */
  trailing_i:
    c = sexp_read_char(ctx, in);
    if (c=='n' || c=='N') {
      res = sexp_read_symbol(ctx, in, c, 1);
      if (res == sexp_intern(ctx, "nf.0i", -1))
        real = res = sexp_make_flonum(ctx, real == SEXP_ONE ? sexp_pos_infinity : sexp_neg_infinity);
      else
        goto invalid;
    } else if ((c!=EOF) && ! sexp_is_separator(c)) {
    invalid:
      res = sexp_read_error(ctx, "invalid complex numeric syntax", sexp_make_character(c), in);
    } else
      sexp_push_char(ctx, c, in);
    if (!sexp_exceptionp(res))
      res = sexp_make_complex(ctx, default_real, real);  /* NNNNi has 0 real */
  } else {                      /* trailing + or - */
    c2 = sexp_read_char(ctx, in);
    if (c2=='i' || c2=='I') {
      default_real = real;
      real = (c=='-') ? SEXP_NEG_ONE : SEXP_ONE;
      goto trailing_i;
    } else {
      sexp_push_char(ctx, c2, in);
      /* read imaginary part */
      if (c=='-') sexp_push_char(ctx, c, in);
      res = sexp_read_number(ctx, in, 10);
      if (sexp_complexp(res)) {
        if (sexp_complex_real(res) == SEXP_ZERO)
          sexp_complex_real(res) = real;
        else
          res = sexp_read_error(ctx, "multiple real parts of complex", res, in);
      } else if ((res == SEXP_ZERO)
                 || (sexp_flonump(res) && sexp_flonum_value(res) == 0.0)) {
        res = sexp_make_complex(ctx, real, res);
      } else {                    /* found trailing +/-NNNN with no i */
        res = sexp_exceptionp(res) ? res
          : sexp_read_error(ctx, "missing imaginary part of complex", res, in);
      }
    }
  }
  sexp_gc_release1(ctx);
  return sexp_complex_normalize(res);
}

#if SEXP_USE_MATH
sexp sexp_read_polar_tail (sexp ctx, sexp in, sexp magnitude) {
  sexp_gc_var2(res, theta);
  sexp_gc_preserve2(ctx, res, theta);
  theta = sexp_read_number(ctx, in, 10);
  if (sexp_exceptionp(theta)) {
    res = theta;
  } else if (sexp_complexp(theta) || !sexp_numberp(theta)) {
    res = sexp_read_error(ctx, "invalid polar numeric syntax", theta, in);
  } else {
    res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ZERO);
    sexp_complex_real(res) = sexp_cos(ctx, NULL, 1, theta);
    sexp_complex_real(res) = sexp_mul(ctx, magnitude, sexp_complex_real(res));
    sexp_complex_imag(res) = sexp_sin(ctx, NULL, 1, theta);
    sexp_complex_imag(res) = sexp_mul(ctx, magnitude, sexp_complex_imag(res));
  }
  sexp_gc_release2(ctx);
  return sexp_complex_normalize(res);
}
#endif
#endif

sexp sexp_read_float_tail (sexp ctx, sexp in, double whole, int negp) {
  int c, c2;
  sexp exponent=SEXP_VOID;
  double val=0.0, scale=0.1, e=0.0;
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  for (c=sexp_read_char(ctx, in); sexp_isdigit(c);
       c=sexp_read_char(ctx, in), scale*=0.1)
    val += digit_value(c)*scale;
#if SEXP_USE_PLACEHOLDER_DIGITS
  for (; c==SEXP_PLACEHOLDER_DIGIT; c=sexp_read_char(ctx, in), scale*=0.1)
    val += sexp_placeholder_digit_value(10)*scale;
#endif
  val += whole;
  if (negp) val *= -1;
  if (is_precision_indicator(c)) {
    c2 = sexp_read_char(ctx, in);
    if (c2 != '+') sexp_push_char(ctx, c2, in);
    exponent = sexp_read_number(ctx, in, 10);
    if (sexp_exceptionp(exponent)) {
      sexp_gc_release1(ctx);
      return exponent;
    }
#if SEXP_USE_COMPLEX
    if (sexp_complexp(exponent)) {
      res = exponent;
      exponent = (sexp_complex_real(res) == SEXP_ZERO ? sexp_complex_imag(res) : sexp_complex_real(res));
    }
#endif
    e = (sexp_fixnump(exponent) ? sexp_unbox_fixnum(exponent)
         : sexp_flonump(exponent) ? sexp_flonum_value(exponent) : 0.0);
#if SEXP_USE_COMPLEX
    if (sexp_complexp(res)) {
      if (sexp_complex_real(res) == SEXP_ZERO) {
        sexp_complex_imag(res) = sexp_make_flonum(ctx, val * pow(10, e));
      } else {
        sexp_complex_real(res) = sexp_make_flonum(ctx, val * pow(10, e));
      }
      sexp_gc_release1(ctx);
      return res;
    }
#endif
  }
  if (e != 0.0) val *= pow(10, e);
#if SEXP_USE_FLONUMS
  res = sexp_make_flonum(ctx, val);
#else
  res = sexp_make_fixnum((sexp_uint_t)val);
#endif
  if (!is_precision_indicator(c)) {
#if SEXP_USE_COMPLEX
    if (c=='i' || c=='i' || c=='+' || c=='-') {
      sexp_push_char(ctx, c, in);
      res = sexp_read_complex_tail(ctx, in, res);
    } else
#endif
    if ((c!=EOF) && ! sexp_is_separator(c))
      res = sexp_read_error(ctx, "invalid numeric syntax",
                            sexp_make_character(c), in);
    else
      sexp_push_char(ctx, c, in);
  }
  sexp_gc_release1(ctx);
  return res;
}

#if SEXP_USE_RATIOS
sexp sexp_make_ratio (sexp ctx, sexp num, sexp den) {
  sexp res = sexp_alloc_type(ctx, ratio, SEXP_RATIO);
  sexp_ratio_numerator(res) = num;
  sexp_ratio_denominator(res) = den;
  return res;
}

sexp sexp_ratio_normalize (sexp ctx, sexp rat, sexp in) {
  sexp tmp;
  sexp_gc_var2(num, den);
  num = sexp_ratio_numerator(rat), den = sexp_ratio_denominator(rat);
  if (den == SEXP_ZERO)
    return sexp_read_error(ctx, "zero denominator in ratio", rat, in);
  else if (num == SEXP_ZERO)
    return SEXP_ZERO;
  sexp_gc_preserve2(ctx, num, den);
  while (den != SEXP_ZERO) {
    tmp = sexp_remainder(ctx, num, den);
    if (sexp_exceptionp(tmp)) {
      sexp_gc_release2(ctx);
      return tmp;
    }
    num = den, den = tmp;
  }
  sexp_ratio_denominator(rat)
    = den = sexp_quotient(ctx, sexp_ratio_denominator(rat), num);
  sexp_ratio_numerator(rat)
    = sexp_quotient(ctx, sexp_ratio_numerator(rat), num);
  if (sexp_exact_negativep(sexp_ratio_denominator(rat))) {
    sexp_negate(sexp_ratio_numerator(rat));
    sexp_negate(sexp_ratio_denominator(rat));
  }
  sexp_ratio_numerator(rat) = sexp_bignum_normalize(sexp_ratio_numerator(rat));
  sexp_ratio_denominator(rat) = sexp_bignum_normalize(sexp_ratio_denominator(rat));
  sexp_gc_release2(ctx);
  return (sexp_ratio_denominator(rat) == SEXP_ONE) ? sexp_ratio_numerator(rat)
    : rat;
}
#endif

sexp sexp_read_number (sexp ctx, sexp in, int base) {
  sexp_sint_t val = 0, tmp = -1;
  int c, digit, negativep = 0;
#if SEXP_USE_PLACEHOLDER_DIGITS
  double whole = 0.0, scale = 0.1;
#endif
  sexp_gc_var2(res, den);

  c = sexp_read_char(ctx, in);
  if (c == '-') {
    negativep = 1;
    c = sexp_read_char(ctx, in);
  }

#if SEXP_USE_COMPLEX
  if (c == 'i' || c == 'I') val = 1;
#endif

  for ( ; sexp_isxdigit(c); c=sexp_read_char(ctx, in)) {
    digit = digit_value(c);
    if ((digit < 0) || (digit >= base))
      break;
    tmp = val * base + digit;
#if SEXP_USE_BIGNUMS
    if ((tmp < val) || (tmp > SEXP_MAX_FIXNUM)) {
      sexp_push_char(ctx, c, in);
      return sexp_read_bignum(ctx, in, val, (negativep ? -1 : 1), base);
    }
#endif
    val = tmp;
  }

#if SEXP_USE_PLACEHOLDER_DIGITS
  if (sexp_placeholder_digit_p(c) && tmp >= 0) {
    whole = val;
    for ( ; sexp_placeholder_digit_p(c); c=sexp_read_char(ctx, in))
      whole = whole*10 + sexp_placeholder_digit_value(base);
    if ((c=='.' || is_precision_indicator(c)) && (base != 10))
      return sexp_read_error(ctx, "found non-base 10 float", SEXP_NULL, in);
    if (c=='.')
      for (c=sexp_read_char(ctx, in); sexp_placeholder_digit_p(c);
           c=sexp_read_char(ctx, in), scale*=0.1)
        whole += sexp_placeholder_digit_value(10)*scale;
    if (is_precision_indicator(c)) {
      sexp_push_char(ctx, c, in);
      return sexp_read_float_tail(ctx, in, whole, negativep);
    } else if ((c!=EOF) && !sexp_is_separator(c)) {
      return sexp_read_error(ctx, "invalid numeric syntax after placeholders",
                             sexp_make_character(c), in);
    }
    sexp_push_char(ctx, c, in);
    return sexp_make_flonum(ctx, (negativep ? -whole : whole));
  }
#endif

  if (c=='.' || is_precision_indicator(c)) {
    if (base != 10)
      return sexp_read_error(ctx, "found non-base 10 float", SEXP_NULL, in);
    if (c!='.') sexp_push_char(ctx, c, in);
    return sexp_read_float_tail(ctx, in, val, negativep);
  } else if (c=='/') {
    sexp_gc_preserve2(ctx, res, den);
    den = sexp_read_number(ctx, in, base);
    if (! (sexp_fixnump(den) || sexp_bignump(den) || sexp_complexp(den)))
      res = (sexp_exceptionp(den)
             ? den : sexp_read_error(ctx, "invalid rational syntax", den, in));
    else {
#if SEXP_USE_RATIOS
#if SEXP_USE_COMPLEX
    if (sexp_complexp(den)) {
      res = sexp_make_fixnum(negativep ? -val : val);
      if (sexp_complex_real(den) == SEXP_ZERO) {
        res = sexp_make_ratio(ctx, res, sexp_complex_imag(den));
        res = sexp_ratio_normalize(ctx, res, in);
        sexp_complex_imag(den) = res;
      } else {
        res = sexp_make_ratio(ctx, res, sexp_complex_real(den));
        res = sexp_ratio_normalize(ctx, res, in);
        sexp_complex_real(den) = res;
      }
      if (!sexp_exceptionp(res))
        res = den;
    } else
#endif
      do {
    res = sexp_make_ratio(ctx, sexp_make_fixnum(negativep ? -val : val), den);
    res = sexp_ratio_normalize(ctx, res, in);
      } while (0);
#else
    if (!sexp_exceptionp(res))
      res = sexp_make_flonum(ctx, (double)(negativep ? -val : val)
                             / (double)sexp_unbox_fixnum(den));
#endif
    }
    sexp_gc_release2(ctx);
    return res;
#if SEXP_USE_COMPLEX
  } else if (c=='i' || c=='I' || c=='+' || c=='-' || c=='@') {
    if (base != 10)
      return sexp_read_error(ctx, "found non-base 10 complex", SEXP_NULL, in);
#if SEXP_USE_MATH
    if (c=='@')
      return sexp_read_polar_tail(ctx, in, sexp_make_fixnum(negativep ? -val : val));
#endif
    sexp_push_char(ctx, c, in);
    return sexp_read_complex_tail(ctx, in, sexp_make_fixnum(negativep ? -val : val));
#endif
  } else {
    if ((c!=EOF) && ! sexp_is_separator(c))
      return sexp_read_error(ctx, "invalid numeric syntax",
                             sexp_make_character(c), in);
    else if (tmp < 0)
      return sexp_read_error(ctx, "digitless numeric literal", SEXP_NULL, in);
    sexp_push_char(ctx, c, in);
  }

  return sexp_make_fixnum(negativep ? -val : val);
}

#if SEXP_USE_UTF8_STRINGS
static int sexp_decode_utf8_char(const unsigned char* s) {
  int i = s[0], len = strlen((const char*)s);
  if ((i >= 0xC0) && (i <= 0xF7) && (s[1]>>6 == 2)) {
    if ((i < 0xE0) && (len == 2)) {
      return ((i&0x3F)<<6) + (s[1]&0x3F);
    } else if ((i < 0xF0) && (len == 3) && (s[2]>>6 == 2)) {
      return ((i&0x1F)<<12) + ((s[1]&0x3F)<<6) + (s[2]&0x3F);
    } else if ((len == 4) && (s[2]>>6 == 2) && (s[3]>>6 == 2)) {
      return ((i&0x0F)<<16) + ((s[1]&0x3F)<<6) + ((s[2]&0x3F)<<6) + (s[3]&0x3F);
    }
  }
  return -1;
}
#endif

sexp sexp_read_raw (sexp ctx, sexp in) {
  char *str;
  int c1, c2, line;
  sexp tmp2;
  sexp_gc_var2(res, tmp);
  sexp_check_block_port(ctx, in, 0);
  sexp_gc_preserve2(ctx, res, tmp);

 scan_loop:
  switch (c1 = sexp_read_char(ctx, in)) {
  case EOF:
    res = SEXP_EOF;
    break;
  case ';':
    while ((c1 = sexp_read_char(ctx, in)) != EOF)
      if (c1 == '\n')
        break;
    /* ... FALLTHROUGH ... */
  case '\n':
    sexp_port_line(in)++;
    goto scan_loop;
  case ' ':
  case '\t':
  case '\f':
  case '\r':
    goto scan_loop;
  case '\'':
    res = sexp_read(ctx, in);
    if (! sexp_exceptionp(res))
      res = sexp_list2(ctx, sexp_global(ctx, SEXP_G_QUOTE_SYMBOL), res);
    break;
  case '`':
    res = sexp_read(ctx, in);
    if (! sexp_exceptionp(res))
      res = sexp_list2(ctx, sexp_global(ctx, SEXP_G_QUASIQUOTE_SYMBOL), res);
    break;
  case ',':
    if ((c1 = sexp_read_char(ctx, in)) == '@') {
      res = sexp_read(ctx, in);
      if (! sexp_exceptionp(res))
        res = sexp_list2(ctx, sexp_global(ctx, SEXP_G_UNQUOTE_SPLICING_SYMBOL), res);
    } else {
      sexp_push_char(ctx, c1, in);
      res = sexp_read(ctx, in);
      if (! sexp_exceptionp(res))
        res = sexp_list2(ctx, sexp_global(ctx, SEXP_G_UNQUOTE_SYMBOL), res);
    }
    break;
  case '"':
    res = sexp_read_string(ctx, in, '"');
    break;
  case '(':
    line = (sexp_port_sourcep(in) ? sexp_port_line(in) : -1);
    res = SEXP_NULL;
    tmp = sexp_read_raw(ctx, in);
    while ((tmp != SEXP_EOF) && (tmp != SEXP_CLOSE) && (tmp != SEXP_RAWDOT)) {
      if (sexp_exceptionp(tmp)) {
        res = tmp;
        break;
      }
      res = sexp_cons(ctx, tmp, res);
      if (sexp_port_sourcep(in) && (line >= 0))
        sexp_pair_source(res)
          = sexp_cons(ctx, sexp_port_name(in), sexp_make_fixnum(line));
      tmp = sexp_read_raw(ctx, in);
    }
    if (! sexp_exceptionp(res)) {
      if (tmp == SEXP_RAWDOT) { /* dotted list */
        if (res == SEXP_NULL) {
          res = sexp_read_error(ctx, "dot before any elements in list",
                                SEXP_NULL, in);
        } else {
          tmp = sexp_read_raw(ctx, in);
          if (sexp_exceptionp(tmp)) {
            res = tmp;
          } else if (tmp == SEXP_CLOSE) {
            res = sexp_read_error(ctx, "no final element in list after dot",
                                  SEXP_NULL, in);
          } else if (sexp_read_raw(ctx, in) != SEXP_CLOSE) {
            res = sexp_read_error(ctx, "multiple tokens in dotted tail",
                                  SEXP_NULL, in);
          } else {
            tmp2 = res;
            res = sexp_nreverse(ctx, res);
            sexp_cdr(tmp2) = tmp;
          }
        }
      } else if (tmp == SEXP_CLOSE) {
        res = (sexp_pairp(res) ? sexp_nreverse(ctx, res) : res);
      } else {
        res = sexp_read_error(ctx, "missing trailing ')' started on line",
                              sexp_make_fixnum(line), in);
      }
    }
    if ((line >= 0) && sexp_pairp(res)) {
      sexp_pair_source(res)
        = sexp_cons(ctx, sexp_port_name(in), sexp_make_fixnum(line));
      for (tmp=sexp_cdr(res); sexp_pairp(tmp); tmp=sexp_cdr(tmp))
        sexp_pair_source(tmp) = sexp_pair_source(res);
    }
    if (sexp_port_sourcep(in))
      for (tmp=res; sexp_pairp(tmp); tmp=sexp_cdr(tmp))
        sexp_immutablep(tmp) = 1;
    break;
#if SEXP_USE_OBJECT_BRACE_LITERALS
  case '{':
    res = sexp_read_symbol(ctx, in, EOF, 0);
    if (!sexp_exceptionp(res)) {
      for (c1=' '; isspace(c1); c1=sexp_read_char(ctx, in))
        ;
      if (c1=='#') {
        tmp = sexp_read(ctx, in);
        if (sexp_symbolp(tmp) && tmp == sexp_intern(ctx, "t", 1))
          tmp = SEXP_TRUE;
        else if (!sexp_fixnump(tmp))
          tmp = sexp_read_error(ctx, "invalid type identifier", tmp, in);
      } else if (c1=='"') {
        tmp = sexp_read_string(ctx, in, '"');
      } else {
        tmp = sexp_read_error(ctx, "brace literal missing type identifier", sexp_make_character(c1), in);
      }
      if (!sexp_exceptionp(tmp)) tmp = sexp_lookup_type(ctx, res, tmp);
      if (tmp && sexp_typep(tmp) && sexp_opcodep(sexp_type_print(tmp))
          && sexp_opcode_func(sexp_type_print(tmp)) == (sexp_proc1)sexp_write_simple_object) {
        res = sexp_alloc_tagged(ctx, sexp_type_size_base(tmp), sexp_type_tag(tmp));
        for (c1=0; ; c1++) {
          tmp2 = sexp_read_raw(ctx, in);
          if (sexp_exceptionp(tmp2)) {
            res = tmp2;
            break;
          } else if (tmp2 == SEXP_CLOSE_BRACE) {
            break;
          } else if (c1 >= sexp_type_field_len_base(tmp)) {
            res = sexp_read_error(ctx, "too many slots in object literal", res, in);
            break;
          } else {
            sexp_slot_set(res, c1, tmp2);
          }
        }
      } else {
        res = sexp_exceptionp(tmp) ? tmp : sexp_read_error(ctx, "invalid type for brace literals", tmp, in);
      }
    }
    break;
#endif
  case '#':
    switch (c1=sexp_read_char(ctx, in)) {
    case 'b': case 'B':
      res = sexp_read_number(ctx, in, 2); break;
    case 'o': case 'O':
      res = sexp_read_number(ctx, in, 8); break;
    case 'd': case 'D':
      res = sexp_read_number(ctx, in, 10); break;
    case 'x': case 'X':
      res = sexp_read_number(ctx, in, 16); break;
    case 'e': case 'E':
      res = sexp_read(ctx, in);
#if SEXP_USE_INFINITIES
      if (sexp_flonump(res)
          && (isnan(sexp_flonum_value(res)) || isinf(sexp_flonum_value(res))))
        res = sexp_read_error(ctx, "can't convert non-finite flonum to exact", res, in);
      else
#endif
      if (sexp_flonump(res))
#if SEXP_USE_RATIOS
        res = sexp_double_to_ratio(ctx, sexp_flonum_value(res));
#elif SEXP_USE_BIGNUMS
        res = sexp_bignum_normalize(sexp_double_to_bignum(ctx, sexp_flonum_value(res)));
#else
        res = sexp_make_fixnum(sexp_flonum_value(res));
#endif
      break;
    case 'i': case 'I':
      res = sexp_read(ctx, in);
      if (sexp_exact_integerp(res))
        res = sexp_make_flonum(ctx, sexp_unbox_fixnum(res));
#if SEXP_USE_RATIOS
      else if (sexp_ratiop(res))
        res = sexp_make_flonum(ctx, sexp_ratio_to_double(res));
#endif
      break;
    case 'f': case 'F':
    case 't': case 'T':
      c2 = sexp_read_char(ctx, in);
      if (c2 == EOF || sexp_is_separator(c2)) {
        res = (sexp_tolower(c1) == 't' ? SEXP_TRUE : SEXP_FALSE);
        sexp_push_char(ctx, c2, in);
      } else {
        sexp_push_char(ctx, c2, in);
        res = sexp_read_symbol(ctx, in, c1, 0);
        if (!sexp_exceptionp(res)) {
          if (strcasecmp("true", sexp_string_data(res)) == 0)
            res = SEXP_TRUE;
          else if (strcasecmp("false", sexp_string_data(res)) == 0)
            res = SEXP_FALSE;
          else
            res = sexp_read_error(ctx, "invalid # syntax", res, in);
        }
      }
      break;
#if SEXP_USE_BYTEVECTOR_LITERALS
    case 'v': case 'V':
      c1 = sexp_read_char(ctx, in);
      if (!(c1=='u'||c1=='U')) {
        res = sexp_read_error(ctx, "invalid syntax #v%c", sexp_make_character(c1), in);
        break;
      }
      /* ... FALLTHROUGH ... */
    case 'u': case 'U':
      if ((c1 = sexp_read_char(ctx, in)) == '8') {
        tmp = sexp_read(ctx, in);
        if (!sexp_listp(ctx, tmp)) {
          res = sexp_exceptionp(tmp) ? tmp
            : sexp_read_error(ctx, "invalid syntax object after #u8", tmp, in);
        } else {
          res = sexp_make_bytes(ctx, sexp_length(ctx, tmp), SEXP_VOID);
          for (c1=0; sexp_pairp(tmp); tmp=sexp_cdr(tmp), c1++) {
            tmp2 = sexp_car(tmp);
            if (!(sexp_fixnump(tmp2) && sexp_unbox_fixnum(tmp2) >= 0
                  && sexp_unbox_fixnum(tmp2) < 0x100)) {
              res = sexp_read_error(ctx, "invalid bytevector value", tmp2, in);
              break;
            } else {
              sexp_bytes_set(res, sexp_make_fixnum(c1), tmp2);
            }
          }
        }
      } else {
        tmp = sexp_list2(ctx, sexp_make_character('u'), sexp_make_character(c1));
        res = sexp_read_error(ctx, "invalid syntax #%c%c", tmp, in);
      }
      break;
#endif
/*     case '0': case '1': case '2': case '3': case '4': */
/*     case '5': case '6': case '7': case '8': case '9': */
    case ';':
      tmp = sexp_read_raw(ctx, in);   /* discard */
      if (sexp_exceptionp(tmp))
        res = tmp;
      else
        goto scan_loop;
      break;
    case '|':
      for (c2 = 1; c2 > 0 && c1 != EOF; ) {
        c1 = sexp_read_char(ctx, in);
        if (c1 == '#') {
          while ((c1 = sexp_read_char(ctx, in)) == '#')
            ;
          if (c1 == '|') c2++;
        } else if (c1 == '|') {
          while ((c1 = sexp_read_char(ctx, in)) == '|')
            ;
          if (c1 == '#') c2--;
        }
      }
      if (c1 == EOF)
        res = sexp_read_error(ctx, "unterminated #| comment", SEXP_NULL, in);
      else
        goto scan_loop;
      break;
    case '!':
#if SEXP_USE_FOLD_CASE_SYMS
      res = sexp_read_symbol(ctx, in, '!', 0);
      if (sexp_stringp(res)
          && strcmp("!fold-case", sexp_string_data(res)) == 0) {
        sexp_port_fold_casep(in) = 1;
      } else if (sexp_stringp(res)
                 && strcmp("!no-fold-case", sexp_string_data(res)) == 0) {
        sexp_port_fold_casep(in) = 0;
      } else {
#endif
        while ((c1 = sexp_read_char(ctx, in)) != EOF)
          if (c1 == '\n')
            break;
        sexp_port_line(in)++;
#if SEXP_USE_FOLD_CASE_SYMS
      }
#endif
      goto scan_loop;
    case '\\':
      c1 = sexp_read_char(ctx, in);
      c2 = sexp_read_char(ctx, in);
      sexp_push_char(ctx, c2, in);
      if ((c1 == 'x' || c1 == 'X') && (sexp_isxdigit(c2))) {
        res = sexp_read_number(ctx, in, 16);
        if (sexp_fixnump(res) && sexp_unbox_fixnum(res) >= 0 && sexp_unbox_fixnum(res) <= 0x10FFFF)
          res = sexp_make_character(sexp_unbox_fixnum(res));
        else if (!sexp_exceptionp(res))
          res = sexp_read_error(ctx, "bad character #\\x literal", res, in);
      } else {
        res = sexp_read_symbol(ctx, in, c1, 0);
        if (sexp_stringp(res)) {
          str = sexp_string_data(res);
          if (sexp_string_length(res) == 0)
            res =
              sexp_read_error(ctx, "unexpected end of character literal",
                              SEXP_NULL, in);
          if (sexp_string_length(res) == 1) {
            res = sexp_make_character(c1);
          } else {
            res = 0;
            for (c2=0; c2 < sexp_num_char_names; c2++) {
              if (strcasecmp(str, sexp_char_names[c2].name) == 0) {
                res = sexp_make_character(sexp_char_names[c2].ch);
                break;
              }
            }
            if (!res) {
#if SEXP_USE_UTF8_STRINGS
              if ((c1=sexp_decode_utf8_char((unsigned char*)str)) > 0) {
                res = sexp_make_character(c1);
                break;
              }
#endif
              tmp = sexp_c_string(ctx, str, -1);
              res = sexp_read_error(ctx, "unknown character name", tmp, in);
            }
          }
        }
      }
      break;
    case '(':
      sexp_push_char(ctx, c1, in);
      res = sexp_read(ctx, in);
      if (sexp_not(sexp_listp(ctx, res))) {
        if (! sexp_exceptionp(res)) {
          res = sexp_read_error(ctx, "dotted list not allowed in vector syntax",
                                SEXP_NULL,
                                in);
        }
      } else {
        res = sexp_list_to_vector(ctx, res);
      }
      break;
    default:
      res = sexp_read_error(ctx, "invalid char following '#'",
                            c1 == EOF ? SEXP_EOF : sexp_make_character(c1), in);
    }
    break;
  case '.':
    c1 = sexp_read_char(ctx, in);
    sexp_push_char(ctx, c1, in);
    if (c1 == EOF || sexp_is_separator(c1)) {
      res = SEXP_RAWDOT;
    } else if (sexp_isdigit(c1)) {
      res = sexp_read_float_tail(ctx, in, 0, 0);
    } else {
      res = sexp_read_symbol(ctx, in, '.', 1);
    }
    break;
  case ')':
    res = SEXP_CLOSE;
    break;
#if SEXP_USE_OBJECT_BRACE_LITERALS
  case '}':
    res = SEXP_CLOSE_BRACE;
    break;
#endif
  case '|':
    res = sexp_read_string(ctx, in, '|');
    if (sexp_stringp(res))
      res = sexp_intern(ctx, sexp_string_data(res), sexp_string_length(res));
    break;
  case '+':
  case '-':
    c2 = sexp_read_char(ctx, in);
    if (c2 == '.' || sexp_isdigit(c2)) {
      sexp_push_char(ctx, c2, in);
      res = sexp_read_number(ctx, in, 10);
      if ((c1 == '-') && ! sexp_exceptionp(res)) {
#if SEXP_USE_FLONUMS
        if (sexp_flonump(res))
#if SEXP_USE_IMMEDIATE_FLONUMS
          res = sexp_make_flonum(ctx, -1 * sexp_flonum_value(res));
#else
          sexp_flonum_value(res) = -1 * sexp_flonum_value(res);
#endif
        else
#endif
#if SEXP_USE_BIGNUMS
          if (sexp_bignump(res)) {
            if ((sexp_bignum_hi(res) == 1)
                && (sexp_bignum_data(res)[0] == (SEXP_MAX_FIXNUM+1)))
              res = sexp_make_fixnum(-sexp_bignum_data(res)[0]);
            else
              sexp_bignum_sign(res) = -sexp_bignum_sign(res);
          } else
#endif
#if SEXP_USE_RATIOS
          if (sexp_ratiop(res)) {
            sexp_negate(sexp_ratio_numerator(res));
          } else
#endif
#if SEXP_USE_COMPLEX
          if (sexp_complexp(res)) {
            if (sexp_complex_real(res) == SEXP_ZERO) {
              sexp_negate(sexp_complex_imag(res));
            } else {
              sexp_negate(sexp_complex_real(res));
            }
          } else
#endif
            res = sexp_fx_mul(res, SEXP_NEG_ONE);
      }
    } else {
      sexp_push_char(ctx, c2, in);
      res = sexp_read_symbol(ctx, in, c1, !SEXP_USE_INFINITIES);
#if SEXP_USE_INFINITIES
      if (sexp_stringp(res)) {
        str = sexp_string_data(res);
        if (strcasecmp(str, "+inf.0") == 0)
          res = sexp_make_flonum(ctx, sexp_pos_infinity);
        else if (strcasecmp(str, "-inf.0") == 0)
          res = sexp_make_flonum(ctx, sexp_neg_infinity);
        else if (strcasecmp(str+1, "nan.0") == 0)
          res = sexp_make_flonum(ctx, sexp_nan);
#if SEXP_USE_COMPLEX
        else if (strcasestr(str+1, "inf.0") == str+1) {
          tmp = sexp_make_flonum(ctx, c1 == '+' ? sexp_pos_infinity : sexp_neg_infinity);
          if (str[6] == 0) {
            res = tmp;
          } else if ((str[6] == 'i' || str[6] == 'I') && str[7] == 0) {
            res = sexp_make_complex(ctx, SEXP_ZERO, tmp);
          } else if (str[6] == '+' || str[6] == '-') {
            res = sexp_substring(ctx, res, SEXP_SIX, SEXP_FALSE);
            res = sexp_string_to_number(ctx, res, SEXP_TEN);
            if (sexp_complexp(res) && (sexp_complex_real(res) == SEXP_ZERO))
              sexp_complex_real(res) = tmp;
            else
              res = sexp_read_error(ctx, "invalid complex infinity", res, in);
          } else {
            res = sexp_read_error(ctx, "invalid infinity", res, in);
          }
        }
#endif
        else
          res = sexp_intern(ctx, str, sexp_string_length(res));
      }
#endif
#if SEXP_USE_COMPLEX
      if (res == sexp_intern(ctx, "+i", -1))
        res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_ONE);
      else if (res == sexp_intern(ctx, "-i", -1))
        res = sexp_make_complex(ctx, SEXP_ZERO, SEXP_NEG_ONE);
#endif
    }
    break;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    sexp_push_char(ctx, c1, in);
    res = sexp_read_number(ctx, in, 10);
    break;
  default:
    res = sexp_read_symbol(ctx, in, c1, 1);
    break;
  }

  if (sexp_port_sourcep(in) && sexp_pointerp(res))
    sexp_immutablep(res) = 1;
  sexp_maybe_unblock_port(ctx, in);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_read_op (sexp ctx, sexp self, sexp_sint_t n, sexp in) {
  sexp res;
  sexp_assert_type(ctx, sexp_iportp, SEXP_IPORT, in);
  res = sexp_read_raw(ctx, in);
  if (res == SEXP_CLOSE)
    res = sexp_read_error(ctx, "too many ')'s", SEXP_NULL, in);
#if SEXP_USE_OBJECT_BRACE_LITERALS
  else if (res == SEXP_CLOSE_BRACE)
    res = sexp_read_error(ctx, "too many '}'s", SEXP_NULL, in);
#endif
  else if (res == SEXP_RAWDOT)
    res = sexp_read_error(ctx, "unexpected '.'", SEXP_NULL, in);
  return res;
}

sexp sexp_read_from_string (sexp ctx, const char *str, sexp_sint_t len) {
  sexp res;
  sexp_gc_var2(s, in);
  sexp_gc_preserve2(ctx, s, in);
  s = sexp_c_string(ctx, str, len);
  in = sexp_make_input_string_port(ctx, s);
  res = sexp_read(ctx, in);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_string_to_number_op (sexp ctx, sexp self, sexp_sint_t n, sexp str, sexp b) {
  int base;
  sexp_gc_var1(in);
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, b);
  if (((base=sexp_unbox_fixnum(b)) < 2) || (base > 36))
    return sexp_user_exception(ctx, self, "invalid numeric base", b);
  if (sexp_string_data(str)[0]=='\0'
      || (sexp_string_data(str)[1]=='\0' && !sexp_isxdigit((unsigned char)(sexp_string_data(str)[0]))))
    return SEXP_FALSE;
  sexp_gc_preserve1(ctx, in);
  in = sexp_make_input_string_port(ctx, str);
  if (sexp_string_data(str)[0] == '+') {
    if (sexp_isdigit((unsigned char)(sexp_string_data(str)[1]))
        || sexp_string_data(str)[1] == '.' || sexp_string_data(str)[1] == '#')
      sexp_read_char(ctx, in);
  }
  in = ((sexp_string_data(str)[0] == '#') || base == 10 ?
        sexp_read(ctx, in) : sexp_read_number(ctx, in, base));
  sexp_gc_release1(ctx);
  return sexp_numberp(in) ? in : SEXP_FALSE;
}

sexp sexp_write_to_string (sexp ctx, sexp obj) {
  sexp str;
  sexp_gc_var1(out);
  sexp_gc_preserve1(ctx, out);
  out = sexp_make_output_string_port(ctx);
  str = sexp_write(ctx, obj, out);
  if (! sexp_exceptionp(str))
    str = sexp_get_output_string(ctx, out);
  sexp_gc_release1(ctx);
  return str;
}

sexp sexp_symbol_to_string_op (sexp ctx, sexp self, sexp_sint_t n, sexp sym) {
#if SEXP_USE_HUFF_SYMS
  if (sexp_isymbolp(sym)) return sexp_write_to_string(ctx, sym);
#endif
  sexp_assert_type(ctx, sexp_lsymbolp, SEXP_SYMBOL, sym);
  return sexp_c_string(ctx, sexp_lsymbol_data(sym), sexp_lsymbol_length(sym));
}

void sexp_init (void) {
#if SEXP_USE_GLOBAL_SYMBOLS
  int i;
#endif
  if (! sexp_initialized_p) {
    sexp_initialized_p = 1;
#if SEXP_USE_BOEHM
    GC_init();
#if SEXP_USE_GLOBAL_SYMBOLS
    GC_add_roots((char*)&sexp_symbol_table,
                 ((char*)&sexp_symbol_table)+sizeof(sexp_symbol_table)+1);
#endif
#elif ! SEXP_USE_MALLOC
    sexp_gc_init();
#endif
#if SEXP_USE_GLOBAL_SYMBOLS
    for (i=0; i<SEXP_SYMBOL_TABLE_SIZE; i++)
      sexp_symbol_table[i] = SEXP_NULL;
#endif
  }
}

