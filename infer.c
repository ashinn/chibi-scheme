/*  infer.c -- basic type and escape inference passes    */
/*  Copyright (c) 2026 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "chibi/eval.h"

#if SEXP_USE_INFERENCE

/************************* Leaf Procedure Analysis ********************/

struct sexp_local_binding {
  sexp cell;
  sexp name;
  sexp loc;
  sexp lam;
};

static int is_leaf_expr (sexp ctx, sexp lam, sexp x, int tailp) {
  if (!sexp_pointerp(x)) {
    return 1;
  }
  switch (sexp_pointer_tag(x)) {
  case SEXP_LIT:
    return 1;
  case SEXP_REF: {
    sexp loc = sexp_ref_loc(x);
    if (loc != lam) return 0;
    return 1;
  }
  case SEXP_SET:
    return 0;
  case SEXP_CND: {
    sexp test = sexp_cnd_test(x);
    while (sexp_pairp(test) && sexp_refp(sexp_car(test)) &&
           sexp_ref_name(sexp_car(test)) == sexp_intern(ctx, "not", 3) &&
           !sexp_lambdap(sexp_ref_loc(sexp_car(test))) &&
           sexp_pairp(sexp_cdr(test)) && sexp_nullp(sexp_cddr(test))) {
      test = sexp_cadr(test);
    }
    return is_leaf_expr(ctx, lam, test, 0) &&
           is_leaf_expr(ctx, lam, sexp_cnd_pass(x), tailp) &&
           is_leaf_expr(ctx, lam, sexp_cnd_fail(x), tailp);
  }
  case SEXP_SEQ: {
    sexp ls = sexp_seq_ls(x);
    for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
      int cur_tailp = sexp_nullp(sexp_cdr(ls)) ? tailp : 0;
      if (!is_leaf_expr(ctx, lam, sexp_car(ls), cur_tailp))
        return 0;
    }
    return 1;
  }
  case SEXP_PAIR: {
    sexp op = sexp_car(x);
    if (sexp_opcodep(op)) {
      sexp_sint_t code = sexp_opcode_code(op);
      sexp_sint_t oclass = sexp_opcode_class(op);
      if (oclass == SEXP_OPC_FOREIGN || oclass == SEXP_OPC_PARAMETER)
        return 0;
      switch (code) {
      case SEXP_OP_ADD:
      case SEXP_OP_SUB:
      case SEXP_OP_MUL:
      case SEXP_OP_QUOTIENT:
      case SEXP_OP_REMAINDER:
      case SEXP_OP_LT:
      case SEXP_OP_LE:
      case SEXP_OP_EQN:
      case SEXP_OP_EQ:
      case SEXP_OP_NULLP:
      case SEXP_OP_FIXNUMP:
      case SEXP_OP_CHARP:
      case SEXP_OP_EOFP:
      case SEXP_OP_SYMBOLP:
      case SEXP_OP_CAR:
      case SEXP_OP_CDR:
      case SEXP_OP_SET_CAR:
      case SEXP_OP_SET_CDR:
      case SEXP_OP_VECTOR_LENGTH:
      case SEXP_OP_VECTOR_REF:
      case SEXP_OP_VECTOR_SET:
      case SEXP_OP_STRING_LENGTH:
      case SEXP_OP_STRING_REF:
      case SEXP_OP_STRING_CURSOR_NEXT:
      case SEXP_OP_STRING_CURSOR_PREV:
      case SEXP_OP_STRING_CURSOR_END:
      case SEXP_OP_BYTES_LENGTH:
      case SEXP_OP_BYTES_REF:
      case SEXP_OP_BYTES_SET:
      case SEXP_OP_SLOT_REF:
      case SEXP_OP_SLOT_SET:
      case SEXP_OP_TYPEP:
      case SEXP_OP_ISA:
      case SEXP_OP_CHAR2INT:
      case SEXP_OP_INT2CHAR:
        break;
      default:
        return 0;
      }
      sexp ls = sexp_cdr(x);
      for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
        if (!is_leaf_expr(ctx, lam, sexp_car(ls), 0))
          return 0;
      }
      return 1;
    }
    return 0;
  }
  default:
    return 0;
  }
}

static int is_leaf_procedure (sexp ctx, sexp lam) {
  /* if (!name || !sexp_symbolp(name)) return 0; */
  if (!lam || !sexp_lambdap(lam)) return 0;
  if (!sexp_truep(sexp_listp(ctx, sexp_lambda_params(lam)))) return 0;
  if (!sexp_nullp(sexp_lambda_fv(lam))) return 0;
  if (sexp_pairp(sexp_lambda_sv(lam))) return 0;
  if (sexp_pairp(sexp_lambda_locals(lam))) return 0;
  return is_leaf_expr(ctx, lam, sexp_lambda_body(lam), 1);
}

static sexp find_lambda_for_ref (sexp ref, const struct sexp_local_binding *bindings, int n) {
  if (!sexp_refp(ref)) return SEXP_FALSE;
  sexp cell = sexp_ref_cell(ref);
  sexp name = sexp_ref_name(ref);
  sexp loc = sexp_ref_loc(ref);
  for (int i = 0; i < n; i++) {
    if (bindings[i].cell == cell || (bindings[i].name == name && bindings[i].loc == loc))
      return bindings[i].lam;
  }
  return SEXP_FALSE;
}

static int collect_local_bindings (sexp body, sexp enclosing_lam,
                                   struct sexp_local_binding *bindings, int max_bindings) {
  int count = 0;
  if (!body || !sexp_pointerp(body)) return 0;
  if (sexp_seqp(body)) {
    for (sexp ls = sexp_seq_ls(body); sexp_pairp(ls); ls = sexp_cdr(ls)) {
      sexp item = sexp_car(ls);
      if (sexp_setp(item)) {
        sexp var = sexp_set_var(item);
        sexp val = sexp_set_value(item);
        if (sexp_refp(var) && sexp_lambdap(val) && enclosing_lam && sexp_ref_loc(var) == enclosing_lam) {
          if (count < max_bindings) {
            bindings[count].cell = sexp_ref_cell(var);
            bindings[count].name = sexp_ref_name(var);
            bindings[count].loc = sexp_ref_loc(var);
            bindings[count].lam = val;
            count++;
          }
        }
      }
    }
  } else if (sexp_setp(body)) {
    sexp var = sexp_set_var(body);
    sexp val = sexp_set_value(body);
    if (sexp_refp(var) && sexp_lambdap(val) && enclosing_lam && sexp_ref_loc(var) == enclosing_lam) {
      if (count < max_bindings) {
        bindings[count].cell = sexp_ref_cell(var);
        bindings[count].name = sexp_ref_name(var);
        bindings[count].loc = sexp_ref_loc(var);
        bindings[count].lam = val;
        count++;
      }
    }
  }
  return count;
}

static inline void sexp_lambda_set_escapes (sexp x) {
  if (x && sexp_lambdap(x)) {
    sexp_sint_t flags = sexp_fixnump(sexp_lambda_flags(x)) ? sexp_unbox_fixnum(sexp_lambda_flags(x)) : 0;
    sexp_lambda_flags(x) = sexp_make_fixnum(flags | SEXP_LAMBDA_ESCAPES);
  }
}

static void analyze_escapes_scope (sexp ctx, sexp body, sexp enclosing_lam,
                                   const struct sexp_local_binding *parent_bindings, int parent_n);

static void analyze_escapes_expr (sexp ctx, sexp x, sexp enclosing_lam,
                                 const struct sexp_local_binding *bindings, int n,
                                 int is_tail, int is_operator, int is_arg) {
  if (!x || !sexp_pointerp(x) || sexp_litp(x)) return;
  int tag = sexp_pointer_tag(x);
  switch (tag) {
  case SEXP_LAMBDA: {
    if (!enclosing_lam || !is_operator) {
      sexp_lambda_set_escapes(x);
    }
    if (is_leaf_procedure(ctx, x)) {
      sexp_lambda_flags(x) = sexp_make_fixnum(sexp_unbox_fixnum(sexp_lambda_flags(x)) | SEXP_LAMBDA_LEAF);
    }
    analyze_escapes_scope(ctx, sexp_lambda_body(x), x, bindings, n);
    break;
  }
  case SEXP_REF: {
    sexp target_lam = find_lambda_for_ref(x, bindings, n);
    if (target_lam && sexp_lambdap(target_lam)) {
      if (!is_operator) {
        sexp_lambda_set_escapes(target_lam);
      }
    }
    break;
  }
  case SEXP_SET: {
    sexp var = sexp_set_var(x);
    sexp val = sexp_set_value(x);
    /* a set! other than the one defining the binding means calls */
    /* through it may reach either lambda, so neither is known */
    sexp old_lam = find_lambda_for_ref(var, bindings, n);
    if (old_lam && sexp_lambdap(old_lam) && old_lam != val) {
      sexp_lambda_set_escapes(old_lam);
      sexp_lambda_set_escapes(val);
    }
    if (sexp_refp(val)) {
      sexp val_lam = find_lambda_for_ref(val, bindings, n);
      if (val_lam && sexp_lambdap(val_lam)) {
        sexp_lambda_set_escapes(val_lam);
      }
    }
    if (sexp_lambdap(val)) {
      if (!enclosing_lam || !sexp_refp(var) || sexp_ref_loc(var) != enclosing_lam) {
        sexp_lambda_set_escapes(val);
      }
      if (is_leaf_procedure(ctx, val)) {
        sexp_lambda_flags(val) = sexp_make_fixnum(sexp_unbox_fixnum(sexp_lambda_flags(val)) | SEXP_LAMBDA_LEAF);
      }
      analyze_escapes_scope(ctx, sexp_lambda_body(val), val, bindings, n);
    } else {
      analyze_escapes_expr(ctx, val, enclosing_lam, bindings, n, 0, 0, 0);
    }
    break;
  }
  case SEXP_PAIR: {
    sexp op = sexp_car(x);
    sexp args = sexp_cdr(x);
    analyze_escapes_expr(ctx, op, enclosing_lam, bindings, n, 0, 1, 0);
    for (sexp ls = args; sexp_pairp(ls); ls = sexp_cdr(ls)) {
      analyze_escapes_expr(ctx, sexp_car(ls), enclosing_lam, bindings, n, 0, 0, 1);
    }
    break;
  }
  case SEXP_CND: {
    analyze_escapes_expr(ctx, sexp_cnd_test(x), enclosing_lam, bindings, n, 0, 0, 0);
    analyze_escapes_expr(ctx, sexp_cnd_pass(x), enclosing_lam, bindings, n, is_tail, 0, 0);
    analyze_escapes_expr(ctx, sexp_cnd_fail(x), enclosing_lam, bindings, n, is_tail, 0, 0);
    break;
  }
  case SEXP_SEQ: {
    sexp ls = sexp_seq_ls(x);
    for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
      int last = sexp_nullp(sexp_cdr(ls));
      analyze_escapes_expr(ctx, sexp_car(ls), enclosing_lam, bindings, n, last ? is_tail : 0, 0, 0);
    }
    break;
  }
  case SEXP_SYNCLO: {
    analyze_escapes_expr(ctx, sexp_synclo_expr(x), enclosing_lam, bindings, n, is_tail, is_operator, is_arg);
    break;
  }
  default:
    break;
  }
}

static void analyze_escapes_scope (sexp ctx, sexp body, sexp enclosing_lam,
                                   const struct sexp_local_binding *parent_bindings, int parent_n) {
  struct sexp_local_binding stack_bindings[32];
  struct sexp_local_binding *bindings = stack_bindings;
  int max_bindings = 32;
  int total_capacity = parent_n + 32;
  if (total_capacity > 32) {
    bindings = (struct sexp_local_binding *)malloc(total_capacity * sizeof(struct sexp_local_binding));
    if (!bindings) return;
    max_bindings = total_capacity;
  }
  for (int i = 0; i < parent_n; i++) {
    bindings[i] = parent_bindings[i];
  }
  int local_count = collect_local_bindings(body, enclosing_lam, bindings + parent_n, max_bindings - parent_n);
  int total_count = parent_n + local_count;

  analyze_escapes_expr(ctx, body, enclosing_lam, bindings, total_count, 1, 0, 0);

  if (bindings != stack_bindings) free(bindings);
}

static void analyze_escapes (sexp ctx, sexp ast) {
  analyze_escapes_scope(ctx, ast, NULL, NULL, 0);
}

/************************* Parameter Type Inference *******************/

int sexp_fixnum_literalp (sexp x, sexp_sint_t *val) {
  if (!sexp_pointerp(x)) {
    if (sexp_fixnump(x)) {
      if (val) *val = sexp_unbox_fixnum(x);
      return 1;
    }
    return 0;
  }
  if (sexp_litp(x)) {
    sexp v = sexp_lit_value(x);
    if (sexp_fixnump(v)) {
      if (val) *val = sexp_unbox_fixnum(v);
      return 1;
    }
  }
  return 0;
}

/* Fixnum magnitudes: a conservative bound b with |x| < 2^b.  x is */
/* proven a fixnum when b <= FIXNUM_MAG_BITS. */
#define FIXNUM_MAG_BITS ((int)(sizeof(sexp_sint_t)*8 - SEXP_FIXNUM_BITS - 1))
/* object lengths are bounded by the address space */
#define LENGTH_BITS 48
/* A loop counter starting below 2^SMALL_START_BITS and stepped by */
/* +-1 stays below 2^(SMALL_START_BITS+1), assuming fewer than */
/* 2^SMALL_START_BITS iterations (centuries at any realistic speed). */
#define SMALL_START_BITS (FIXNUM_MAG_BITS - 2)

/* Param type entries are core type tags, with the magnitude for */
/* fixnums, or the element type above the tag for uniform vectors. */
#define TYPE_ENTRY(type, bits) ((type) == SEXP_FIXNUM ? (type) + ((bits) << 16) : (type))
#define UVECTOR_ENTRY(etype) (SEXP_UNIFORM_VECTOR | ((etype) << 8))
#define ENTRY_TYPE(e) ((e) & 0xFF)
#define ENTRY_SUBTYPE(e) (((e) >> 8) & 0x3F)
#define ENTRY_KIND(e) ((e) & 0x3FFF)  /* type and subtype */
/* a flonum native code keeps unboxed */
#define ENTRY_UNBOXED 0x4000
#define ENTRY_BITS(e) ((e) >> 16)
/* a container type checked on entry, the order-th check made there */
#define ENTRY_CHECKED 0x8000
#define CHECKED_ENTRY(type, order) ((type) | ENTRY_CHECKED | ((order) << 16))

static int bits_of (sexp_uint_t v) {
  int b = 0;
  for (; v; v >>= 1) b++;
  return b;
}

static int max_int (int a, int b) { return a > b ? a : b; }

/* the type entry inferred for the param x refers to, or -1 */
static int ref_param_entry (sexp x) {
  sexp lam, params, types;
  if (!x || !sexp_pointerp(x) || !sexp_refp(x)) return -1;
  lam = sexp_ref_loc(x);
  if (!sexp_lambdap(lam)) return -1;
  params = sexp_lambda_params(lam);
  types = sexp_lambda_param_types(lam);
  for (; sexp_pairp(params) && sexp_pairp(types);
       params = sexp_cdr(params), types = sexp_cdr(types))
    if (sexp_car(params) == sexp_ref_name(x))
    {
      int e;
      if (!sexp_fixnump(sexp_car(types))) return -1;
      e = (int)sexp_unbox_fixnum(sexp_car(types));
      return (e & ENTRY_CHECKED) ? ENTRY_KIND(e) : (e & ~ENTRY_UNBOXED);
    }
  return -1;
}

static int is_ref_to_var (sexp x, sexp lam, sexp sym) {
  return (x && sexp_pointerp(x) && sexp_pointer_tag(x) == SEXP_REF &&
          sexp_ref_loc(x) == lam && sexp_ref_name(x) == sym);
}

/* magnitude bound of the exact integer x, or -1 if unknown */
static int fixnum_bits (sexp ctx, sexp x) {
  sexp_sint_t c;
  sexp ls;
  int b, n;
  if (!x) return -1;
  if (sexp_fixnum_literalp(x, &c))
    return bits_of(c < 0 ? -(sexp_uint_t)c : (sexp_uint_t)c);
  if (!sexp_pointerp(x)) return -1;
  switch (sexp_pointer_tag(x)) {
  case SEXP_REF:
    b = ref_param_entry(x);
    return (b >= 0 && ENTRY_TYPE(b) == SEXP_FIXNUM) ? ENTRY_BITS(b) : -1;
  case SEXP_CND: {
    int p = fixnum_bits(ctx, sexp_cnd_pass(x)), f = fixnum_bits(ctx, sexp_cnd_fail(x));
    return (p < 0 || f < 0) ? -1 : max_int(p, f);
  }
  case SEXP_SEQ:
    for (ls = sexp_seq_ls(x); sexp_pairp(ls) && sexp_pairp(sexp_cdr(ls)); ls = sexp_cdr(ls))
      ;
    return sexp_pairp(ls) ? fixnum_bits(ctx, sexp_car(ls)) : -1;
  case SEXP_SYNCLO:
    return fixnum_bits(ctx, sexp_synclo_expr(x));
  case SEXP_PAIR:
    break;
  default:
    return -1;
  }
  if (!sexp_opcodep(sexp_car(x))) return -1;
  ls = sexp_cdr(x);
  switch (sexp_opcode_code(sexp_car(x))) {
  case SEXP_OP_VECTOR_LENGTH:
  case SEXP_OP_BYTES_LENGTH:
  case SEXP_OP_STRING_LENGTH:
    return LENGTH_BITS;
  case SEXP_OP_BYTES_REF:
    return 8;
#if SEXP_USE_UNIFORM_VECTOR_LITERALS
  case SEXP_OP_UVECTOR_LENGTH:
    return LENGTH_BITS;
  case SEXP_OP_UVECTOR_REF:
    if (sexp_fixnump(sexp_opcode_data(sexp_car(x))))
      switch (sexp_unbox_fixnum(sexp_opcode_data(sexp_car(x)))) {
      case SEXP_U1: return 1;
      case SEXP_S8: return 8;
      case SEXP_S16: case SEXP_U16: return 16;
      case SEXP_S32: case SEXP_U32: return 32;
      }
    return -1;
#endif
  case SEXP_OP_CHAR2INT:
    return 32;  /* sexp_unbox_character returns an int */
  case SEXP_OP_ADD:
  case SEXP_OP_SUB:
    /* |a1 +- ... +- an| < n * 2^max */
    for (b = 0, n = 0; sexp_pairp(ls); ls = sexp_cdr(ls), n++) {
      int bi = fixnum_bits(ctx, sexp_car(ls));
      if (bi < 0) return -1;
      b = max_int(b, bi);
    }
    return n == 0 ? 0 : b + bits_of(n - 1);
  case SEXP_OP_MUL:
    for (b = 0, n = 0; sexp_pairp(ls); ls = sexp_cdr(ls), n++) {
      int bi = fixnum_bits(ctx, sexp_car(ls));
      if (bi < 0) return -1;
      b += bi;
    }
    return n == 0 ? 1 : b;
  case SEXP_OP_QUOTIENT:
  case SEXP_OP_REMAINDER:
    if (!sexp_pairp(ls) || !sexp_pairp(sexp_cdr(ls)) || !sexp_nullp(sexp_cddr(ls)))
      return -1;
    b = fixnum_bits(ctx, sexp_car(ls));
    if (b < 0 || !sexp_fixnum_literalp(sexp_cadr(ls), &c) || c == 0)
      return -1;
    if (sexp_opcode_code(sexp_car(x)) == SEXP_OP_QUOTIENT)
      return (c == 1 || c == -1) ? b : b - 1;
    return bits_of((c < 0 ? -(sexp_uint_t)c : (sexp_uint_t)c) - 1);
  default:
    return -1;
  }
}

int sexp_proven_fixnump (sexp ctx, sexp lam, sexp x) {
  int b;
  if (sexp_fixnum_literalp(x, NULL)) return 1;
  b = fixnum_bits(ctx, x);
  return b >= 0 && b <= FIXNUM_MAG_BITS;
}

/* the type entry of x: a type tag known for its value, or SEXP_OBJECT */
static int infer_expr_entry (sexp ctx, sexp x) {
  sexp ls;
  int b = fixnum_bits(ctx, x);
  if (b >= 0 && b <= FIXNUM_MAG_BITS) return TYPE_ENTRY(SEXP_FIXNUM, b);
  if (sexp_fixnum_literalp(x, NULL)) return TYPE_ENTRY(SEXP_FIXNUM, FIXNUM_MAG_BITS);
  if (!x || !sexp_pointerp(x)) {
    if (sexp_booleanp(x)) return SEXP_BOOLEAN;
    if (sexp_charp(x)) return SEXP_CHAR;
    return SEXP_OBJECT;
  }
  switch (sexp_pointer_tag(x)) {
  case SEXP_LIT: {
    sexp v = sexp_lit_value(x);
    if (sexp_booleanp(v)) return SEXP_BOOLEAN;
    if (sexp_charp(v)) return SEXP_CHAR;
    if (!sexp_pointerp(v)) return SEXP_OBJECT;
    switch (sexp_pointer_tag(v)) {
    case SEXP_FLONUM: case SEXP_STRING: case SEXP_VECTOR:
    case SEXP_BYTES: case SEXP_PAIR:
      return sexp_pointer_tag(v);
    default:
      return SEXP_OBJECT;
    }
  }
#if SEXP_USE_FLONUMS
  case SEXP_FLONUM:
    return SEXP_FLONUM;
#endif
  case SEXP_PAIR: {
    sexp op = sexp_car(x);
    if (!sexp_opcodep(op)) return SEXP_OBJECT;
    switch (sexp_opcode_code(op)) {
#if SEXP_USE_FLONUMS
    case SEXP_OP_ADD: case SEXP_OP_SUB: case SEXP_OP_MUL: case SEXP_OP_DIV:
      /* flonums only give flonums (with exact operands, exact zero */
      /* is special and complex numbers are contagious) */
      for (ls = sexp_cdr(x), b = 0; sexp_pairp(ls); ls = sexp_cdr(ls), b++)
        if (ENTRY_TYPE(infer_expr_entry(ctx, sexp_car(ls))) != SEXP_FLONUM)
          return SEXP_OBJECT;
      return b > 0 ? SEXP_FLONUM : SEXP_OBJECT;
#endif
#if SEXP_USE_UNIFORM_VECTOR_LITERALS
    case SEXP_OP_UVECTOR_REF:
      if (sexp_opcode_data(op) == sexp_make_fixnum(SEXP_F64)
          || sexp_opcode_data(op) == sexp_make_fixnum(SEXP_F32))
        return SEXP_FLONUM;
      return SEXP_OBJECT;
#endif
    case SEXP_OP_STRING_REF:
    case SEXP_OP_INT2CHAR:
    case SEXP_OP_CHAR_UPCASE:
    case SEXP_OP_CHAR_DOWNCASE:
      return SEXP_CHAR;
    case SEXP_OP_MAKE_VECTOR:
      return SEXP_VECTOR;
    default:
      break;
    }
    switch (sexp_opcode_class(op)) {
    case SEXP_OPC_TYPE_PREDICATE:
    case SEXP_OPC_PREDICATE:
    case SEXP_OPC_ARITHMETIC_CMP:
      return SEXP_BOOLEAN;
    default:
      return SEXP_OBJECT;
    }
  }
  case SEXP_CND: {
    int p = infer_expr_entry(ctx, sexp_cnd_pass(x));
    return p == infer_expr_entry(ctx, sexp_cnd_fail(x)) ? p : SEXP_OBJECT;
  }
  case SEXP_SEQ:
    for (ls = sexp_seq_ls(x); sexp_pairp(ls) && sexp_pairp(sexp_cdr(ls)); ls = sexp_cdr(ls))
      ;
    return sexp_pairp(ls) ? infer_expr_entry(ctx, sexp_car(ls)) : SEXP_OBJECT;
  case SEXP_SYNCLO:
    return infer_expr_entry(ctx, sexp_synclo_expr(x));
  case SEXP_REF:
    b = ref_param_entry(x);
    return b >= 0 ? b : SEXP_OBJECT;
  default:
    return SEXP_OBJECT;
  }
}

int sexp_inferred_type (sexp ctx, sexp x) {
  return ENTRY_TYPE(infer_expr_entry(ctx, x));
}

int sexp_inferred_uvector_type (sexp ctx, sexp x) {
  int e = infer_expr_entry(ctx, x);
  return ENTRY_TYPE(e) == SEXP_UNIFORM_VECTOR ? ENTRY_SUBTYPE(e) : -1;
}

/* combine the entries of two possible values, -1 meaning none yet */
static int merge_entry (int a, int b) {
  if (a == -1) return b;
  if (ENTRY_TYPE(a) == SEXP_FIXNUM && ENTRY_TYPE(b) == SEXP_FIXNUM)
    return TYPE_ENTRY(SEXP_FIXNUM, max_int(ENTRY_BITS(a), ENTRY_BITS(b)));
  return a == b ? a : SEXP_OBJECT;
}

/* Which ways can the fixnum param p be stepped by 1 without overflow */
/* in the pass and fail branches of test?  The flags are or'ed in. */
#define STEP_UP   1
#define STEP_DOWN 2

static void analyze_cnd_bound (sexp ctx, sexp lam, sexp test, sexp p_sym,
                               int *pass_ok, int *fail_ok) {
  int *tmp, b, rel, strict, nonstrict;
  sexp op, a0, a1;
  while (sexp_pairp(test) && sexp_refp(sexp_car(test)) &&
         sexp_ref_name(sexp_car(test)) == sexp_intern(ctx, "not", 3) &&
         !sexp_lambdap(sexp_ref_loc(sexp_car(test))) &&
         sexp_pairp(sexp_cdr(test)) && sexp_nullp(sexp_cddr(test))) {
    test = sexp_cadr(test);
    tmp = pass_ok; pass_ok = fail_ok; fail_ok = tmp;
  }
  if (!sexp_pairp(test) || !sexp_opcodep(sexp_car(test))) return;
  op = sexp_car(test);
  if (!sexp_pairp(sexp_cdr(test)) || !sexp_pairp(sexp_cddr(test))
      || !sexp_nullp(sexp_cdr(sexp_cddr(test))))
    return;
  a0 = sexp_cadr(test);
  a1 = sexp_caddr(test);
  /* rel: -2 for p < b, -1 for p <= b, 0 for p = b, 1 for p >= b, 2 for p > b */
  switch (sexp_opcode_code(op)) {
  case SEXP_OP_LT:  rel = sexp_opcode_inverse(op) ? 2 : -2; break;
  case SEXP_OP_LE:  rel = sexp_opcode_inverse(op) ? 1 : -1; break;
  case SEXP_OP_EQN: rel = 0; break;
  default: return;
  }
  if (is_ref_to_var(a0, lam, p_sym)) {
    b = fixnum_bits(ctx, a1);
  } else if (is_ref_to_var(a1, lam, p_sym)) {
    b = fixnum_bits(ctx, a0);
    rel = -rel;
  } else {
    return;
  }
  if (b < 0) return;
  strict = b <= FIXNUM_MAG_BITS;         /* p < b <= max, or p > b >= min */
  nonstrict = b <= FIXNUM_MAG_BITS - 1;  /* b < max and b > min */
  switch (rel) {
  case -2:
    if (strict) *pass_ok |= STEP_UP;
    if (nonstrict) *fail_ok |= STEP_DOWN;
    break;
  case -1:
    if (nonstrict) *pass_ok |= STEP_UP;
    if (strict) *fail_ok |= STEP_DOWN;
    break;
  case 0:
    if (nonstrict) *pass_ok |= STEP_UP | STEP_DOWN;
    break;
  case 1:
    if (nonstrict) *pass_ok |= STEP_DOWN;
    if (strict) *fail_ok |= STEP_UP;
    break;
  case 2:
    if (strict) *pass_ok |= STEP_DOWN;
    if (nonstrict) *fail_ok |= STEP_UP;
    break;
  }
}

/* How a recursive call passes param p as arg k. */
#define STEP_OTHER 0
#define STEP_SAME  4

static int check_recursive_step (sexp ctx, sexp lam, sexp app, sexp p_sym, int k) {
  sexp args = sexp_cdr(app), arg_k, op, a0, a1;
  sexp_sint_t c;
  for (int i = 0; i < k && sexp_pairp(args); i++)
    args = sexp_cdr(args);
  if (!sexp_pairp(args)) return STEP_OTHER;
  arg_k = sexp_car(args);
  if (is_ref_to_var(arg_k, lam, p_sym)) return STEP_SAME;
  if (!sexp_pairp(arg_k) || !sexp_opcodep(sexp_car(arg_k))
      || !sexp_pairp(sexp_cdr(arg_k)) || !sexp_pairp(sexp_cddr(arg_k))
      || !sexp_nullp(sexp_cdr(sexp_cddr(arg_k))))
    return STEP_OTHER;
  op = sexp_car(arg_k);
  a0 = sexp_cadr(arg_k);
  a1 = sexp_caddr(arg_k);
  if (sexp_opcode_code(op) == SEXP_OP_ADD) {
    if (!is_ref_to_var(a0, lam, p_sym)) { sexp t = a0; a0 = a1; a1 = t; }
  } else if (sexp_opcode_code(op) == SEXP_OP_SUB) {
    if (!sexp_fixnum_literalp(a1, &c)) return STEP_OTHER;
    a1 = sexp_make_fixnum(-c);
  } else {
    return STEP_OTHER;
  }
  if (!is_ref_to_var(a0, lam, p_sym) || !sexp_fixnum_literalp(a1, &c))
    return STEP_OTHER;
  return c == 0 ? STEP_SAME : c == 1 ? STEP_UP : c == -1 ? STEP_DOWN : STEP_OTHER;
}

static int is_call_to_target (sexp op, sexp target_lam, sexp name, sexp loc) {
  if (op == target_lam) return 1;
  if (sexp_refp(op)) {
    if (sexp_ref_loc(op) == target_lam)
      return 1;
    if (!name && target_lam && sexp_lambdap(target_lam))
      name = sexp_lambda_name(target_lam);
    if (name && sexp_symbolp(name) && sexp_ref_name(op) == name) {
      if (loc && sexp_ref_loc(op) == loc)
        return 1;
      if (!loc)
        return 1;
    }
  }
  return 0;
}

static void process_external_call (sexp ctx, sexp app, int *types, int num_params) {
  sexp args = sexp_cdr(app);
  for (int k = 0; k < num_params; k++) {
    if (!sexp_pairp(args)) {
      types[k] = SEXP_OBJECT;
      continue;
    }
    types[k] = merge_entry(types[k], infer_expr_entry(ctx, sexp_car(args)));
    args = sexp_cdr(args);
  }
}

static void scan_external_calls (sexp ctx, sexp target_lam, sexp name, sexp loc,
                                 sexp x, int *types, int num_params) {
  if (!x || !sexp_pointerp(x) || sexp_litp(x)) return;
  int tag = sexp_pointer_tag(x);
  if (tag == SEXP_LAMBDA) {
    if (x == target_lam) return;
    scan_external_calls(ctx, target_lam, name, loc, sexp_lambda_body(x), types, num_params);
    return;
  }
  if (tag == SEXP_SET) {
    scan_external_calls(ctx, target_lam, name, loc, sexp_set_value(x), types, num_params);
    return;
  }
  if (tag == SEXP_SEQ) {
    for (sexp ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls))
      scan_external_calls(ctx, target_lam, name, loc, sexp_car(ls), types, num_params);
    return;
  }
  if (tag == SEXP_CND) {
    scan_external_calls(ctx, target_lam, name, loc, sexp_cnd_test(x), types, num_params);
    scan_external_calls(ctx, target_lam, name, loc, sexp_cnd_pass(x), types, num_params);
    scan_external_calls(ctx, target_lam, name, loc, sexp_cnd_fail(x), types, num_params);
    return;
  }
  if (tag == SEXP_SYNCLO) {
    scan_external_calls(ctx, target_lam, name, loc, sexp_synclo_expr(x), types, num_params);
    return;
  }
  if (tag == SEXP_PAIR) {
    sexp op = sexp_car(x);
    if (is_call_to_target(op, target_lam, name, loc)) {
      process_external_call(ctx, x, types, num_params);
    } else {
      scan_external_calls(ctx, target_lam, name, loc, op, types, num_params);
    }
    for (sexp ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
      scan_external_calls(ctx, target_lam, name, loc, sexp_car(ls), types, num_params);
    }
  }
}

/* What the recursive calls pass for each param, besides its type. */
struct param_steps {
  int arg_bits;  /* max magnitude of args other than steps */
  int stepped;   /* some call steps it by +-1 */
  int unguarded; /* some step isn't guarded against overflow */
};

static void scan_recursive_calls (sexp ctx, sexp target_lam, sexp name, sexp loc,
                                  sexp x, int *types, struct param_steps *steps,
                                  int num_params, const int *ok);

/* Scan the body of a let, app, with its params known to be flonums */
/* where their values are, so the recursive calls in it see them, as */
/* those of a loop param stepped through a let.  Otherwise they keep */
/* the types found so far, which are sound. */
static void scan_let_recursive_calls (sexp ctx, sexp target_lam, sexp name, sexp loc,
                                      sexp app, int *types, struct param_steps *steps,
                                      int num_params, const int *ok) {
  sexp let = sexp_car(app), p, a, old, t;
  sexp_gc_var2(hyp, saved);
  for (p = sexp_lambda_params(let), a = sexp_cdr(app); sexp_pairp(p) && sexp_pairp(a);
       p = sexp_cdr(p), a = sexp_cdr(a))
    ;
  if (!sexp_nullp(p) || !sexp_nullp(a)) {
    scan_recursive_calls(ctx, target_lam, name, loc, let, types, steps, num_params, ok);
    return;
  }
  sexp_gc_preserve2(ctx, hyp, saved);
  saved = sexp_lambda_param_types(let);
  hyp = SEXP_NULL;
  for (p = sexp_lambda_params(let), a = sexp_cdr(app), old = saved; sexp_pairp(p);
       p = sexp_cdr(p), a = sexp_cdr(a), old = sexp_pairp(old) ? sexp_cdr(old) : old) {
    if (ENTRY_TYPE(infer_expr_entry(ctx, sexp_car(a))) == SEXP_FLONUM
        && !sexp_truep(sexp_memq(ctx, sexp_car(p), sexp_lambda_sv(let))))
      t = sexp_make_fixnum(SEXP_FLONUM);
    else if (sexp_pairp(old))
      t = sexp_car(old);
    else
      t = sexp_make_fixnum(SEXP_OBJECT);
    hyp = sexp_cons(ctx, t, hyp);
  }
  sexp_lambda_param_types(let) = sexp_nreverse(ctx, hyp);
  scan_recursive_calls(ctx, target_lam, name, loc, let, types, steps, num_params, ok);
  sexp_lambda_param_types(let) = saved;
  sexp_gc_release2(ctx);
}

static void scan_recursive_calls (sexp ctx, sexp target_lam, sexp name, sexp loc,
                                  sexp x, int *types, struct param_steps *steps,
                                  int num_params, const int *ok) {
  if (!x || !sexp_pointerp(x) || sexp_litp(x)) return;
  int tag = sexp_pointer_tag(x);
  if (tag == SEXP_LAMBDA) {
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_lambda_body(x), types, steps, num_params, ok);
    return;
  }
  if (tag == SEXP_SET) {
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_set_value(x), types, steps, num_params, ok);
    return;
  }
  if (tag == SEXP_SEQ) {
    for (sexp ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls))
      scan_recursive_calls(ctx, target_lam, name, loc, sexp_car(ls), types, steps, num_params, ok);
    return;
  }
  if (tag == SEXP_CND) {
    int stack_ok[64], *pass_ok, *fail_ok;
    sexp params = sexp_lambda_params(target_lam);
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_cnd_test(x), types, steps, num_params, ok);
    pass_ok = (2 * num_params <= 64) ? stack_ok : (int *)malloc(2 * num_params * sizeof(int));
    if (!pass_ok) return;
    fail_ok = pass_ok + num_params;
    for (int k = 0; k < num_params; k++, params = sexp_cdr(params)) {
      pass_ok[k] = fail_ok[k] = ok[k];
      if (ENTRY_TYPE(types[k]) == SEXP_FIXNUM)
        analyze_cnd_bound(ctx, target_lam, sexp_cnd_test(x), sexp_car(params),
                          &pass_ok[k], &fail_ok[k]);
    }
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_cnd_pass(x), types, steps, num_params, pass_ok);
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_cnd_fail(x), types, steps, num_params, fail_ok);
    if (pass_ok != stack_ok) free(pass_ok);
    return;
  }
  if (tag == SEXP_SYNCLO) {
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_synclo_expr(x), types, steps, num_params, ok);
    return;
  }
  if (tag == SEXP_PAIR) {
    sexp op = sexp_car(x);
    if (is_call_to_target(op, target_lam, name, loc)) {
      sexp args = sexp_cdr(x);
      sexp params = sexp_lambda_params(target_lam);
      for (int k = 0; k < num_params; k++, params = sexp_cdr(params)) {
        if (!sexp_pairp(args)) {
          types[k] = SEXP_OBJECT;
          continue;
        }
        sexp arg_k = sexp_car(args);
        args = sexp_cdr(args);
        if (types[k] == SEXP_OBJECT || types[k] == -1)
          continue;
        int step = check_recursive_step(ctx, target_lam, x, sexp_car(params), k);
        if (step == STEP_SAME)
          continue;
        if (ENTRY_TYPE(types[k]) == SEXP_FIXNUM) {
          if (step == STEP_UP || step == STEP_DOWN) {
            steps[k].stepped = 1;
            if (!(ok[k] & step)) steps[k].unguarded = 1;
          } else {
            int b = fixnum_bits(ctx, arg_k);
            if (b >= 0 && b <= FIXNUM_MAG_BITS)
              steps[k].arg_bits = max_int(steps[k].arg_bits, b);
            else
              types[k] = SEXP_OBJECT;
          }
        } else if (infer_expr_entry(ctx, arg_k) != types[k]) {
          types[k] = SEXP_OBJECT;
        }
      }
    } else if (sexp_lambdap(op)) {
      scan_let_recursive_calls(ctx, target_lam, name, loc, x, types, steps, num_params, ok);
    } else {
      scan_recursive_calls(ctx, target_lam, name, loc, op, types, steps, num_params, ok);
    }
    for (sexp ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
      scan_recursive_calls(ctx, target_lam, name, loc, sexp_car(ls), types, steps, num_params, ok);
    }
  }
}

/* Types demanded on entry: walking from a lambda's entry in evaluation */
/* order through code that can't fail or have effects, the first ops */
/* that can fail type check its params.  Checking those types once on */
/* entry raises the same errors at the same point, and every later use */
/* (including in loops) is then known to have the type. */

#define DEMAND_MAX_DEPTH 16
#define DEMAND_MAX_BINDINGS 16

enum { WALK_STOP, WALK_SAFE, WALK_FOUND };

struct demand_walk {
  sexp lam;                       /* whose params are demanded */
  int num_params, num_found;
  int *types;                     /* per param, or -1 */
  int *order;                     /* per param, the check order */
  int depth, num_bindings;
  sexp binding_name[DEMAND_MAX_BINDINGS], binding_loc[DEMAND_MAX_BINDINGS];
  sexp binding_lam[DEMAND_MAX_BINDINGS];
};

/* the container type arg0 of op is checked against, or -1; *only is */
/* set if that's the only way op can fail */
static int demand_op_type (sexp op, int *only) {
  *only = 0;
  switch (sexp_opcode_code(op)) {
  case SEXP_OP_CAR: case SEXP_OP_CDR:
    *only = 1;
  case SEXP_OP_SET_CAR: case SEXP_OP_SET_CDR:
    return SEXP_PAIR;
  case SEXP_OP_VECTOR_LENGTH:
    *only = 1;
  case SEXP_OP_VECTOR_REF: case SEXP_OP_VECTOR_SET:
    return SEXP_VECTOR;
  case SEXP_OP_BYTES_LENGTH:
    *only = 1;
  case SEXP_OP_BYTES_REF: case SEXP_OP_BYTES_SET:
    return SEXP_BYTES;
  case SEXP_OP_STRING_LENGTH:
    *only = 1;
    return SEXP_STRING;
#if SEXP_USE_UNIFORM_VECTOR_LITERALS
  case SEXP_OP_UVECTOR_LENGTH:
    *only = 1;
  case SEXP_OP_UVECTOR_REF: case SEXP_OP_UVECTOR_SET:
    if (!sexp_fixnump(sexp_opcode_data(op))) return -1;
    return UVECTOR_ENTRY(sexp_unbox_fixnum(sexp_opcode_data(op)));
#endif
  default:
    return -1;
  }
}

/* the index of the param of w->lam x is an unmutated ref to, or -1 */
static int demand_param_index (struct demand_walk *w, sexp x) {
  sexp ls;
  int k;
  if (!x || !sexp_pointerp(x) || !sexp_refp(x) || sexp_ref_loc(x) != w->lam)
    return -1;
  for (ls = sexp_lambda_sv(w->lam); sexp_pairp(ls); ls = sexp_cdr(ls))
    if (sexp_car(ls) == sexp_ref_name(x)) return -1;
  for (k = 0, ls = sexp_lambda_params(w->lam); sexp_pairp(ls); ls = sexp_cdr(ls), k++)
    if (sexp_car(ls) == sexp_ref_name(x)) return k;
  return -1;
}

static int demand_walk_expr (sexp ctx, struct demand_walk *w, sexp x);

/* is x a constant: an immediate, a literal, or a value such as a */
/* flonum left in the AST as is */
static int ast_constantp (sexp x) {
  if (!x || !sexp_pointerp(x)) return 1;
  switch (sexp_pointer_tag(x)) {
  case SEXP_PAIR: case SEXP_LAMBDA: case SEXP_REF: case SEXP_SET:
  case SEXP_SET_SYN: case SEXP_CND: case SEXP_SEQ: case SEXP_SYNCLO:
    return 0;
  default:
    return 1;
  }
}

/* evaluate a lambda's body as called with already evaluated args */
static int demand_walk_body (sexp ctx, struct demand_walk *w, sexp lam) {
  int res;
  if (w->depth >= DEMAND_MAX_DEPTH) return WALK_STOP;
  w->depth++;
  res = demand_walk_expr(ctx, w, sexp_lambda_body(lam));
  w->depth--;
  /* the body's value is returned, not something we can continue from */
  return res == WALK_FOUND ? WALK_FOUND : WALK_STOP;
}

/* evaluate args, which must all be safe but at most one */
static int demand_walk_args (sexp ctx, struct demand_walk *w, sexp args) {
  sexp ls, unsafe = NULL;
  for (ls = args; sexp_pairp(ls); ls = sexp_cdr(ls)) {
    sexp a = sexp_car(ls);
    if (ast_constantp(a) || sexp_lambdap(a)
        || (sexp_refp(a) && sexp_lambdap(sexp_ref_loc(a))))
      continue;
    if (unsafe) return WALK_STOP;
    unsafe = a;
  }
  return unsafe ? demand_walk_expr(ctx, w, unsafe) : WALK_SAFE;
}

static int demand_walk_expr (sexp ctx, struct demand_walk *w, sexp x) {
  sexp ls, op;
  int i, res, type, only, k;
  if (ast_constantp(x)) return WALK_SAFE;
  switch (sexp_pointer_tag(x)) {
  case SEXP_LAMBDA:
    return WALK_SAFE;
  case SEXP_REF:
    /* globals may be undefined */
    return sexp_lambdap(sexp_ref_loc(x)) ? WALK_SAFE : WALK_STOP;
  case SEXP_SYNCLO:
    return demand_walk_expr(ctx, w, sexp_synclo_expr(x));
  case SEXP_SEQ:
    for (ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls))
      if ((res = demand_walk_expr(ctx, w, sexp_car(ls))) != WALK_SAFE)
        return res;
    return WALK_SAFE;
  case SEXP_SET:
    if (!sexp_refp(sexp_set_var(x)) || !sexp_lambdap(sexp_ref_loc(sexp_set_var(x))))
      return WALK_STOP;
    if ((res = demand_walk_expr(ctx, w, sexp_set_value(x))) != WALK_SAFE)
      return res;
    /* remember local procedures, to follow calls to them */
    for (i = 0; i < w->num_bindings; i++)
      if (w->binding_name[i] == sexp_ref_name(sexp_set_var(x))
          && w->binding_loc[i] == sexp_ref_loc(sexp_set_var(x)))
        break;
    if (i == w->num_bindings) {
      if (i == DEMAND_MAX_BINDINGS) return WALK_STOP;
      w->num_bindings++;
      w->binding_name[i] = sexp_ref_name(sexp_set_var(x));
      w->binding_loc[i] = sexp_ref_loc(sexp_set_var(x));
    }
    w->binding_lam[i] = sexp_lambdap(sexp_set_value(x)) ? sexp_set_value(x) : NULL;
    return WALK_SAFE;
  case SEXP_CND:
    res = demand_walk_expr(ctx, w, sexp_cnd_test(x));
    if (res != WALK_SAFE) return res;
    /* both branches would have to make the same next checks */
    return WALK_STOP;
  case SEXP_PAIR:
    op = sexp_car(x);
    if (sexp_opcodep(op)) {
      res = demand_walk_args(ctx, w, sexp_cdr(x));
      if (res != WALK_SAFE) return res;
      if (sexp_opcode_class(op) == SEXP_OPC_TYPE_PREDICATE
          && sexp_opcode_code(op) != SEXP_OP_ISA)
        return WALK_SAFE;
      if (sexp_opcode_code(op) == SEXP_OP_EQ)
        return WALK_SAFE;
      type = demand_op_type(op, &only);
      if (type < 0 || !sexp_pairp(sexp_cdr(x))) return WALK_STOP;
      k = demand_param_index(w, sexp_cadr(x));
      if (k < 0) return WALK_STOP;
      if (w->types[k] < 0) {
        w->types[k] = type;
        w->order[k] = w->num_found++;
      } else if (w->types[k] != type) {
        /* always fails, on a later check */
        return WALK_FOUND;
      }
      /* continue past ops that can fail only on that type check */
      return only ? WALK_SAFE : WALK_FOUND;
    }
    if (sexp_lambdap(op)) {
      res = demand_walk_args(ctx, w, sexp_cdr(x));
      if (res != WALK_SAFE) return res;
      return demand_walk_body(ctx, w, op);
    }
    if (sexp_refp(op)) {
      for (i = 0; i < w->num_bindings; i++)
        if (w->binding_name[i] == sexp_ref_name(op) && w->binding_loc[i] == sexp_ref_loc(op))
          break;
      if (i == w->num_bindings || !w->binding_lam[i]
          || sexp_length(ctx, sexp_lambda_params(w->binding_lam[i]))
             != sexp_length(ctx, sexp_cdr(x)))
        return WALK_STOP;
      res = demand_walk_args(ctx, w, sexp_cdr(x));
      if (res != WALK_SAFE) return res;
      return demand_walk_body(ctx, w, w->binding_lam[i]);
    }
    return WALK_STOP;
  default:
    return WALK_STOP;
  }
}

/* give unknown params of lam the types demanded on entry */
static void apply_demanded_types (sexp ctx, sexp lam, int *types, int num_params) {
  int stack[64], *dtypes = stack, *order, k;
  struct demand_walk w;
  if (2 * num_params > 64) {
    dtypes = (int *)malloc(2 * num_params * sizeof(int));
    if (!dtypes) return;
  }
  order = dtypes + num_params;
  for (k = 0; k < num_params; k++) dtypes[k] = -1;
  w.lam = lam;
  w.num_params = num_params;
  w.num_found = 0;
  w.types = dtypes;
  w.order = order;
  w.depth = 0;
  w.num_bindings = 0;
  demand_walk_expr(ctx, &w, sexp_lambda_body(lam));
  for (k = 0; k < num_params; k++)
    if (dtypes[k] >= 0 && types[k] == SEXP_OBJECT)
      types[k] = CHECKED_ENTRY(dtypes[k], order[k]);
  if (dtypes != stack) free(dtypes);
}

/* the type param k of lam is checked to have on entry, or -1, and in */
/* *order where that check comes among them, and in *subtype the */
/* uniform vector element type checked, or 0 */
int sexp_param_checked_type (sexp lam, int k, int *order, int *subtype) {
  sexp ls = sexp_lambda_param_types(lam);
  int e;
  for (; k > 0 && sexp_pairp(ls); k--) ls = sexp_cdr(ls);
  if (!sexp_pairp(ls) || !sexp_fixnump(sexp_car(ls))) return -1;
  e = (int)sexp_unbox_fixnum(sexp_car(ls));
  if (!(e & ENTRY_CHECKED)) return -1;
  *order = ENTRY_BITS(e);
  *subtype = ENTRY_SUBTYPE(e);
  return ENTRY_TYPE(e);
}

/* set when a flonum param assumption fails, which may be from */
/* lacking the types of the lambdas visited later */
static int flonum_assumption_failed;

static int count_flonum_entries (const int *types, int num_params) {
  int k, n = 0;
  for (k = 0; k < num_params; k++)
    if (types[k] >= 0 && ENTRY_TYPE(types[k]) == SEXP_FLONUM) n++;
  return n;
}

/* Scan the recursive calls, assuming the params all calls so far pass */
/* flonums are flonums, so a loop param stepped by flonum arithmetic on */
/* itself stays a flonum.  Params passed something else lose the */
/* assumption, and the scan repeats until it holds.  Only these */
/* assumptions are visible: fixnum bounds can't be found inductively, */
/* and mutated params can be anything. */
static void scan_recursive_calls_flonums (sexp ctx, sexp lam, sexp name, sexp loc,
                                          int *types, struct param_steps *steps,
                                          int num_params, const int *ok) {
  int k, n = count_flonum_entries(types, num_params), prev, first = n;
  sexp sv = sexp_lambda_sv(lam), params;
  sexp_gc_var1(hyp);
  if (n == 0) {
    scan_recursive_calls(ctx, lam, name, loc, sexp_lambda_body(lam), types, steps, num_params, ok);
    return;
  }
  sexp_gc_preserve1(ctx, hyp);
  do {
    prev = n;
    hyp = SEXP_NULL;
    for (k = num_params - 1; k >= 0; k--) {
      for (params = sexp_lambda_params(lam), n = 0; n < k; n++) params = sexp_cdr(params);
      hyp = sexp_cons(ctx, sexp_make_fixnum(
                (types[k] >= 0 && ENTRY_TYPE(types[k]) == SEXP_FLONUM
                 && !sexp_truep(sexp_memq(ctx, sexp_car(params), sv)))
                ? SEXP_FLONUM : SEXP_OBJECT), hyp);
    }
    sexp_lambda_param_types(lam) = hyp;
    scan_recursive_calls(ctx, lam, name, loc, sexp_lambda_body(lam), types, steps, num_params, ok);
    n = count_flonum_entries(types, num_params);
  } while (n != prev);
  if (n != first) flonum_assumption_failed = 1;
  sexp_lambda_param_types(lam) = SEXP_NULL;
  sexp_gc_release1(ctx);
}

static void analyze_lambda_param_types (sexp ctx, sexp lam, sexp prev_lam, sexp top_expr, sexp name, sexp loc) {
  if (!lam || !sexp_lambdap(lam)) return;
  /* don't reason from types stored by an earlier analysis */
  sexp_lambda_param_types(lam) = SEXP_NULL;
  sexp params = sexp_lambda_params(lam);
  if (!sexp_listp(ctx, params) || sexp_nullp(params)) {
    sexp_lambda_param_types(lam) = SEXP_NULL;
    return;
  }

  int num_params = (int)sexp_unbox_fixnum(sexp_length(ctx, params));
  int stack_types[32], stack_ok[32];
  struct param_steps stack_steps[32];
  int *types = stack_types, *ok = stack_ok;
  struct param_steps *steps = stack_steps;
  if (num_params > 32) {
    types = (int *)malloc(num_params * sizeof(int));
    ok = (int *)malloc(num_params * sizeof(int));
    steps = (struct param_steps *)malloc(num_params * sizeof(struct param_steps));
    if (!types || !ok || !steps) {
      free(types); free(ok); free(steps);
      sexp_lambda_param_types(lam) = SEXP_NULL;
      return;
    }
  }

  for (int k = 0; k < num_params; k++) {
    types[k] = -1;
    ok[k] = 0;
    steps[k].arg_bits = 0;
    steps[k].stepped = steps[k].unguarded = 0;
  }

  if (sexp_lambda_escapes_p(lam)) {
    /* callers are unknown */
    for (int k = 0; k < num_params; k++) types[k] = SEXP_OBJECT;
  } else {
    if (prev_lam && sexp_lambdap(prev_lam)) {
      scan_external_calls(ctx, lam, name, loc, sexp_lambda_body(prev_lam), types, num_params);
    }
    if (top_expr) {
      scan_external_calls(ctx, lam, name, loc, top_expr, types, num_params);
    }
    scan_recursive_calls_flonums(ctx, lam, name, loc, types, steps, num_params, ok);
  }

  sexp p = params;
  for (int k = 0; k < num_params; k++, p = sexp_cdr(p)) {
    sexp p_sym = sexp_car(p);
    if (types[k] == -1 || !sexp_symbolp(p_sym)
        || sexp_truep(sexp_memq(ctx, p_sym, sexp_lambda_sv(lam)))) {
      types[k] = SEXP_OBJECT;
    } else if (ENTRY_TYPE(types[k]) == SEXP_FIXNUM) {
      /* the values are the initial args, other recursive args, or steps */
      int b = max_int(ENTRY_BITS(types[k]), steps[k].arg_bits);
      if (steps[k].stepped) {
        if (b <= SMALL_START_BITS)
          b = SMALL_START_BITS + 1;
        else if (!steps[k].unguarded)
          b = FIXNUM_MAG_BITS;
        else
          b = -1;
      }
      types[k] = b < 0 ? SEXP_OBJECT : TYPE_ENTRY(SEXP_FIXNUM, b);
    }
  }

  apply_demanded_types(ctx, lam, types, num_params);

  sexp_gc_var1(types_list);
  sexp_gc_preserve1(ctx, types_list);
  types_list = SEXP_NULL;
  for (int k = num_params - 1; k >= 0; k--) {
    types_list = sexp_cons(ctx, sexp_make_fixnum(types[k]), types_list);
  }
  sexp_lambda_param_types(lam) = types_list;
  sexp_gc_release1(ctx);

  if (types != stack_types) {
    free(types); free(ok); free(steps);
  }
}

/************************* Inline Lets ********************************/

/* Is app a let, ((lambda (params ...) body) args ...) within lam, */
/* which native code compiles inline, binding the params in lam's */
/* frame instead of making and calling a closure? */
int sexp_inline_letp (sexp ctx, sexp lam, sexp app) {
  sexp op, p, a;
  if (!lam || !sexp_lambdap(lam) || !sexp_pairp(app)) return 0;
  op = sexp_car(app);
  if (!sexp_lambdap(op) || sexp_pairp(sexp_lambda_locals(op))) return 0;
  for (p = sexp_lambda_params(op), a = sexp_cdr(app);
       sexp_pairp(p) && sexp_pairp(a); p = sexp_cdr(p), a = sexp_cdr(a))
    ;
  return sexp_nullp(p) && sexp_nullp(a);
}

/************************* Unboxed Flonums ****************************/

/* Native code keeps a flonum param unboxed, as a raw double in its */
/* frame, when the param is inferred to be a flonum without a check, */
/* is never mutated or captured, and is used at least once as an */
/* operand of float arithmetic or the value stored in a float vector, */
/* and otherwise only passed on to unboxed params by self tail calls */
/* or lets, or returned from tail position (boxed once on the way out). */
/* Other uses would box it each time, so are left to the boxed value. */

struct unbox_cand { sexp lam; int k, alive, good, bad; };
struct unbox_state { struct unbox_cand *c; int n, size; };
#define UNBOX_MAX_CHAIN 32

static int param_position (sexp lam, sexp name) {
  sexp ls = sexp_lambda_params(lam);
  int k = 0;
  for (; sexp_pairp(ls); ls = sexp_cdr(ls), k++)
    if (sexp_car(ls) == name) return k;
  return -1;
}

static sexp param_types_ref (sexp lam, int k) {
  sexp ls = sexp_lambda_param_types(lam);
  for (; k > 0 && sexp_pairp(ls); k--) ls = sexp_cdr(ls);
  return sexp_pairp(ls) ? ls : NULL;
}

static struct unbox_cand *unbox_find (struct unbox_state *st, sexp lam, int k) {
  for (int i = 0; i < st->n; i++)
    if (st->c[i].lam == lam && st->c[i].k == k && st->c[i].alive)
      return &st->c[i];
  return NULL;
}

/* the live candidate the ref x is to, or NULL */
static struct unbox_cand *unbox_ref_cand (struct unbox_state *st, sexp x) {
  if (!x || !sexp_pointerp(x) || !sexp_refp(x) || !sexp_lambdap(sexp_ref_loc(x)))
    return NULL;
  return unbox_find(st, sexp_ref_loc(x), param_position(sexp_ref_loc(x), sexp_ref_name(x)));
}

static void unbox_collect (sexp ctx, struct unbox_state *st, sexp x) {
  sexp ls, cell;
  int k;
  if (!x || !sexp_pointerp(x)) return;
  switch (sexp_pointer_tag(x)) {
  case SEXP_LAMBDA:
    if (sexp_truep(sexp_listp(ctx, sexp_lambda_params(x)))) {
      for (k = 0, ls = sexp_lambda_params(x); sexp_pairp(ls); ls = sexp_cdr(ls), k++) {
        cell = param_types_ref(x, k);
        if (!cell || sexp_car(cell) != sexp_make_fixnum(SEXP_FLONUM)
            || sexp_truep(sexp_memq(ctx, sexp_car(ls), sexp_lambda_sv(x))))
          continue;
        if (st->n == st->size) {
          int size = st->size ? 2 * st->size : 16;
          struct unbox_cand *c = realloc(st->c, size * sizeof(struct unbox_cand));
          if (!c) return;
          st->c = c;
          st->size = size;
        }
        st->c[st->n].lam = x;
        st->c[st->n].k = k;
        st->c[st->n].alive = 1;
        st->n++;
      }
    }
    unbox_collect(ctx, st, sexp_lambda_body(x));
    break;
  case SEXP_SET:
    unbox_collect(ctx, st, sexp_set_value(x));
    break;
  case SEXP_PAIR:
    for (ls = x; sexp_pairp(ls); ls = sexp_cdr(ls))
      unbox_collect(ctx, st, sexp_car(ls));
    break;
  case SEXP_CND:
    unbox_collect(ctx, st, sexp_cnd_test(x));
    unbox_collect(ctx, st, sexp_cnd_pass(x));
    unbox_collect(ctx, st, sexp_cnd_fail(x));
    break;
  case SEXP_SEQ:
    for (ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls))
      unbox_collect(ctx, st, sexp_car(ls));
    break;
  case SEXP_SYNCLO:
    unbox_collect(ctx, st, sexp_synclo_expr(x));
    break;
  }
}

/* the walk's position: the innermost procedure and its self name, the */
/* lets inlined in it so far, whose params share its frame, and when */
/* in tail position, the procedure and lets it's the tail of */
struct unbox_pos {
  sexp fn, name, loc;
  sexp frame[UNBOX_MAX_CHAIN];
  sexp chain[UNBOX_MAX_CHAIN];
  int nframe, nchain;
};

static int unbox_in_frame (const struct unbox_pos *pos, sexp lam) {
  for (int i = 0; i < pos->nframe; i++)
    if (pos->frame[i] == lam) return 1;
  return 0;
}

static int unbox_tail_of (const struct unbox_pos *pos, sexp lam) {
  for (int i = 0; i < pos->nchain; i++)
    if (pos->chain[i] == lam) return 1;
  return 0;
}

static void unbox_scan (sexp ctx, struct unbox_state *st, sexp x,
                        const struct unbox_pos *pos);

static void unbox_scan_nontail (sexp ctx, struct unbox_state *st, sexp x,
                                const struct unbox_pos *pos) {
  struct unbox_pos p = *pos;
  p.nchain = 0;
  unbox_scan(ctx, st, x, &p);
}

/* x is passed on to the param k of lam */
static void unbox_scan_copy (sexp ctx, struct unbox_state *st, sexp x,
                             sexp lam, int k, const struct unbox_pos *pos) {
  struct unbox_cand *c = unbox_ref_cand(st, x);
  if (!c)
    unbox_scan_nontail(ctx, st, x, pos);
  else if (!unbox_find(st, lam, k) || !unbox_in_frame(pos, c->lam))
    c->bad = 1;
}

/* x is computed as a raw double where it's an unboxed candidate */
static void unbox_scan_float (sexp ctx, struct unbox_state *st, sexp x,
                              const struct unbox_pos *pos) {
  struct unbox_cand *c = unbox_ref_cand(st, x);
  if (c && unbox_in_frame(pos, c->lam))
    c->good = 1;
  else if (c)
    c->bad = 1;
  else
    unbox_scan_nontail(ctx, st, x, pos);
}

static void unbox_scan_lambda (sexp ctx, struct unbox_state *st, sexp lam,
                               sexp name, sexp loc) {
  struct unbox_pos p;
  if (!name && sexp_symbolp(sexp_lambda_name(lam)))
    name = sexp_lambda_name(lam);
  if (name && !loc && sexp_symbolp(name))
    loc = sexp_env_cell(ctx, sexp_context_env(ctx), name, 0);
  p.fn = lam;
  p.name = name;
  p.loc = loc;
  p.frame[0] = p.chain[0] = lam;
  p.nframe = p.nchain = 1;
  unbox_scan(ctx, st, sexp_lambda_body(lam), &p);
}

static void unbox_scan (sexp ctx, struct unbox_state *st, sexp x,
                        const struct unbox_pos *pos) {
  struct unbox_cand *c;
  struct unbox_pos p;
  sexp ls, op;
  int k;
  if (!x || !sexp_pointerp(x)) return;
  switch (sexp_pointer_tag(x)) {
  case SEXP_REF:
    if ((c = unbox_ref_cand(st, x)) && !unbox_tail_of(pos, sexp_ref_loc(x)))
      c->bad = 1;
    break;
  case SEXP_LAMBDA:
    unbox_scan_lambda(ctx, st, x, NULL, NULL);
    break;
  case SEXP_SET:
    if (sexp_lambdap(sexp_set_value(x)) && sexp_refp(sexp_set_var(x)))
      unbox_scan_lambda(ctx, st, sexp_set_value(x), sexp_ref_name(sexp_set_var(x)),
                        sexp_ref_loc(sexp_set_var(x)));
    else
      unbox_scan_nontail(ctx, st, sexp_set_value(x), pos);
    break;
  case SEXP_CND:
    unbox_scan_nontail(ctx, st, sexp_cnd_test(x), pos);
    unbox_scan(ctx, st, sexp_cnd_pass(x), pos);
    unbox_scan(ctx, st, sexp_cnd_fail(x), pos);
    break;
  case SEXP_SEQ:
    for (ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
      if (sexp_pairp(sexp_cdr(ls)))
        unbox_scan_nontail(ctx, st, sexp_car(ls), pos);
      else
        unbox_scan(ctx, st, sexp_car(ls), pos);
    }
    break;
  case SEXP_SYNCLO:
    unbox_scan(ctx, st, sexp_synclo_expr(x), pos);
    break;
  case SEXP_PAIR:
    op = sexp_car(x);
    if (sexp_opcodep(op)) {
      sexp_sint_t code = sexp_opcode_code(op);
      int n = (int)sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(x)));
      if (n == 2 && (code == SEXP_OP_ADD || code == SEXP_OP_SUB
                     || code == SEXP_OP_MUL || code == SEXP_OP_DIV)) {
        for (ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls))
          unbox_scan_float(ctx, st, sexp_car(ls), pos);
        break;
      }
#if SEXP_USE_UNIFORM_VECTOR_LITERALS
      if (n == 3 && code == SEXP_OP_UVECTOR_SET
          && (sexp_opcode_data(op) == sexp_make_fixnum(SEXP_F64)
              || sexp_opcode_data(op) == sexp_make_fixnum(SEXP_F32))) {
        unbox_scan_nontail(ctx, st, sexp_cadr(x), pos);
        unbox_scan_nontail(ctx, st, sexp_caddr(x), pos);
        unbox_scan_float(ctx, st, sexp_car(sexp_cdddr(x)), pos);
        break;
      }
#endif
      for (ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls))
        unbox_scan_nontail(ctx, st, sexp_car(ls), pos);
    } else if (sexp_inline_letp(ctx, pos->fn, x)) {
      for (k = 0, ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls), k++)
        unbox_scan_copy(ctx, st, sexp_car(ls), op, k, pos);
      p = *pos;
      if (p.nframe < UNBOX_MAX_CHAIN)
        p.frame[p.nframe++] = op;
      if (p.nchain < UNBOX_MAX_CHAIN)
        p.chain[p.nchain++] = op;
      else
        p.chain[(p.nchain = 1) - 1] = op;
      unbox_scan(ctx, st, sexp_lambda_body(op), &p);
    } else if (pos->fn && pos->name && unbox_tail_of(pos, pos->fn)
               && sexp_refp(op) && sexp_ref_name(op) == pos->name
               && sexp_ref_loc(op) == pos->loc
               && sexp_truep(sexp_listp(ctx, sexp_lambda_params(pos->fn)))
               && sexp_length(ctx, sexp_cdr(x)) == sexp_length(ctx, sexp_lambda_params(pos->fn))) {
      /* a self tail call, which jumps to the start with the new params */
      for (k = 0, ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls), k++)
        unbox_scan_copy(ctx, st, sexp_car(ls), pos->fn, k, pos);
    } else {
      for (ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls))
        unbox_scan_nontail(ctx, st, sexp_car(ls), pos);
      unbox_scan_nontail(ctx, st, op, pos);
    }
    break;
  }
}

static void infer_unboxed (sexp ctx, sexp ast) {
  struct unbox_state st = {NULL, 0, 0};
  struct unbox_pos pos;
  sexp cell;
  int i, changed;
  unbox_collect(ctx, &st, ast);
  if (st.n > 0) {
    pos.fn = pos.name = pos.loc = NULL;
    pos.nframe = pos.nchain = 0;
    do {
      for (i = 0; i < st.n; i++) st.c[i].good = st.c[i].bad = 0;
      unbox_scan(ctx, &st, ast, &pos);
      for (i = changed = 0; i < st.n; i++)
        if (st.c[i].alive && (st.c[i].bad || !st.c[i].good))
          st.c[i].alive = 0, changed = 1;
    } while (changed);
    for (i = 0; i < st.n; i++)
      if (st.c[i].alive && (cell = param_types_ref(st.c[i].lam, st.c[i].k)))
        sexp_car(cell) = sexp_make_fixnum(SEXP_FLONUM | ENTRY_UNBOXED);
  }
  free(st.c);
}

int sexp_param_unboxedp (sexp lam, int k) {
  sexp cell = param_types_ref(lam, k);
  return cell && sexp_car(cell) == sexp_make_fixnum(SEXP_FLONUM | ENTRY_UNBOXED);
}

int sexp_unboxed_refp (sexp x) {
  sexp lam;
  if (!x || !sexp_pointerp(x) || !sexp_refp(x)) return 0;
  lam = sexp_ref_loc(x);
  return sexp_lambdap(lam)
    && sexp_param_unboxedp(lam, param_position(lam, sexp_ref_name(x)));
}

/************************* Allocation Analysis ************************/

/* Conservatively, can evaluating x allocate?  Self calls (to self_name */
/* at self_loc) are assumed not to, since they are checked recursively. */

int sexp_expr_may_allocp (sexp ctx, sexp lam, sexp x, sexp self_name, sexp self_loc) {
  if (!x || !sexp_pointerp(x) || sexp_litp(x))
    return 0;

  int tag = sexp_pointer_tag(x);
  switch (tag) {
  case SEXP_REF:
    /* boxing an unboxed flonum */
    return sexp_unboxed_refp(x);

  case SEXP_SET:
    return sexp_expr_may_allocp(ctx, lam, sexp_set_value(x), self_name, self_loc);

  case SEXP_SEQ: {
    for (sexp ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
      if (sexp_expr_may_allocp(ctx, lam, sexp_car(ls), self_name, self_loc))
        return 1;
    }
    return 0;
  }

  case SEXP_CND: {
    return sexp_expr_may_allocp(ctx, lam, sexp_cnd_test(x), self_name, self_loc) ||
           sexp_expr_may_allocp(ctx, lam, sexp_cnd_pass(x), self_name, self_loc) ||
           sexp_expr_may_allocp(ctx, lam, sexp_cnd_fail(x), self_name, self_loc);
  }

  case SEXP_LAMBDA:
    return 1;

  case SEXP_PAIR: {
    sexp op = sexp_car(x);
    if (sexp_opcodep(op)) {
      if (!sexp_opcode_non_alloc_p(op) && !sexp_proven_fixnump(ctx, lam, x))
        return 1;

      for (sexp ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
        if (sexp_expr_may_allocp(ctx, lam, sexp_car(ls), self_name, self_loc))
          return 1;
      }
      return 0;
    }

    if (sexp_inline_letp(ctx, lam, x)) {
      /* boxing mutated params allocates */
      if (sexp_pairp(sexp_lambda_sv(op)))
        return 1;
      for (sexp ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
        if (sexp_expr_may_allocp(ctx, lam, sexp_car(ls), self_name, self_loc))
          return 1;
      }
      return sexp_expr_may_allocp(ctx, lam, sexp_lambda_body(op), self_name, self_loc);
    }

    if (self_name && sexp_refp(op) && (self_name == sexp_ref_name(op))
        && lam && (self_loc == sexp_ref_loc(op))) {
      for (sexp ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
        if (sexp_expr_may_allocp(ctx, lam, sexp_car(ls), self_name, self_loc))
          return 1;
      }
      return 0;
    }

    return 1;
  }

  default:
    return 0;
  }
}

/************************* Driver *************************************/

/* what a pass over the lambdas infers */
enum { INFER_TYPES, INFER_FLAGS };

static void infer_param_types_expr (sexp ctx, sexp x, sexp enclosing_lam, sexp top, int pass);

static void infer_param_types_lambda (sexp ctx, sexp lam, sexp enclosing_lam,
                                      sexp top, sexp name, sexp loc, int pass) {
  if (!name && sexp_symbolp(sexp_lambda_name(lam)))
    name = sexp_lambda_name(lam);
  if (name && !loc && sexp_symbolp(name))
    loc = sexp_env_cell(ctx, sexp_context_env(ctx), name, 0);
  if (pass == INFER_TYPES) {
    analyze_lambda_param_types(ctx, lam, enclosing_lam, top, name, loc);
    infer_param_types_expr(ctx, sexp_lambda_body(lam), lam, top, pass);
    return;
  }
  /* after the param types, which sexp_proven_fixnump depends on */
  sexp_sint_t flags = sexp_fixnump(sexp_lambda_flags(lam)) ? sexp_unbox_fixnum(sexp_lambda_flags(lam)) : 0;
  if (!sexp_expr_may_allocp(ctx, lam, sexp_lambda_body(lam), name, loc))
    flags |= SEXP_LAMBDA_NON_ALLOC;
  else
    /* a leaf's frame isn't a GC root, so it can't hold values across */
    /* allocation, as in the slow paths of arithmetic on non-fixnums */
    flags &= ~SEXP_LAMBDA_LEAF;
  sexp_lambda_flags(lam) = sexp_make_fixnum(flags);
  infer_param_types_expr(ctx, sexp_lambda_body(lam), lam, top, pass);
}

/* Visit lambdas in code generation order: a lambda's parameter types */
/* are inferred from the types already known for its callers. */
static void infer_param_types_expr (sexp ctx, sexp x, sexp enclosing_lam, sexp top, int pass) {
  sexp ls;
  if (!x || !sexp_pointerp(x)) return;
  switch (sexp_pointer_tag(x)) {
  case SEXP_LAMBDA:
    infer_param_types_lambda(ctx, x, enclosing_lam, top, NULL, NULL, pass);
    break;
  case SEXP_SET:
    if (sexp_lambdap(sexp_set_value(x)) && sexp_refp(sexp_set_var(x)))
      infer_param_types_lambda(ctx, sexp_set_value(x), enclosing_lam, top,
                               sexp_ref_name(sexp_set_var(x)),
                               sexp_ref_loc(sexp_set_var(x)), pass);
    else
      infer_param_types_expr(ctx, sexp_set_value(x), enclosing_lam, top, pass);
    break;
  case SEXP_PAIR:
    if (sexp_opcodep(sexp_car(x))) {
      for (ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls))
        infer_param_types_expr(ctx, sexp_car(ls), enclosing_lam, top, pass);
    } else {
      /* general application: arguments first, then the operator */
      for (ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls))
        infer_param_types_expr(ctx, sexp_car(ls), enclosing_lam, top, pass);
      infer_param_types_expr(ctx, sexp_car(x), enclosing_lam, top, pass);
    }
    break;
  case SEXP_CND:
    infer_param_types_expr(ctx, sexp_cnd_test(x), enclosing_lam, top, pass);
    infer_param_types_expr(ctx, sexp_cnd_pass(x), enclosing_lam, top, pass);
    infer_param_types_expr(ctx, sexp_cnd_fail(x), enclosing_lam, top, pass);
    break;
  case SEXP_SEQ:
    for (ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls))
      infer_param_types_expr(ctx, sexp_car(ls), enclosing_lam, top, pass);
    break;
  case SEXP_SYNCLO:
    infer_param_types_expr(ctx, sexp_synclo_expr(x), enclosing_lam, top, pass);
    break;
  }
}

/* Lambdas are visited in code generation order, so their callers' */
/* types are known, but not those of the lambdas they contain.  When */
/* that defeated a flonum param assumption, the types are inferred */
/* again from those found, which are sound to reason from. */
static void infer_param_types (sexp ctx, sexp ast) {
  flonum_assumption_failed = 0;
  infer_param_types_expr(ctx, ast, NULL, ast, INFER_TYPES);
  if (flonum_assumption_failed)
    infer_param_types_expr(ctx, ast, NULL, ast, INFER_TYPES);
  infer_unboxed(ctx, ast);
  infer_param_types_expr(ctx, ast, NULL, ast, INFER_FLAGS);
}

static sexp infer (sexp ctx, sexp ast) {
  analyze_escapes(ctx, ast);
  infer_param_types(ctx, ast);
  return ast;
}

sexp sexp_infer (sexp ctx, sexp self, sexp_sint_t n, sexp ast) {
  return infer(ctx, ast);
}

#endif
