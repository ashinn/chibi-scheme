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
    sexp old_lam = find_lambda_for_ref(var, bindings, n);
    if (old_lam && sexp_lambdap(old_lam)) {
      sexp_lambda_set_escapes(old_lam);
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

static int is_param_fixnum (sexp lam, sexp var_name) {
  if (!lam || !sexp_lambdap(lam) || !var_name || !sexp_symbolp(var_name))
    return 0;
  sexp types = sexp_lambda_param_types(lam);
  if (!sexp_pairp(types))
    return 0;
  sexp params = sexp_lambda_params(lam);
  for (; sexp_pairp(params) && sexp_pairp(types);
       params = sexp_cdr(params), types = sexp_cdr(types)) {
    if (sexp_car(params) == var_name) {
      sexp t = sexp_car(types);
      return sexp_fixnump(t) && sexp_unbox_fixnum(t) == SEXP_FIXNUM;
    }
  }
  return 0;
}

static int is_ref_fixnum (sexp x) {
  if (!x || !sexp_pointerp(x) || sexp_pointer_tag(x) != SEXP_REF)
    return 0;
  sexp loc = sexp_ref_loc(x);
  if (sexp_lambdap(loc)) {
    return is_param_fixnum(loc, sexp_ref_name(x));
  }
  return 0;
}

static int is_ref_to_var (sexp x, sexp lam, sexp sym) {
  return (x && sexp_pointerp(x) && sexp_pointer_tag(x) == SEXP_REF &&
          sexp_ref_loc(x) == lam && sexp_ref_name(x) == sym);
}

int sexp_proven_fixnump (sexp ctx, sexp lam, sexp x) {
  sexp_sint_t c;
  if (!x) return 0;
  if (sexp_fixnum_literalp(x, &c)) return 1;
  if (!sexp_pointerp(x)) return 0;

  int tag = sexp_pointer_tag(x);
  if (tag == SEXP_REF) {
    return is_ref_fixnum(x);
  }

  if (tag == SEXP_PAIR) {
    sexp op = sexp_car(x);
    if (!sexp_opcodep(op)) return 0;
    sexp_sint_t code = sexp_opcode_code(op);
    switch (code) {
    case SEXP_OP_VECTOR_LENGTH:
    case SEXP_OP_BYTES_LENGTH:
    case SEXP_OP_STRING_LENGTH:
    case SEXP_OP_BYTES_REF:
    case SEXP_OP_CHAR2INT:
      return 1;
    case SEXP_OP_REMAINDER: {
      sexp ls = sexp_cdr(x);
      if (sexp_pairp(ls) && sexp_pairp(sexp_cdr(ls))) {
        sexp arg0 = sexp_car(ls);
        sexp arg1 = sexp_cadr(ls);
        if (sexp_proven_fixnump(ctx, lam, arg0) && sexp_fixnum_literalp(arg1, &c) && c > 0) return 1;
      }
      return 0;
    }
    case SEXP_OP_ADD: {
      sexp ls = sexp_cdr(x);
      if (sexp_nullp(ls)) return 1;
      if (sexp_nullp(sexp_cdr(ls))) return sexp_proven_fixnump(ctx, lam, sexp_car(ls));
      if (sexp_pairp(sexp_cdr(ls))) {
        sexp arg0 = sexp_car(ls);
        sexp arg1 = sexp_cadr(ls);
        sexp_sint_t c0, c1;
        if (sexp_fixnum_literalp(arg0, &c0) && sexp_fixnum_literalp(arg1, &c1)) {
          sexp_sint_t sum = c0 + c1;
          if (sum >= SEXP_MIN_FIXNUM && sum <= SEXP_MAX_FIXNUM) return 1;
        }
        if (sexp_proven_fixnump(ctx, lam, arg0) &&
            sexp_fixnum_literalp(arg1, &c) && c >= -1 && c <= 1)
          return 1;
        if (sexp_proven_fixnump(ctx, lam, arg1) &&
            sexp_fixnum_literalp(arg0, &c) && c >= -1 && c <= 1)
          return 1;
      }
      return 0;
    }
    case SEXP_OP_SUB: {
      sexp ls = sexp_cdr(x);
      if (sexp_pairp(ls) && sexp_nullp(sexp_cdr(ls))) {
        return sexp_fixnum_literalp(sexp_car(ls), &c);
      }
      if (sexp_pairp(ls) && sexp_pairp(sexp_cdr(ls))) {
        sexp arg0 = sexp_car(ls);
        sexp arg1 = sexp_cadr(ls);
        sexp_sint_t c0, c1;
        if (sexp_fixnum_literalp(arg0, &c0) && sexp_fixnum_literalp(arg1, &c1)) {
          sexp_sint_t diff = c0 - c1;
          if (diff >= SEXP_MIN_FIXNUM && diff <= SEXP_MAX_FIXNUM) return 1;
        }
        if (sexp_proven_fixnump(ctx, lam, arg0) &&
            sexp_fixnum_literalp(arg1, &c) && c >= -1 && c <= 1)
          return 1;
      }
      return 0;
    }
    default:
      return 0;
    }
  }

  return 0;
}

static int is_known_fixnum_bound (sexp ctx, sexp lam, sexp x) {
  sexp_sint_t c;
  if (!x) return 0;
  if (sexp_fixnum_literalp(x, &c)) return 1;
  if (is_ref_fixnum(x)) return 1;
  if (sexp_pointerp(x) && sexp_pointer_tag(x) == SEXP_PAIR) {
    sexp op = sexp_car(x);
    if (sexp_opcodep(op)) {
      sexp_sint_t code = sexp_opcode_code(op);
      if (code == SEXP_OP_VECTOR_LENGTH ||
          code == SEXP_OP_BYTES_LENGTH ||
          code == SEXP_OP_STRING_LENGTH)
        return 1;
    }
  }
  return sexp_proven_fixnump(ctx, lam, x);
}

static void analyze_cnd_bound (sexp ctx, sexp lam, sexp test, sexp p_sym, int *pass_b, int *fail_b) {
  while (sexp_pairp(test) && sexp_refp(sexp_car(test)) &&
         sexp_ref_name(sexp_car(test)) == sexp_intern(ctx, "not", 3) &&
         !sexp_lambdap(sexp_ref_loc(sexp_car(test))) &&
         sexp_pairp(sexp_cdr(test)) && sexp_nullp(sexp_cddr(test))) {
    test = sexp_cadr(test);
    int tmp = *pass_b; *pass_b = *fail_b; *fail_b = tmp;
  }
  if (!sexp_pairp(test) || !sexp_opcodep(sexp_car(test))) return;
  sexp op = sexp_car(test);
  sexp_sint_t code = sexp_opcode_code(op);
  sexp arg0 = sexp_pairp(sexp_cdr(test)) ? sexp_cadr(test) : SEXP_NULL;
  sexp arg1 = (sexp_pairp(sexp_cdr(test)) && sexp_pairp(sexp_cddr(test))) ? sexp_caddr(test) : SEXP_NULL;
  int inv = sexp_opcode_inverse(op);

  if (is_ref_to_var(arg0, lam, p_sym) && is_known_fixnum_bound(ctx, lam, arg1)) {
    if (code == SEXP_OP_LT || code == SEXP_OP_LE) {
      if (!inv) *pass_b = 1;
      else *fail_b = 1;
    } else if (code == SEXP_OP_EQN) {
      *fail_b = 1;
    }
  } else if (is_ref_to_var(arg1, lam, p_sym) && is_known_fixnum_bound(ctx, lam, arg0)) {
    if (code == SEXP_OP_LT || code == SEXP_OP_LE) {
      if (!inv) *fail_b = 1;
      else *pass_b = 1;
    } else if (code == SEXP_OP_EQN) {
      *fail_b = 1;
    }
  } else if (code == SEXP_OP_FIXNUMP && is_ref_to_var(arg0, lam, p_sym)) {
    *pass_b = 1;
  }
}

static int check_recursive_step (sexp ctx, sexp lam, sexp app, sexp p_sym, int k) {
  sexp args = sexp_cdr(app);
  for (int i = 0; i < k && sexp_pairp(args); i++)
    args = sexp_cdr(args);
  if (!sexp_pairp(args)) return 0;
  sexp arg_k = sexp_car(args);

  if (is_ref_to_var(arg_k, lam, p_sym)) return 1; /* Invariant */

  if (sexp_pairp(arg_k) && sexp_opcodep(sexp_car(arg_k))) {
    sexp op = sexp_car(arg_k);
    sexp_sint_t code = sexp_opcode_code(op);
    sexp ls = sexp_cdr(arg_k);
    if (sexp_pairp(ls) && sexp_pairp(sexp_cdr(ls))) {
      sexp a0 = sexp_car(ls);
      sexp a1 = sexp_cadr(ls);
      sexp_sint_t c;
      if (code == SEXP_OP_ADD) {
        if (is_ref_to_var(a0, lam, p_sym) && sexp_fixnum_literalp(a1, &c) && c >= -1 && c <= 1)
          return 2; /* Stepped */
        if (is_ref_to_var(a1, lam, p_sym) && sexp_fixnum_literalp(a0, &c) && c >= -1 && c <= 1)
          return 2; /* Stepped */
      } else if (code == SEXP_OP_SUB) {
        if (is_ref_to_var(a0, lam, p_sym) && sexp_fixnum_literalp(a1, &c) && c >= -1 && c <= 1)
          return 2; /* Stepped */
      }
    }
  }
  return 0;
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

static int infer_expr_type (sexp ctx, sexp caller_lam, sexp x) {
  if (!x) return SEXP_OBJECT;
  if (sexp_fixnump(x)) return SEXP_FIXNUM;
  if (sexp_booleanp(x)) return SEXP_BOOLEAN;
  if (sexp_charp(x)) return SEXP_CHAR;
  if (sexp_nullp(x)) return SEXP_OBJECT;
  if (!sexp_pointerp(x)) return SEXP_OBJECT;

  int tag = sexp_pointer_tag(x);
  switch (tag) {
  case SEXP_LIT: {
    sexp val = sexp_lit_value(x);
    if (sexp_fixnump(val)) return SEXP_FIXNUM;
    if (sexp_flonump(val)) return SEXP_FLONUM;
    if (sexp_stringp(val)) return SEXP_STRING;
    if (sexp_symbolp(val)) return SEXP_SYMBOL;
    if (sexp_vectorp(val)) return SEXP_VECTOR;
    if (sexp_bytesp(val)) return SEXP_BYTES;
    if (sexp_pairp(val)) return SEXP_PAIR;
    if (sexp_booleanp(val)) return SEXP_BOOLEAN;
    if (sexp_charp(val)) return SEXP_CHAR;
    if (sexp_nullp(val)) return SEXP_OBJECT;
    return SEXP_OBJECT;
  }
  case SEXP_PAIR: {
    sexp op = sexp_car(x);
    if (sexp_opcodep(op)) {
      sexp rt = sexp_opcode_return_type(op);
      if (sexp_fixnump(rt)) {
        sexp_sint_t code = sexp_unbox_fixnum(rt);
        if (code == SEXP_FIXNUM || code == SEXP_BOOLEAN || code == SEXP_CHAR ||
            code == SEXP_STRING || code == SEXP_BYTES || code == SEXP_VECTOR)
          return (int)code;
      }
      if (sexp_proven_fixnump(ctx, caller_lam, x))
        return SEXP_FIXNUM;
    }
    return SEXP_OBJECT;
  }
  case SEXP_CND: {
    int t_pass = infer_expr_type(ctx, caller_lam, sexp_cnd_pass(x));
    int t_fail = infer_expr_type(ctx, caller_lam, sexp_cnd_fail(x));
    if (t_pass == t_fail && t_pass != SEXP_OBJECT)
      return t_pass;
    return SEXP_OBJECT;
  }
  case SEXP_SEQ: {
    sexp ls = sexp_seq_ls(x);
    for (; sexp_pairp(ls) && sexp_pairp(sexp_cdr(ls)); ls = sexp_cdr(ls));
    if (sexp_pairp(ls))
      return infer_expr_type(ctx, caller_lam, sexp_car(ls));
    return SEXP_OBJECT;
  }
  case SEXP_SYNCLO:
    return infer_expr_type(ctx, caller_lam, sexp_synclo_expr(x));
  case SEXP_REF: {
    if (caller_lam && sexp_lambdap(caller_lam) && sexp_ref_loc(x) == caller_lam) {
      sexp p_types = sexp_lambda_param_types(caller_lam);
      sexp params = sexp_lambda_params(caller_lam);
      while (sexp_pairp(p_types) && sexp_pairp(params)) {
        if (sexp_car(params) == sexp_ref_name(x)) {
          if (sexp_fixnump(sexp_car(p_types)))
            return (int)sexp_unbox_fixnum(sexp_car(p_types));
          break;
        }
        p_types = sexp_cdr(p_types);
        params = sexp_cdr(params);
      }
    }
    return SEXP_OBJECT;
  }
  default:
    return SEXP_OBJECT;
  }
}

static void process_external_call (sexp ctx, sexp caller_lam, sexp app, int *types, int num_params) {
  sexp args = sexp_cdr(app);
  for (int k = 0; k < num_params; k++) {
    if (!sexp_pairp(args)) {
      types[k] = SEXP_OBJECT;
      continue;
    }
    sexp arg_k = sexp_car(args);
    args = sexp_cdr(args);

    int t = infer_expr_type(ctx, caller_lam, arg_k);
    if (types[k] == -1) {
      types[k] = t;
    } else if (types[k] != t) {
      types[k] = SEXP_OBJECT;
    }
  }
}

static void scan_external_calls (sexp ctx, sexp caller_lam, sexp target_lam,
                                sexp name, sexp loc, sexp x, int *types, int num_params) {
  if (!x || !sexp_pointerp(x) || sexp_litp(x)) return;
  int tag = sexp_pointer_tag(x);
  if (tag == SEXP_LAMBDA) {
    if (x == target_lam) return;
    scan_external_calls(ctx, x, target_lam, name, loc, sexp_lambda_body(x), types, num_params);
    return;
  }
  if (tag == SEXP_SET) {
    scan_external_calls(ctx, caller_lam, target_lam, name, loc, sexp_set_value(x), types, num_params);
    return;
  }
  if (tag == SEXP_SEQ) {
    for (sexp ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls))
      scan_external_calls(ctx, caller_lam, target_lam, name, loc, sexp_car(ls), types, num_params);
    return;
  }
  if (tag == SEXP_CND) {
    scan_external_calls(ctx, caller_lam, target_lam, name, loc, sexp_cnd_test(x), types, num_params);
    scan_external_calls(ctx, caller_lam, target_lam, name, loc, sexp_cnd_pass(x), types, num_params);
    scan_external_calls(ctx, caller_lam, target_lam, name, loc, sexp_cnd_fail(x), types, num_params);
    return;
  }
  if (tag == SEXP_SYNCLO) {
    scan_external_calls(ctx, caller_lam, target_lam, name, loc, sexp_synclo_expr(x), types, num_params);
    return;
  }
  if (tag == SEXP_PAIR) {
    sexp op = sexp_car(x);
    if (is_call_to_target(op, target_lam, name, loc)) {
      process_external_call(ctx, caller_lam, x, types, num_params);
    } else {
      scan_external_calls(ctx, caller_lam, target_lam, name, loc, op, types, num_params);
    }
    for (sexp ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
      scan_external_calls(ctx, caller_lam, target_lam, name, loc, sexp_car(ls), types, num_params);
    }
  }
}

static void scan_recursive_calls (sexp ctx, sexp target_lam, sexp name, sexp loc,
                                 sexp x, int *types, int num_params, uint64_t bounded_mask) {
  if (!x || !sexp_pointerp(x) || sexp_litp(x)) return;
  int tag = sexp_pointer_tag(x);
  if (tag == SEXP_LAMBDA) {
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_lambda_body(x), types, num_params, bounded_mask);
    return;
  }
  if (tag == SEXP_SET) {
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_set_value(x), types, num_params, bounded_mask);
    return;
  }
  if (tag == SEXP_SEQ) {
    for (sexp ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls))
      scan_recursive_calls(ctx, target_lam, name, loc, sexp_car(ls), types, num_params, bounded_mask);
    return;
  }
  if (tag == SEXP_CND) {
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_cnd_test(x), types, num_params, bounded_mask);

    uint64_t pass_mask = bounded_mask;
    uint64_t fail_mask = bounded_mask;

    sexp params = sexp_lambda_params(target_lam);
    for (int k = 0; k < num_params && sexp_pairp(params) && k < 64; k++, params = sexp_cdr(params)) {
      sexp p_sym = sexp_car(params);
      int p_pass = (bounded_mask & (1ULL << k)) != 0;
      int p_fail = p_pass;
      analyze_cnd_bound(ctx, target_lam, sexp_cnd_test(x), p_sym, &p_pass, &p_fail);
      if (p_pass) pass_mask |= (1ULL << k);
      if (p_fail) fail_mask |= (1ULL << k);
    }

    scan_recursive_calls(ctx, target_lam, name, loc, sexp_cnd_pass(x), types, num_params, pass_mask);
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_cnd_fail(x), types, num_params, fail_mask);
    return;
  }
  if (tag == SEXP_SYNCLO) {
    scan_recursive_calls(ctx, target_lam, name, loc, sexp_synclo_expr(x), types, num_params, bounded_mask);
    return;
  }
  if (tag == SEXP_PAIR) {
    sexp op = sexp_car(x);
    if (is_call_to_target(op, target_lam, name, loc)) {
      sexp args = sexp_cdr(x);
      sexp params = sexp_lambda_params(target_lam);
      for (int k = 0; k < num_params; k++) {
        if (!sexp_pairp(args)) {
          types[k] = SEXP_OBJECT;
          continue;
        }
        sexp arg_k = sexp_car(args);
        args = sexp_cdr(args);
        sexp p_sym = sexp_car(params);
        params = sexp_cdr(params);

        if (types[k] == SEXP_FIXNUM) {
          int is_bounded = (k < 64) ? ((bounded_mask & (1ULL << k)) != 0) : 0;
          int step = check_recursive_step(ctx, target_lam, x, p_sym, k);
          if (step == 1) {
            /* Invariant */
          } else if (step == 2 && is_bounded) {
            /* Stepped and bounded fixnum */
          } else if (sexp_proven_fixnump(ctx, target_lam, arg_k)) {
            /* Independently proven fixnum */
          } else {
            types[k] = SEXP_OBJECT;
          }
        } else if (types[k] != SEXP_OBJECT && types[k] != -1) {
          int step = check_recursive_step(ctx, target_lam, x, p_sym, k);
          if (step == 1) {
            /* Invariant */
          } else {
            int t = infer_expr_type(ctx, target_lam, arg_k);
            if (t != types[k]) {
              types[k] = SEXP_OBJECT;
            }
          }
        }
      }
    } else {
      scan_recursive_calls(ctx, target_lam, name, loc, op, types, num_params, bounded_mask);
    }
    for (sexp ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
      scan_recursive_calls(ctx, target_lam, name, loc, sexp_car(ls), types, num_params, bounded_mask);
    }
  }
}

static void analyze_lambda_param_types (sexp ctx, sexp lam, sexp prev_lam, sexp top_expr, sexp name, sexp loc) {
  if (!lam || !sexp_lambdap(lam) || sexp_lambda_escapes_p(lam)) {
    if (lam && sexp_lambdap(lam))
      sexp_lambda_param_types(lam) = SEXP_NULL;
    return;
  }
  sexp params = sexp_lambda_params(lam);
  if (!sexp_listp(ctx, params) || sexp_nullp(params)) {
    sexp_lambda_param_types(lam) = SEXP_NULL;
    return;
  }

  int num_params = (int)sexp_unbox_fixnum(sexp_length(ctx, params));
  int stack_types[32];
  int *types = (num_params <= 32) ? stack_types : (int *)malloc(num_params * sizeof(int));
  if (!types) return;

  for (int k = 0; k < num_params; k++)
    types[k] = -1;

  if (prev_lam && sexp_lambdap(prev_lam)) {
    scan_external_calls(ctx, prev_lam, lam, name, loc, sexp_lambda_body(prev_lam), types, num_params);
  }
  if (top_expr) {
    scan_external_calls(ctx, NULL, lam, name, loc, top_expr, types, num_params);
  }

  scan_recursive_calls(ctx, lam, name, loc, sexp_lambda_body(lam), types, num_params, 0ULL);

  sexp p = params;
  for (int k = 0; k < num_params; k++, p = sexp_cdr(p)) {
    sexp p_sym = sexp_car(p);
    if (!sexp_symbolp(p_sym) || sexp_truep(sexp_memq(ctx, p_sym, sexp_lambda_sv(lam)))) {
      types[k] = SEXP_OBJECT;
    }
    if (types[k] == -1) {
      types[k] = SEXP_OBJECT;
    }
  }

  sexp_gc_var1(types_list);
  sexp_gc_preserve1(ctx, types_list);
  types_list = SEXP_NULL;
  for (int k = num_params - 1; k >= 0; k--) {
    types_list = sexp_cons(ctx, sexp_make_fixnum(types[k]), types_list);
  }
  sexp_lambda_param_types(lam) = types_list;
  sexp_gc_release1(ctx);

  if (types != stack_types) free(types);
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
    return 0;

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

static void infer_param_types_expr (sexp ctx, sexp x, sexp enclosing_lam, sexp top);

static void infer_param_types_lambda (sexp ctx, sexp lam, sexp enclosing_lam,
                                      sexp top, sexp name, sexp loc) {
  if (!name && sexp_symbolp(sexp_lambda_name(lam)))
    name = sexp_lambda_name(lam);
  if (name && !loc && sexp_symbolp(name))
    loc = sexp_env_cell(ctx, sexp_context_env(ctx), name, 0);
  analyze_lambda_param_types(ctx, lam, enclosing_lam, top, name, loc);
  /* after the param types, which sexp_proven_fixnump depends on */
  if (!sexp_expr_may_allocp(ctx, lam, sexp_lambda_body(lam), name, loc))
    sexp_lambda_flags(lam) = sexp_make_fixnum((sexp_fixnump(sexp_lambda_flags(lam)) ? sexp_unbox_fixnum(sexp_lambda_flags(lam)) : 0) | SEXP_LAMBDA_NON_ALLOC);
  infer_param_types_expr(ctx, sexp_lambda_body(lam), lam, top);
}

/* Visit lambdas in code generation order: a lambda's parameter types */
/* are inferred from the types already known for its callers. */
static void infer_param_types_expr (sexp ctx, sexp x, sexp enclosing_lam, sexp top) {
  sexp ls;
  if (!x || !sexp_pointerp(x)) return;
  switch (sexp_pointer_tag(x)) {
  case SEXP_LAMBDA:
    infer_param_types_lambda(ctx, x, enclosing_lam, top, NULL, NULL);
    break;
  case SEXP_SET:
    if (sexp_lambdap(sexp_set_value(x)) && sexp_refp(sexp_set_var(x)))
      infer_param_types_lambda(ctx, sexp_set_value(x), enclosing_lam, top,
                               sexp_ref_name(sexp_set_var(x)),
                               sexp_ref_loc(sexp_set_var(x)));
    else
      infer_param_types_expr(ctx, sexp_set_value(x), enclosing_lam, top);
    break;
  case SEXP_PAIR:
    if (sexp_opcodep(sexp_car(x))) {
      for (ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls))
        infer_param_types_expr(ctx, sexp_car(ls), enclosing_lam, top);
    } else {
      /* general application: arguments first, then the operator */
      for (ls = sexp_cdr(x); sexp_pairp(ls); ls = sexp_cdr(ls))
        infer_param_types_expr(ctx, sexp_car(ls), enclosing_lam, top);
      infer_param_types_expr(ctx, sexp_car(x), enclosing_lam, top);
    }
    break;
  case SEXP_CND:
    infer_param_types_expr(ctx, sexp_cnd_test(x), enclosing_lam, top);
    infer_param_types_expr(ctx, sexp_cnd_pass(x), enclosing_lam, top);
    infer_param_types_expr(ctx, sexp_cnd_fail(x), enclosing_lam, top);
    break;
  case SEXP_SEQ:
    for (ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls))
      infer_param_types_expr(ctx, sexp_car(ls), enclosing_lam, top);
    break;
  case SEXP_SYNCLO:
    infer_param_types_expr(ctx, sexp_synclo_expr(x), enclosing_lam, top);
    break;
  }
}

static void infer_param_types (sexp ctx, sexp ast) {
  infer_param_types_expr(ctx, ast, NULL, ast);
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
