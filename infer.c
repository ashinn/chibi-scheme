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

static sexp infer (sexp ctx, sexp ast) {
  analyze_escapes(ctx, ast);
  return ast;
}

sexp sexp_infer (sexp ctx, sexp self, sexp_sint_t n, sexp ast) {
  return infer(ctx, ast);
}

#endif
