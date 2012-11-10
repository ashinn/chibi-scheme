/*  vm.c -- stack-based virtual machine backend               */
/*  Copyright (c) 2009-2012 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#if SEXP_USE_NATIVE_X86
#include "opt/x86.c"
#else
/* ... the rest of this file ... */

#include "chibi/eval.h"

#if SEXP_USE_DEBUG_VM > 1
static void sexp_print_stack (sexp ctx, sexp *stack, int top, int fp, sexp out) {
  int i;
  if (! sexp_oportp(out)) out = sexp_current_error_port(ctx);
  for (i=0; i<top; i++) {
    sexp_write_char(ctx, ((i==fp) ? '*' : ' '), out);
    if (i < 10) sexp_write_char(ctx, '0', out);
    sexp_write(ctx, sexp_make_fixnum(i), out);
    sexp_write_string(ctx, ": ", out);
    sexp_write(ctx, stack[i], out);
    sexp_newline(ctx, out);
  }
}
#else
#define sexp_print_stack(ctx, stack, top, fp, out)
#endif

#if SEXP_USE_FULL_SOURCE_INFO
static sexp sexp_lookup_source_info (sexp src, int ip) {
  int i;
  if (src && sexp_procedurep(src))
    src = sexp_procedure_source(src);
  if (src && sexp_vectorp(src) && sexp_vector_length(src) > 0) {
    for (i=1; i<sexp_vector_length(src); i++)
      if (sexp_unbox_fixnum(sexp_car(sexp_vector_ref(src, sexp_make_fixnum(i)))) > ip)
        return sexp_cdr(sexp_vector_ref(src, sexp_make_fixnum(i-1)));
    return sexp_cdr(sexp_vector_ref(src, sexp_make_fixnum(sexp_vector_length(src)-1)));
  }
  return SEXP_FALSE;
}
#endif

void sexp_stack_trace (sexp ctx, sexp out) {
  int i, fp=sexp_context_last_fp(ctx);
  sexp self, bc, src, *stack=sexp_stack_data(sexp_context_stack(ctx));
  if (! sexp_oportp(out))
    out = sexp_current_error_port(ctx);
  for (i=fp; i>4; i=sexp_unbox_fixnum(stack[i+3])) {
    self = stack[i+2];
    if (self && sexp_procedurep(self)) {
      sexp_write_string(ctx, "  called from ", out);
      bc = sexp_procedure_code(self);
      if (sexp_symbolp(sexp_bytecode_name(bc)))
        sexp_write(ctx, sexp_bytecode_name(bc), out);
      else
        sexp_write_string(ctx, "<anonymous>", out);
      src = sexp_bytecode_source(bc);
#if SEXP_USE_FULL_SOURCE_INFO
      if (src && sexp_vectorp(src))
        src = sexp_lookup_source_info(src, sexp_unbox_fixnum(stack[i+3]));
#endif
      if (src && sexp_pairp(src)) {
        if (sexp_fixnump(sexp_cdr(src)) && (sexp_cdr(src) >= SEXP_ZERO)) {
          sexp_write_string(ctx, " on line ", out);
          sexp_write(ctx, sexp_cdr(src), out);
        }
        if (sexp_stringp(sexp_car(src))) {
          sexp_write_string(ctx, " of file ", out);
          sexp_write_string(ctx, sexp_string_data(sexp_car(src)), out);
        }
      }
      sexp_write_char(ctx, '\n', out);
    }
  }
}

sexp sexp_stack_trace_op (sexp ctx, sexp self, sexp_sint_t n, sexp out) {
  sexp_stack_trace(ctx, out);
  return SEXP_VOID;
}

/************************* code generation ****************************/

static void sexp_inc_context_pos(sexp ctx, sexp_sint_t off) {
  sexp_context_pos(ctx) = sexp_fx_add(sexp_context_pos(ctx), sexp_make_fixnum(off));
}

static void sexp_inc_context_depth(sexp ctx, sexp_sint_t off) {
  sexp_context_depth(ctx) = sexp_fx_add(sexp_context_depth(ctx), sexp_make_fixnum(off));
  if (sexp_unbox_fixnum(sexp_context_depth(ctx))
      > sexp_unbox_fixnum(sexp_context_max_depth(ctx)))
    sexp_context_max_depth(ctx) = sexp_context_depth(ctx);
}

static void bytecode_preserve (sexp ctx, sexp obj) {
  sexp ls = sexp_bytecode_literals(sexp_context_bc(ctx));
  if (sexp_pointerp(obj) && !sexp_symbolp(obj)
      && sexp_not(sexp_memq(ctx, obj, ls)))
    sexp_push(ctx, sexp_bytecode_literals(sexp_context_bc(ctx)), obj);
}

static void sexp_emit_word (sexp ctx, sexp_uint_t val)  {
  unsigned char *data;
  sexp_expand_bcode(ctx, sizeof(sexp));
  if (sexp_exceptionp(sexp_context_exception(ctx)))
    return;
  data = sexp_bytecode_data(sexp_context_bc(ctx));
  sexp_context_align_pos(ctx);
  *((sexp_uint_t*)(&(data[sexp_unbox_fixnum(sexp_context_pos(ctx))]))) = val;
  sexp_inc_context_pos(ctx, sizeof(sexp));
}

static void sexp_emit_push (sexp ctx, sexp obj) {
  sexp_emit(ctx, SEXP_OP_PUSH);
  sexp_emit_word(ctx, (sexp_uint_t)obj);
  sexp_inc_context_depth(ctx, 1);
  bytecode_preserve(ctx, obj);
}

void sexp_emit_return (sexp ctx) {
  sexp_emit(ctx, SEXP_OP_RET);
}

static void sexp_push_source (sexp ctx, sexp source) {
#if SEXP_USE_FULL_SOURCE_INFO
  sexp src, bc = sexp_context_bc(ctx);
  sexp_gc_var1(tmp);
  if (source && sexp_pairp(source)) {
    src = sexp_bytecode_source(bc);
    if (!src) src = sexp_bytecode_source(bc) = SEXP_NULL;
    if (!sexp_pairp(src)
        || sexp_unbox_fixnum(sexp_context_pos(ctx)) > sexp_unbox_fixnum(sexp_caar(src))) {
      sexp_gc_preserve1(ctx, tmp);
      tmp = sexp_cons(ctx, sexp_context_pos(ctx), source);
      if (sexp_pairp(tmp)) {
        tmp = sexp_cons(ctx, tmp, src);
        if (sexp_pairp(tmp)) sexp_bytecode_source(bc) = tmp;
      }
      sexp_gc_release1(ctx);
    }
  }
#endif
}

static sexp_sint_t sexp_context_make_label (sexp ctx) {
  sexp_sint_t label;
  sexp_context_align_pos(ctx);
  label = sexp_unbox_fixnum(sexp_context_pos(ctx));
  sexp_inc_context_pos(ctx, sizeof(sexp_uint_t));
  return label;
}

static void sexp_context_patch_label (sexp ctx, sexp_sint_t label) {
  sexp bc = sexp_context_bc(ctx);
  unsigned char *data = sexp_bytecode_data(bc)+label;
  if (!sexp_exceptionp(sexp_context_exception(ctx)))
    *((sexp_sint_t*)data) = sexp_unbox_fixnum(sexp_context_pos(ctx))-label;
}

static void generate_lit (sexp ctx, sexp value) {
  sexp_emit_push(ctx, value);
}

static void generate_drop_prev (sexp ctx, sexp prev) {
  if ((sexp_pairp(prev) && sexp_opcodep(sexp_car(prev))
       && ((sexp_opcode_return_type(sexp_car(prev)) == SEXP_VOID
            && sexp_opcode_class(sexp_car(prev)) != SEXP_OPC_FOREIGN)
           || (sexp_opcode_code(sexp_car(prev)) == SEXP_OP_PUSH)))
      || sexp_setp(prev) || sexp_litp(prev) || prev == SEXP_VOID)
    sexp_inc_context_pos(ctx, -(1 + sizeof(sexp)));
  else
    sexp_emit(ctx, SEXP_OP_DROP);
}

static void generate_seq (sexp ctx, sexp name, sexp loc, sexp lam, sexp app) {
  sexp head=app, tail=sexp_cdr(app);
  sexp_uint_t tailp = sexp_context_tailp(ctx);
  sexp_push_source(ctx, sexp_pair_source(app));
  sexp_context_tailp(ctx) = 0;
  for ( ; sexp_pairp(tail); head=tail, tail=sexp_cdr(tail))
    if (sexp_pointerp(sexp_car(head)) && (! sexp_litp(sexp_car(head)))) {
      sexp_generate(ctx, name, loc, lam, sexp_car(head));
      generate_drop_prev(ctx, sexp_car(head));
      sexp_inc_context_depth(ctx, -1);
    }
  sexp_context_tailp(ctx) = tailp;
  sexp_generate(ctx, name, loc, lam, sexp_car(head));
}

static void generate_cnd (sexp ctx, sexp name, sexp loc, sexp lam, sexp cnd) {
  sexp_sint_t label1, label2, tailp=sexp_context_tailp(ctx);
  sexp_push_source(ctx, sexp_cnd_source(cnd));
  sexp_context_tailp(ctx) = 0;
  sexp_generate(ctx, name, loc, lam, sexp_cnd_test(cnd));
  sexp_context_tailp(ctx) = tailp;
  sexp_emit(ctx, SEXP_OP_JUMP_UNLESS);
  sexp_inc_context_depth(ctx, -1);
  label1 = sexp_context_make_label(ctx);
  sexp_generate(ctx, name, loc, lam, sexp_cnd_pass(cnd));
  sexp_context_tailp(ctx) = tailp;
  sexp_emit(ctx, SEXP_OP_JUMP);
  sexp_inc_context_depth(ctx, -1);
  label2 = sexp_context_make_label(ctx);
  sexp_context_patch_label(ctx, label1);
  sexp_generate(ctx, name, loc, lam, sexp_cnd_fail(cnd));
  sexp_context_patch_label(ctx, label2);
}

static void generate_non_global_ref (sexp ctx, sexp name, sexp cell,
                                     sexp lambda, sexp fv, int unboxp) {
  sexp_uint_t i;
  sexp loc = sexp_cdr(cell);
  if (loc == lambda && sexp_lambdap(lambda)) {
    /* local ref */
    sexp_emit(ctx, SEXP_OP_LOCAL_REF);
    sexp_emit_word(ctx, sexp_param_index(lambda, name));
  } else {
    /* closure ref */
    for (i=0; sexp_pairp(fv); fv=sexp_cdr(fv), i++)
      if ((name == sexp_ref_name(sexp_car(fv)))
          && (loc == sexp_ref_loc(sexp_car(fv))))
        break;
    sexp_emit(ctx, SEXP_OP_CLOSURE_REF);
    sexp_emit_word(ctx, i);
  }
  if (unboxp && (sexp_truep(sexp_memq(ctx, name, sexp_lambda_sv(loc)))))
    sexp_emit(ctx, SEXP_OP_CDR);
  sexp_inc_context_depth(ctx, +1);
}

static void generate_ref (sexp ctx, sexp ref, int unboxp) {
  sexp lam;
  sexp_push_source(ctx, sexp_ref_source(ref));
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global ref */
    if (unboxp) {
      sexp_emit(ctx, (sexp_cdr(sexp_ref_cell(ref)) == SEXP_UNDEF)
		? SEXP_OP_GLOBAL_REF : SEXP_OP_GLOBAL_KNOWN_REF);
      sexp_emit_word(ctx, (sexp_uint_t)sexp_ref_cell(ref));
      bytecode_preserve(ctx, sexp_ref_cell(ref));
    } else
      sexp_emit_push(ctx, sexp_ref_cell(ref));
  } else {
    lam = sexp_context_lambda(ctx);
    generate_non_global_ref(ctx, sexp_ref_name(ref), sexp_ref_cell(ref),
                            lam, sexp_lambda_fv(lam), unboxp);
  }
}

static void generate_set (sexp ctx, sexp set) {
  sexp ref = sexp_set_var(set), lambda;
  sexp_push_source(ctx, sexp_set_source(set));
  /* compile the value */
  sexp_context_tailp(ctx) = 0;
  if (sexp_lambdap(sexp_set_value(set))) {
    sexp_lambda_name(sexp_set_value(set)) = sexp_ref_name(ref);
    sexp_generate(ctx, sexp_ref_name(ref), sexp_ref_loc(ref), sexp_set_value(set), sexp_set_value(set));
  } else {
    sexp_generate(ctx, 0, 0, 0, sexp_set_value(set));
  }
  if (! sexp_lambdap(sexp_ref_loc(ref))) {
    /* global vars are set directly */
    if (sexp_cdr(sexp_ref_cell(ref)) == SEXP_UNDEF) {
      /* force an undefined variable error if still undef at runtime */
      generate_ref(ctx, ref, 1);
      sexp_emit(ctx, SEXP_OP_DROP);
    }
    sexp_emit_push(ctx, sexp_ref_cell(ref));
    sexp_emit(ctx, SEXP_OP_SET_CDR);
  } else {
    lambda = sexp_ref_loc(ref);
    if (sexp_truep(sexp_memq(ctx, sexp_ref_name(ref), sexp_lambda_sv(lambda)))) {
      /* stack or closure mutable vars are boxed */
      generate_ref(ctx, ref, 0);
      sexp_emit(ctx, SEXP_OP_SET_CDR);
    } else {
      /* internally defined variable */
      sexp_emit(ctx, SEXP_OP_LOCAL_SET);
      sexp_emit_word(ctx, sexp_param_index(lambda, sexp_ref_name(ref)));
    }
  }
  sexp_emit_push(ctx, SEXP_VOID);
  sexp_inc_context_depth(ctx, +1);
}

static void generate_opcode_app (sexp ctx, sexp app) {
  sexp op = sexp_car(app);
  sexp_sint_t i, num_args, inv_default=0;
  sexp_gc_var1(ls);
  sexp_gc_preserve1(ctx, ls);

  if (sexp_opcode_tail_call_p(op) && !sexp_context_tailp(ctx)) {
    sexp_warn(ctx, "tail-call only opcode in non-tail position: ", app);
    generate_lit(ctx, SEXP_VOID);
    return;
  }

  num_args = sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(app)));
  sexp_context_tailp(ctx) = 0;

  if (sexp_opcode_class(op) != SEXP_OPC_PARAMETER) {

    /* maybe push the default for an optional argument */
    if ((num_args == sexp_opcode_num_args(op))
        && sexp_opcode_variadic_p(op) && sexp_opcode_data(op)) {
      if (sexp_opcode_inverse(op)) {
        inv_default = 1;
      } else {
        if (sexp_opcode_opt_param_p(op) && sexp_opcodep(sexp_opcode_data(op))) {
#if SEXP_USE_GREEN_THREADS
          sexp_emit(ctx, SEXP_OP_PARAMETER_REF);
          sexp_emit_word(ctx, (sexp_uint_t)sexp_opcode_data(op));
          bytecode_preserve(ctx, sexp_opcode_data(op));
#else
          sexp_emit_push(ctx, sexp_opcode_data(sexp_opcode_data(op)));
#endif
          sexp_emit(ctx, SEXP_OP_CDR);
        } else {
          sexp_emit_push(ctx, sexp_opcode_data(op));
        }
        sexp_inc_context_depth(ctx, +1);
        num_args++;
      }
    }

    /* push the arguments onto the stack in reverse order */
    if (!sexp_opcode_static_param_p(op)) {
      ls = ((sexp_opcode_inverse(op)
             && (sexp_opcode_class(op) != SEXP_OPC_ARITHMETIC))
            ? sexp_cdr(app) : sexp_reverse(ctx, sexp_cdr(app)));
      for ( ; sexp_pairp(ls); ls = sexp_cdr(ls)) {
        sexp_generate(ctx, 0, 0, 0, sexp_car(ls));
#if SEXP_USE_AUTO_FORCE
        if ((sexp_opcode_class(op) != SEXP_OPC_CONSTRUCTOR)
            || sexp_opcode_code(op) == SEXP_OP_MAKE_VECTOR)
          sexp_emit(ctx, SEXP_OP_FORCE);
#endif
      }
    }

  }

  /* push the default for inverse opcodes */
  if (inv_default) {
    sexp_emit_push(ctx, sexp_opcode_data(op));
    if (sexp_opcode_opt_param_p(op)) sexp_emit(ctx, SEXP_OP_CDR);
    sexp_inc_context_depth(ctx, +1);
    num_args++;
  }

  /* emit the actual operator call */
  switch (sexp_opcode_class(op)) {
  case SEXP_OPC_ARITHMETIC:
    /* fold variadic arithmetic operators */
    for (i=num_args-1; i>0; i--)
      sexp_emit(ctx, sexp_opcode_code(op));
    break;
  case SEXP_OPC_ARITHMETIC_CMP:
    if (num_args > 2) {
      sexp_emit(ctx, SEXP_OP_STACK_REF);
      sexp_emit_word(ctx, 2);
      sexp_emit(ctx, SEXP_OP_STACK_REF);
      sexp_emit_word(ctx, 2);
      sexp_emit(ctx, sexp_opcode_code(op));
      sexp_emit(ctx, SEXP_OP_AND);
      for (i=num_args-2; i>0; i--) {
        sexp_emit(ctx, SEXP_OP_STACK_REF);
        sexp_emit_word(ctx, 3);
        sexp_emit(ctx, SEXP_OP_STACK_REF);
        sexp_emit_word(ctx, 3);
        sexp_emit(ctx, sexp_opcode_code(op));
        sexp_emit(ctx, SEXP_OP_AND);
        sexp_emit(ctx, SEXP_OP_AND);
      }
    } else
      sexp_emit(ctx, sexp_opcode_code(op));
    break;
  case SEXP_OPC_FOREIGN:
    sexp_emit(ctx, sexp_opcode_code(op));
    sexp_emit_word(ctx, (sexp_uint_t)op);
    bytecode_preserve(ctx, op);
    break;
  case SEXP_OPC_TYPE_PREDICATE:
  case SEXP_OPC_GETTER:
  case SEXP_OPC_SETTER:
  case SEXP_OPC_CONSTRUCTOR:
    sexp_emit(ctx, sexp_opcode_code(op));
    if ((sexp_opcode_class(op) != SEXP_OPC_CONSTRUCTOR)
        || sexp_opcode_code(op) == SEXP_OP_MAKE) {
      if (sexp_opcode_data(op))
        sexp_emit_word(ctx, sexp_unbox_fixnum(sexp_opcode_data(op)));
      if (sexp_opcode_data2(op))
        sexp_emit_word(ctx, sexp_unbox_fixnum(sexp_opcode_data2(op)));
      if (sexp_opcode_data(op) || sexp_opcode_data2(op))
        bytecode_preserve(ctx, op);
    }
    break;
  case SEXP_OPC_PARAMETER:
#if SEXP_USE_GREEN_THREADS
    if (num_args > 0) {
      if (sexp_opcode_data2(op) && sexp_applicablep(sexp_opcode_data2(op))) {
        ls = sexp_list2(ctx, sexp_opcode_data2(op), sexp_cadr(app));
        sexp_generate(ctx, 0, 0, 0, ls);
      } else {
        sexp_generate(ctx, 0, 0, 0, sexp_cadr(app));
      }
    }
    sexp_emit(ctx, SEXP_OP_PARAMETER_REF);
    sexp_emit_word(ctx, (sexp_uint_t)op);
    bytecode_preserve(ctx, op);
#else
    if (num_args > 0) sexp_generate(ctx, 0, 0, 0, sexp_cadr(app));
    sexp_emit_push(ctx, sexp_opcode_data(op));
#endif
    sexp_emit(ctx, ((num_args == 0) ? SEXP_OP_CDR : SEXP_OP_SET_CDR));
    if (num_args > 0) sexp_emit_push(ctx, SEXP_VOID);
    break;
  default:
    sexp_emit(ctx, sexp_opcode_code(op));
  }

  if (sexp_opcode_static_param_p(op))
    for (ls=sexp_cdr(app); sexp_pairp(ls); ls=sexp_cdr(ls))
      sexp_emit_word(ctx, sexp_unbox_fixnum(sexp_litp(sexp_car(ls)) ?
					    sexp_lit_value(sexp_car(ls)) :
					    sexp_car(ls)));

  if (sexp_opcode_return_type(op) == SEXP_VOID
      && sexp_opcode_class(op) != SEXP_OPC_FOREIGN)
    sexp_emit_push(ctx, SEXP_VOID);

  sexp_inc_context_depth(ctx, -(num_args-1));
  sexp_gc_release1(ctx);
}

static void generate_general_app (sexp ctx, sexp app) {
  sexp_uint_t len = sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(app))),
    tailp = sexp_context_tailp(ctx);
  sexp_gc_var1(ls);
  sexp_gc_preserve1(ctx, ls);

  /* push the arguments onto the stack */
  sexp_context_tailp(ctx) = 0;
  for (ls=sexp_reverse(ctx, sexp_cdr(app)); sexp_pairp(ls); ls=sexp_cdr(ls))
    sexp_generate(ctx, 0, 0, 0, sexp_car(ls));

  /* push the operator onto the stack */
  sexp_generate(ctx, 0, 0, 0, sexp_car(app));

  /* maybe overwrite the current frame */
  sexp_emit(ctx, (tailp ? SEXP_OP_TAIL_CALL : SEXP_OP_CALL));
  sexp_emit_word(ctx, (sexp_uint_t)sexp_make_fixnum(len));

  sexp_context_tailp(ctx) = tailp;
  sexp_inc_context_depth(ctx, -len);
  sexp_gc_release1(ctx);
}

#if SEXP_USE_TAIL_JUMPS
static void generate_tail_jump (sexp ctx, sexp name, sexp loc, sexp lam, sexp app) {
  sexp_gc_var3(ls1, ls2, ls3);
  sexp_gc_preserve3(ctx, ls1, ls2, ls3);

  /* overwrite the arguments that differ */
  sexp_context_tailp(ctx) = 0;
  for (ls1=sexp_cdr(app), ls2=sexp_lambda_params(lam), ls3=SEXP_NULL;
       sexp_pairp(ls1); ls1=sexp_cdr(ls1), ls2=sexp_cdr(ls2)) {
    if (!(sexp_refp(sexp_car(ls1))
          && sexp_ref_name(sexp_car(ls1)) == sexp_car(ls2)
          && sexp_ref_loc(sexp_car(ls1)) == lam
          && sexp_not(sexp_memq(ctx, sexp_car(ls2), sexp_lambda_sv(lam))))) {
      sexp_generate(ctx, 0, 0, 0, sexp_car(ls1));
      ls3 = sexp_cons(ctx, sexp_car(ls2), ls3);
    }
  }
  for (ls1=ls3; sexp_pairp(ls1); ls1=sexp_cdr(ls1)) {
    sexp_emit(ctx, SEXP_OP_LOCAL_SET);
    sexp_emit_word(ctx, sexp_param_index(lam, sexp_car(ls1)));
  }

  /* drop the current result and jump */
  sexp_emit(ctx, SEXP_OP_JUMP);
  sexp_emit_word(ctx, (sexp_uint_t) (-sexp_unbox_fixnum(sexp_context_pos(ctx)) +
				     (sexp_pairp(sexp_lambda_locals(lam))
				      ? 1 + sizeof(sexp) : 0)));

  sexp_context_tailp(ctx) = 1;
  sexp_gc_release3(ctx);
}
#endif

static void generate_app (sexp ctx, sexp name, sexp loc, sexp lam, sexp app) {
  sexp_push_source(ctx, sexp_pair_source(app));
  if (sexp_opcodep(sexp_car(app)))
    generate_opcode_app(ctx, app);
#if SEXP_USE_TAIL_JUMPS
  else if (sexp_context_tailp(ctx) && sexp_refp(sexp_car(app))
           && name == sexp_ref_name(sexp_car(app))
           && loc == sexp_ref_loc(sexp_car(app))
           && (sexp_length(ctx, sexp_cdr(app))
               == sexp_length(ctx, sexp_lambda_params(lam))))
    generate_tail_jump(ctx, name, loc, lam, app);
#endif
  else
    generate_general_app(ctx, app);
}

#if SEXP_USE_UNBOXED_LOCALS
static int sexp_internal_definep(sexp ctx, sexp x) {
  return sexp_lambdap(sexp_ref_loc(x))
    && sexp_truep(sexp_memq(ctx, sexp_ref_name(x),
                            sexp_lambda_locals(sexp_ref_loc(x))));
}

static int sexp_mutual_internal_definep(sexp ctx, sexp x, sexp fv) {
  return sexp_internal_definep(ctx, x)
    && sexp_ref_loc(x) == sexp_ref_loc(fv) && sexp_internal_definep(ctx, fv)
    && sexp_not(sexp_memq(ctx, sexp_ref_name(fv),
                          sexp_lambda_sv(sexp_ref_loc(fv))));
}

static int generate_lambda_locals (sexp ctx, sexp name, sexp loc, sexp lam, sexp x) {
  sexp ls;
  if (sexp_seqp(x)) {
    for (ls=sexp_seq_ls(x); sexp_pairp(ls); ls=sexp_cdr(ls))
      if (!generate_lambda_locals(ctx, name, loc, lam, sexp_car(ls)))
        return 0;
    return 1;
  } else if (sexp_setp(x) && sexp_internal_definep(ctx, sexp_set_var(x))) {
    sexp_generate(ctx, name, loc, lam, x);
    sexp_inc_context_pos(ctx, -(1 + sizeof(sexp)));
    return 1;
  }
  return 0;
}

static int generate_lambda_body (sexp ctx, sexp name, sexp loc, sexp lam, sexp x, sexp prev_lam) {
  sexp_uint_t k, updatep, tailp;
  sexp ls, ref, fv, prev_fv;
  if (sexp_exceptionp(sexp_context_exception(ctx)))
    return 0;
  if (sexp_seqp(x)) {
    tailp = sexp_context_tailp(ctx);
    sexp_context_tailp(ctx) = 0;
    for (ls=sexp_seq_ls(x); sexp_pairp(ls); ls=sexp_cdr(ls)) {
      if (sexp_nullp(sexp_cdr(ls))) sexp_context_tailp(ctx) = tailp;
      if (!generate_lambda_body(ctx, name, loc, lam, sexp_car(ls), prev_lam)) {
        if (sexp_pairp(sexp_cdr(ls))) {
          generate_drop_prev(ctx, sexp_car(ls));
          for (ls=sexp_cdr(ls); sexp_pairp(ls) && sexp_pairp(sexp_cdr(ls));
               ls=sexp_cdr(ls)) {
            sexp_generate(ctx, name, loc, lam, sexp_car(ls));
            generate_drop_prev(ctx, sexp_car(ls));
          }
          sexp_context_tailp(ctx) = tailp;
          sexp_generate(ctx, name, loc, lam, sexp_car(ls));
        }
        return 0;
      }
    }
    return 1;
  } else if (sexp_setp(x) && sexp_internal_definep(ctx, sexp_set_var(x))) {
    updatep = 0;
    if (sexp_lambdap(sexp_set_value(x))) {
      /* update potentially changed bindings */
      fv = sexp_lambda_fv(sexp_set_value(x));
      prev_fv = sexp_lambdap(prev_lam) ? sexp_lambda_fv(prev_lam) : SEXP_NULL;
      for (k=0; fv && sexp_pairp(fv); fv=sexp_cdr(fv), k++) {
        ref = sexp_car(fv);
        if (sexp_mutual_internal_definep(ctx, sexp_set_var(x), ref)) {
          if (!updatep) {
            updatep = 1;
            generate_non_global_ref(ctx, sexp_ref_name(sexp_set_var(x)),
                                    sexp_ref_cell(sexp_set_var(x)),
                                    lam, sexp_lambda_fv(lam), 1);
            sexp_emit(ctx, SEXP_OP_CLOSURE_VARS);
          }
          generate_non_global_ref(ctx, sexp_ref_name(ref), sexp_ref_cell(ref),
                                  lam, sexp_lambda_fv(lam), 1);
          sexp_emit_push(ctx, sexp_make_fixnum(k));
          sexp_emit(ctx, SEXP_OP_STACK_REF);
          sexp_emit_word(ctx, 3);
          sexp_emit(ctx, SEXP_OP_VECTOR_SET);
          sexp_inc_context_depth(ctx, -1);
        }
      }
    }
    if (updatep) sexp_emit(ctx, SEXP_OP_DROP);
    return 1;
  }
  sexp_generate(ctx, name, loc, lam, x);
  return 0;
}
#endif

static void generate_lambda (sexp ctx, sexp name, sexp loc, sexp lam, sexp lambda) {
  sexp ctx2, fv, ls, flags, len, ref, prev_lambda, prev_fv;
  sexp_sint_t k;
  sexp_gc_var2(tmp, bc);
  if (sexp_exceptionp(sexp_context_exception(ctx)))
    return;
  prev_lambda = sexp_context_lambda(ctx);
  prev_fv = sexp_lambdap(prev_lambda) ? sexp_lambda_fv(prev_lambda) : SEXP_NULL;
  fv = sexp_lambda_fv(lambda);
  ctx2 = sexp_make_eval_context(ctx, sexp_context_stack(ctx), sexp_context_env(ctx), 0, 0);
  if (sexp_exceptionp(ctx2)) {
    sexp_context_exception(ctx) = ctx2;
    return;
  }
  sexp_context_lambda(ctx2) = lambda;
  sexp_gc_preserve2(ctx, tmp, bc);
  tmp = sexp_cons(ctx2, SEXP_ZERO, sexp_lambda_source(lambda));
  /* allocate space for local vars */
  k = sexp_unbox_fixnum(sexp_length(ctx, sexp_lambda_locals(lambda)));
  if (k > 0) {
#if SEXP_USE_RESERVE_OPCODE
    sexp_emit(ctx2, SEXP_OP_RESERVE);
    sexp_emit_word(ctx2, k);
#else
    while (k--) sexp_emit_push(ctx2, SEXP_UNDEF);
#endif
  }
  /* box mutable vars */
  for (ls=sexp_lambda_sv(lambda); sexp_pairp(ls); ls=sexp_cdr(ls)) {
    k = sexp_param_index(lambda, sexp_car(ls));
    sexp_emit(ctx2, SEXP_OP_LOCAL_REF);
    sexp_emit_word(ctx2, k);
    sexp_emit_push(ctx2, sexp_car(ls));
    sexp_emit(ctx2, SEXP_OP_CONS);
    sexp_emit(ctx2, SEXP_OP_LOCAL_SET);
    sexp_emit_word(ctx2, k);
  }
  if (lam != lambda) loc = 0;
#if SEXP_USE_UNBOXED_LOCALS
  sexp_context_tailp(ctx2) = 0;
  generate_lambda_locals(ctx2, name, loc, lambda, sexp_lambda_body(lambda));
  sexp_context_tailp(ctx2) = 1;
  generate_lambda_body(ctx2, name, loc, lambda, sexp_lambda_body(lambda), prev_lambda);
#else
  sexp_context_tailp(ctx2) = 1;
  sexp_generate(ctx2, name, loc, lam, sexp_lambda_body(lambda));
#endif
  flags = sexp_make_fixnum(sexp_not(sexp_listp(ctx, sexp_lambda_params(lambda)))
                           ? (SEXP_PROC_VARIADIC + (sexp_rest_unused_p(lambda)
                                                    ? SEXP_PROC_UNUSED_REST: 0))
                           : SEXP_PROC_NONE);
  len = sexp_length(ctx2, sexp_lambda_params(lambda));
  bc = sexp_complete_bytecode(ctx2);
  if (sexp_exceptionp(bc)) {
    sexp_context_exception(ctx) = bc;
  } else {
  sexp_bytecode_name(bc) = sexp_lambda_name(lambda);
#if ! SEXP_USE_FULL_SOURCE_INFO
  sexp_bytecode_source(bc) = sexp_lambda_source(lambda);
#endif
  if (sexp_nullp(fv)) {
    /* shortcut, no free vars */
    tmp = sexp_make_vector(ctx2, SEXP_ZERO, SEXP_VOID);
    tmp = sexp_make_procedure(ctx2, flags, len, bc, tmp);
    bytecode_preserve(ctx, tmp);
    generate_lit(ctx, tmp);
  } else {
    /* push the closed vars */
    sexp_emit_push(ctx, SEXP_VOID);
    sexp_emit_push(ctx, sexp_length(ctx, fv));
    sexp_emit(ctx, SEXP_OP_MAKE_VECTOR);
    sexp_inc_context_depth(ctx, -1);
    for (k=0; sexp_pairp(fv); fv=sexp_cdr(fv), k++) {
      ref = sexp_car(fv);
      generate_non_global_ref(ctx, sexp_ref_name(ref), sexp_ref_cell(ref),
                              prev_lambda, prev_fv, 0);
      sexp_emit_push(ctx, sexp_make_fixnum(k));
      sexp_emit(ctx, SEXP_OP_STACK_REF);
      sexp_emit_word(ctx, 3);
      sexp_emit(ctx, SEXP_OP_VECTOR_SET);
      sexp_inc_context_depth(ctx, -1);
    }
    /* push the additional procedure info and make the closure */
    sexp_emit(ctx, SEXP_OP_MAKE_PROCEDURE);
    sexp_emit_word(ctx, (sexp_uint_t)flags);
    sexp_emit_word(ctx, (sexp_uint_t)len);
    sexp_emit_word(ctx, (sexp_uint_t)bc);
    bytecode_preserve(ctx, bc);
  }
  }
  sexp_gc_release2(ctx);
}

void sexp_generate (sexp ctx, sexp name, sexp loc, sexp lam, sexp x) {
  if (sexp_exceptionp(sexp_context_exception(ctx)))
    return;
  if (sexp_pointerp(x)) {
    switch (sexp_pointer_tag(x)) {
    case SEXP_PAIR:   generate_app(ctx, name, loc, lam, x); break;
    case SEXP_LAMBDA: generate_lambda(ctx, name, loc, lam, x); break;
    case SEXP_CND:    generate_cnd(ctx, name, loc, lam, x); break;
    case SEXP_REF:    generate_ref(ctx, x, 1); break;
    case SEXP_SET:    generate_set(ctx, x); break;
    case SEXP_SEQ:    generate_seq(ctx, name, loc, lam, sexp_seq_ls(x)); break;
    case SEXP_LIT:    generate_lit(ctx, sexp_lit_value(x)); break;
    default:          generate_lit(ctx, x);
    }
  } else {
    generate_lit(ctx, x);
  }
}

static sexp make_param_list (sexp ctx, sexp_uint_t i) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_NULL;
  for ( ; i>0; i--)
    res = sexp_cons(ctx, sexp_make_fixnum(i), res);
  sexp_gc_release1(ctx);
  return res;
}

static sexp make_opcode_procedure (sexp ctx, sexp op, sexp_uint_t i) {
  sexp ls, res, env;
  sexp_gc_var6(bc, params, ref, refs, lambda, ctx2);
  if (i == sexp_opcode_num_args(op)) { /* return before preserving */
    if (sexp_opcode_proc(op)) return sexp_opcode_proc(op);
  } else if (i < sexp_opcode_num_args(op)) {
    return sexp_compile_error(ctx, "not enough args for opcode", op);
  } else if (! sexp_opcode_variadic_p(op)) { /* i > num_args */
    return sexp_compile_error(ctx, "too many args for opcode", op);
  }
  sexp_gc_preserve6(ctx, bc, params, ref, refs, lambda, ctx2);
  params = make_param_list(ctx, i);
  lambda = sexp_make_lambda(ctx, params);
  ctx2 = sexp_make_child_context(ctx, lambda);
  env = sexp_extend_env(ctx2, sexp_context_env(ctx), params, lambda);
  if (sexp_exceptionp(env)) {
    res = env;
  } else {
    sexp_context_env(ctx2) = env;
    for (ls=params, refs=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls)) {
      ref = sexp_make_ref(ctx2, sexp_car(ls), sexp_env_cell(env, sexp_car(ls), 0));
      if (!sexp_exceptionp(ref)) sexp_push(ctx2, refs, ref);
    }
    if (!sexp_exceptionp(refs))
      refs = sexp_reverse(ctx2, refs);
    refs = sexp_cons(ctx2, op, refs);
    if (sexp_exceptionp(refs)) {
      res = refs;
    } else {
      generate_opcode_app(ctx2, refs);
      bc = sexp_complete_bytecode(ctx2);
      sexp_bytecode_name(bc) = sexp_opcode_name(op);
      res=sexp_make_procedure(ctx2, SEXP_ZERO, sexp_make_fixnum(i), bc, SEXP_VOID);
      if (i == sexp_opcode_num_args(op))
        sexp_opcode_proc(op) = res;
    }
  }
  sexp_gc_release6(ctx);
  return res;
}

/*********************** the virtual machine **************************/

sexp sexp_make_trampoline (sexp ctx, sexp proc, sexp args) {
  return sexp_make_exception(ctx, SEXP_TRAMPOLINE, SEXP_FALSE, args, proc, SEXP_FALSE);
}

#if SEXP_USE_GROW_STACK
static int sexp_grow_stack (sexp ctx, int min_size) {
  sexp stack, old_stack = sexp_context_stack(ctx), *from, *to;
  int i, size = sexp_stack_length(old_stack), new_size;
  new_size = size * 2;
  if (new_size < min_size) new_size = min_size;
  if (new_size > SEXP_MAX_STACK_SIZE) {
    if (size == SEXP_MAX_STACK_SIZE)
      return 0;
    new_size = SEXP_MAX_STACK_SIZE;
  }
  stack = sexp_alloc_tagged(ctx, (sexp_sizeof(stack)+sizeof(sexp)*new_size),
                            SEXP_STACK);
  if (!stack || sexp_exceptionp(stack))
    return 0;
  sexp_stack_length(stack) = new_size;
  sexp_stack_top(stack) = sexp_context_top(ctx);
  from = sexp_stack_data(old_stack);
  to = sexp_stack_data(stack);
  for (i=sexp_context_top(ctx)+1; i>=0; i--)
    to[i] = from[i];
  for (; ctx; ctx=sexp_context_parent(ctx))
    if (sexp_context_stack(ctx) == old_stack)
      sexp_context_stack(ctx) = stack;
  return 1;
}
#else
#define sexp_grow_stack(ctx, min_size) 0
#endif

static sexp sexp_save_stack (sexp ctx, sexp *stack, sexp_uint_t to) {
  sexp res, *data;
  sexp_uint_t i;
  res = sexp_make_vector(ctx, sexp_make_fixnum(to), SEXP_VOID);
  data = sexp_vector_data(res);
  for (i=0; i<to; i++)
    data[i] = stack[i];
  return res;
}

static sexp sexp_restore_stack (sexp ctx, sexp saved) {
  sexp_uint_t len = sexp_vector_length(saved), i;
  sexp *from = sexp_vector_data(saved), *to;
#if SEXP_USE_CHECK_STACK
  if ((len+64 >= sexp_stack_length(sexp_context_stack(ctx)))
      && !sexp_grow_stack(ctx, len+64))
    return sexp_global(ctx, SEXP_G_OOS_ERROR);
#endif
  to = sexp_stack_data(sexp_context_stack(ctx));
  for (i=0; i<len; i++)
    to[i] = from[i];
  sexp_context_top(ctx) = len;
  return SEXP_VOID;
}

#define _ARG1 stack[top-1]
#define _ARG2 stack[top-2]
#define _ARG3 stack[top-3]
#define _ARG4 stack[top-4]
#define _ARG5 stack[top-5]
#define _ARG6 stack[top-6]
#define _PUSH(x) (stack[top++]=(x))
#define _POP() (stack[--top])

#if SEXP_USE_ALIGNED_BYTECODE
#define _ALIGN_IP() ip = (unsigned char *)sexp_word_align((sexp_uint_t)ip)
#else
#define _ALIGN_IP()
#endif

#define _WORD0 ((sexp*)ip)[0]
#define _UWORD0 ((sexp_uint_t*)ip)[0]
#define _SWORD0 ((sexp_sint_t*)ip)[0]
#define _WORD1 ((sexp*)ip)[1]
#define _UWORD1 ((sexp_uint_t*)ip)[1]
#define _SWORD1 ((sexp_sint_t*)ip)[1]
#define _WORD2 ((sexp*)ip)[2]

#define sexp_raise(msg, args)                                       \
  do {sexp_context_top(ctx) = top+1;                                \
      stack[top] = args;                                            \
      stack[top] = sexp_user_exception(ctx, self, msg, stack[top]); \
      top++;                                                        \
      goto call_error_handler;}                                     \
  while (0)

#define sexp_check_exception()                                 \
  do {if (sexp_exceptionp(_ARG1)) {                            \
      goto call_error_handler;}}                               \
    while (0)

static int sexp_check_type(sexp ctx, sexp a, sexp b) {
  int d;
  sexp t, v;
  if (! sexp_pointerp(a))
    return 0;
  if (sexp_isa(a, b))
    return 1;
  t = sexp_object_type(ctx, a);
  v = sexp_type_cpl(t);
  if (! sexp_vectorp(v))
    return 0;
  if (b == sexp_type_by_index(ctx, SEXP_OBJECT))
    return 1;
  d = sexp_type_depth(b);
  return (d < sexp_vector_length(v))
    && sexp_vector_ref(v, sexp_make_fixnum(d)) == b;
}

#if SEXP_USE_GREEN_THREADS
#define sexp_fcall_return(x, i)                             \
  if (sexp_exceptionp(x)) {                                 \
    if (x == sexp_global(ctx, SEXP_G_IO_BLOCK_ERROR)) {     \
      fuel = 0; ip--; goto loop;                            \
    } else {                                                \
      top -= i;                                             \
      _ARG1 = x;                                            \
      ip += sizeof(sexp);                                   \
      goto call_error_handler;                              \
    }                                                       \
  } else {                                                  \
    top -= i;                                               \
    _ARG1 = x;                                              \
    ip += sizeof(sexp);                                     \
  }
#else
#define sexp_fcall_return(x, i)                                 \
  top -= i; _ARG1 = x; ip += sizeof(sexp); sexp_check_exception();
#endif

#if SEXP_USE_EXTENDED_FCALL
#include "opt/fcall.c"
#endif

#if SEXP_USE_PROFILE_VM
sexp_uint_t profile1[SEXP_OP_NUM_OPCODES];
sexp_uint_t profile2[SEXP_OP_NUM_OPCODES][SEXP_OP_NUM_OPCODES];

sexp sexp_reset_vm_profile (sexp ctx, sexp self, sexp_sint_t n) {
  int i, j;
  for (i=0; i<SEXP_OP_NUM_OPCODES; i++) {
    profile1[i] = 0;
    for (j=0; j<SEXP_OP_NUM_OPCODES; j++) profile2[i][j] = 0;
  }
  return SEXP_VOID;
}

sexp sexp_print_vm_profile (sexp ctx, sexp self, sexp_sint_t n) {
  int i, j;
  for (i=0; i<SEXP_OP_NUM_OPCODES; i++)
    fprintf(stderr, "%s %lu\n", sexp_opcode_names[i], profile1[i]);
  for (i=0; i<SEXP_OP_NUM_OPCODES; i++)
    for (j=0; j<SEXP_OP_NUM_OPCODES; j++)
      fprintf(stderr, "%s %s %lu\n", sexp_opcode_names[i],
              sexp_opcode_names[j], profile2[i][j]);
  return SEXP_VOID;
}
#endif

#if SEXP_USE_DEBUG_THREADS
static const char* sexp_thread_debug_name(sexp ctx) {
  if (sexp_stringp(sexp_context_name(ctx)))
    return sexp_string_data(sexp_context_name(ctx));
  return "?";
}

static char* sexp_thread_debug_event_type(sexp ctx) {
  sexp evt = sexp_context_event(ctx);
  return sexp_portp(evt) ? "p" : sexp_contextp(evt) ? "c" : "?";
}

static void* sexp_thread_debug_event(sexp ctx) {
  return (void*)sexp_context_event(ctx);
}
#endif

#if SEXP_USE_CHECK_STACK
#define sexp_ensure_stack(n)                                            \
  if (top+(n) >= sexp_stack_length(sexp_context_stack(ctx))) {          \
    sexp_context_top(ctx) = top;                                        \
    if (sexp_grow_stack(ctx, (n))) {                                    \
      stack = sexp_stack_data(sexp_context_stack(ctx));                 \
    } else {                                                            \
      _ARG1 = sexp_global(ctx, SEXP_G_OOS_ERROR);                       \
      goto end_loop;                                                    \
    }                                                                   \
  }
#else
#define sexp_ensure_stack(n)
#endif

sexp sexp_apply (sexp ctx, sexp proc, sexp args) {
  unsigned char *ip;
  sexp bc, cp, *stack = sexp_stack_data(sexp_context_stack(ctx));
  sexp_sint_t i, j, k, fp, top = sexp_stack_top(sexp_context_stack(ctx));
#if SEXP_USE_GREEN_THREADS
  sexp root_thread = ctx;
  sexp_sint_t fuel = sexp_context_refuel(ctx);
#endif
#if SEXP_USE_PROFILE_VM
  unsigned char last_op = SEXP_OP_NOOP;
#endif
#if SEXP_USE_BIGNUMS
  sexp_lsint_t prod;
#endif
  sexp_gc_var3(self, tmp1, tmp2);
  sexp_gc_preserve3(ctx, self, tmp1, tmp2);
  fp = top - 4;
  self = sexp_global(ctx, SEXP_G_FINAL_RESUMER);
  bc = sexp_procedure_code(self);
  cp = sexp_procedure_vars(self);
  ip = sexp_bytecode_data(bc) - sizeof(sexp);
  tmp1 = proc, tmp2 = args;
  i = sexp_unbox_fixnum(sexp_length(ctx, tmp2));
  sexp_ensure_stack(i + 64 + (sexp_procedurep(tmp1) ? sexp_bytecode_max_depth(sexp_procedure_code(tmp1)) : 0));
  for (top += i; sexp_pairp(tmp2); tmp2=sexp_cdr(tmp2), top--)
    _ARG1 = sexp_car(tmp2);
  top += i+1;
  goto make_call;

 loop:
#if SEXP_USE_GREEN_THREADS
  if (--fuel <= 0) {
    tmp1 = sexp_global(ctx, SEXP_G_THREADS_SCHEDULER);
    if (sexp_applicablep(tmp1) && sexp_not(sexp_global(ctx, SEXP_G_ATOMIC_P))) {
      /* save thread */
      sexp_context_top(ctx) = top;
      sexp_context_ip(ctx) = ip;
      sexp_context_last_fp(ctx) = fp;
      sexp_context_proc(ctx) = self;
      /* run scheduler */
#if SEXP_USE_DEBUG_THREADS
      tmp2 = ctx;
#endif
      ctx = sexp_apply1(ctx, tmp1, root_thread);
      /* restore thread */
      stack = sexp_stack_data(sexp_context_stack(ctx));
      top = sexp_context_top(ctx);
      fp = sexp_context_last_fp(ctx);
      ip = sexp_context_ip(ctx);
      self = sexp_context_proc(ctx);
      bc = sexp_procedure_code(self);
      cp = sexp_procedure_vars(self);
#if SEXP_USE_DEBUG_THREADS
      if (ctx != tmp2) {
        fprintf(stderr, "****** schedule %p: %p (%s) active:",
                root_thread, ctx, sexp_thread_debug_name(ctx));
        for (tmp1=sexp_global(ctx, SEXP_G_THREADS_FRONT); sexp_pairp(tmp1); tmp1=sexp_cdr(tmp1))
          fprintf(stderr, " %p (%s)", sexp_car(tmp1), sexp_thread_debug_name(sexp_car(tmp1)));
        fprintf(stderr, " paused:");
        for (tmp1=sexp_global(ctx, SEXP_G_THREADS_PAUSED); sexp_pairp(tmp1); tmp1=sexp_cdr(tmp1))
          fprintf(stderr, " %p (%s) [%s %p]", sexp_car(tmp1), sexp_thread_debug_name(sexp_car(tmp1)), sexp_thread_debug_event_type(sexp_car(tmp1)), sexp_thread_debug_event(sexp_car(tmp1)));
        fprintf(stderr, " ******\n");
      }
#endif
    }
    fuel = sexp_context_refuel(ctx);
    if (fuel <= 0) goto end_loop;
  }
#endif
#if SEXP_USE_DEBUG_VM
  if (sexp_context_tracep(ctx)) {
    sexp_print_stack(ctx, stack, top, fp, SEXP_FALSE);
    fprintf(stderr, "****** VM %s %s ip: %p stack: %p top: %ld fp: %ld (%ld)\n",
            (*ip<=SEXP_OP_NUM_OPCODES) ? sexp_opcode_names[*ip] : "UNKNOWN",
            (SEXP_OP_FCALL0 <= *ip && *ip <= SEXP_OP_FCALL4
             ? sexp_string_data(sexp_opcode_name(((sexp*)(ip+1))[0])) : ""),
            ip, stack, top, fp, (fp<1024 ? sexp_unbox_fixnum(stack[fp+3]) : -1));
  }
#endif
#if SEXP_USE_PROFILE_VM
  profile1[*ip]++;
  profile2[last_op][*ip]++;
  last_op = *ip;
#endif
  switch (*ip++) {
  case SEXP_OP_NOOP:
    break;
  call_error_handler:
    if (! sexp_exception_procedure(_ARG1))
      sexp_exception_procedure(_ARG1) = self;
#if SEXP_USE_FULL_SOURCE_INFO
    if (sexp_not(sexp_exception_source(_ARG1))
        && sexp_procedurep(sexp_exception_procedure(_ARG1))
        && sexp_procedure_source(sexp_exception_procedure(_ARG1)))
      sexp_exception_source(_ARG1) = sexp_lookup_source_info(sexp_exception_procedure(_ARG1), (ip-sexp_bytecode_data(bc)));
#endif
  case SEXP_OP_RAISE:
    sexp_context_top(ctx) = top;
    if (sexp_trampolinep(_ARG1)) {
      tmp1 = sexp_trampoline_procedure(_ARG1);
      tmp2 = sexp_trampoline_args(_ARG1);
      top--;
      goto apply1;
    }
    tmp1 = sexp_parameter_ref(ctx, sexp_global(ctx, SEXP_G_ERR_HANDLER));
    sexp_context_last_fp(ctx) = fp;
    if (! sexp_procedurep(tmp1)) {
#if SEXP_USE_GREEN_THREADS
      sexp_context_errorp(ctx) = 1;
#endif
      goto end_loop;
    }
    stack[top] = SEXP_ONE;
    stack[top+1] = sexp_make_fixnum(ip-sexp_bytecode_data(bc));
    stack[top+2] = self;
    stack[top+3] = sexp_make_fixnum(fp);
    top += 4;
    self = tmp1;
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(self);
    fp = top-4;
    break;
  case SEXP_OP_RESUMECC:
    sexp_context_top(ctx) = top;
    tmp1 = stack[fp-1];
    tmp2 = sexp_restore_stack(ctx, sexp_vector_ref(cp, 0));
    if (sexp_exceptionp(tmp2)) {_ARG1 = tmp2; goto call_error_handler;}
    top = sexp_context_top(ctx);
    fp = sexp_unbox_fixnum(_ARG1);
    self = _ARG2;
    bc = sexp_procedure_code(self);
    cp = sexp_procedure_vars(self);
    ip = sexp_bytecode_data(bc) + sexp_unbox_fixnum(_ARG3);
    i = sexp_unbox_fixnum(_ARG4);
    top -= 4;
    _ARG1 = tmp1;
    break;
  case SEXP_OP_CALLCC:
    stack[top] = SEXP_ONE;
    stack[top+1] = sexp_make_fixnum(ip-sexp_bytecode_data(bc));
    stack[top+2] = self;
    stack[top+3] = sexp_make_fixnum(fp);
    tmp1 = _ARG1;
    i = 1;
    sexp_context_top(ctx) = top;
    tmp2 = sexp_make_vector(ctx, SEXP_ONE, SEXP_UNDEF);
    sexp_vector_set(tmp2, SEXP_ZERO, sexp_save_stack(ctx, stack, top+4));
    _ARG1 = sexp_make_procedure(ctx,
                                SEXP_ZERO,
                                SEXP_ONE,
                                sexp_global(ctx, SEXP_G_RESUMECC_BYTECODE),
                                tmp2);
    top++;
    ip -= sizeof(sexp);
    goto make_call;
  case SEXP_OP_APPLY1:
    tmp1 = _ARG1;
    tmp2 = _ARG2;
  apply1:
    i = sexp_unbox_fixnum(sexp_length(ctx, tmp2)); /* number of params */
    sexp_ensure_stack(i + 64 + (sexp_procedurep(tmp1) ? sexp_bytecode_max_depth(sexp_procedure_code(tmp1)) : 0));
    k = sexp_unbox_fixnum(stack[fp+3]);            /* previous fp */
    j = sexp_unbox_fixnum(stack[fp]);              /* previous num params */
    self = stack[fp+2];
    bc = sexp_procedure_code(self);
    cp = sexp_procedure_vars(self);
    ip = (sexp_bytecode_data(bc)+sexp_unbox_fixnum(stack[fp+1])) - sizeof(sexp);
    for (top=fp-j+i-1; sexp_pairp(tmp2); tmp2=sexp_cdr(tmp2), top--)
      stack[top] = sexp_car(tmp2);
    top = fp+i-j+1;
    fp = k;
    goto make_call;
  case SEXP_OP_TAIL_CALL:
    _ALIGN_IP();
    i = sexp_unbox_fixnum(_WORD0);             /* number of params */
    tmp1 = _ARG1;                              /* procedure to call */
    /* save frame info */
    tmp2 = stack[fp+3];                        /* previous fp */
    j = sexp_unbox_fixnum(stack[fp]);          /* previous num params */
    self = stack[fp+2];
    bc = sexp_procedure_code(self);
    cp = sexp_procedure_vars(self);
    ip = (sexp_bytecode_data(bc)+sexp_unbox_fixnum(stack[fp+1])) - sizeof(sexp);
    /* copy new args into place */
    for (k=0; k<i; k++)
      stack[fp-j+k] = stack[top-1-i+k];
    top = fp+i-j+1;
    fp = sexp_unbox_fixnum(tmp2);
    goto make_call;
  case SEXP_OP_CALL:
    _ALIGN_IP();
    i = sexp_unbox_fixnum(_WORD0);
    tmp1 = _ARG1;
  make_call:
    sexp_context_top(ctx) = top;
    if (sexp_opcodep(tmp1)) {
      /* compile non-inlined opcode applications on the fly */
      tmp1 = make_opcode_procedure(ctx, tmp1, i);
      if (sexp_exceptionp(tmp1)) {
        _ARG1 = tmp1;
        goto call_error_handler;
      }
    }
    if (! sexp_procedurep(tmp1))
      sexp_raise("non procedure application", sexp_list1(ctx, tmp1));
    j = i - sexp_procedure_num_args(tmp1);
    if (j < 0)
      sexp_raise("not enough args",
                 sexp_list2(ctx, tmp1, sexp_make_fixnum(i)));
    /* ensure there's sufficient stack space before pushing args */
    sexp_ensure_stack(sexp_bytecode_max_depth(sexp_procedure_code(tmp1))+64);
    if (j > 0) {
      if (sexp_procedure_variadic_p(tmp1)) {
        if (!sexp_procedure_unused_rest_p(tmp1)) {
          stack[top-i-1] = sexp_cons(ctx, stack[top-i-1], SEXP_NULL);
          for (k=top-i; k<top-(i-j)-1; k++)
            stack[top-i-1] = sexp_cons(ctx, stack[k], stack[top-i-1]);
          for ( ; k<top; k++)
            stack[k-j+1] = stack[k];
          top -= (j-1);
          i -= (j-1);
        }
      } else {
        sexp_raise("too many args", sexp_list2(ctx, tmp1, sexp_make_fixnum(i)));
      }
    } else if (sexp_procedure_variadic_p(tmp1) &&
               !sexp_procedure_unused_rest_p(tmp1)) {
      /* shift stack, set extra arg to null */
      for (k=top; k>=top-i; k--)
        stack[k] = stack[k-1];
      stack[top-i-1] = SEXP_NULL;
      top++;
      i++;
    }
    _ARG1 = sexp_make_fixnum(i);
    stack[top] = sexp_make_fixnum(ip+sizeof(sexp)-sexp_bytecode_data(bc));
    stack[top+1] = self;
    stack[top+2] = sexp_make_fixnum(fp);
    top += 3;
    self = tmp1;
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(self);
    fp = top-4;
    break;
  case SEXP_OP_FCALL0:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    sexp_context_last_fp(ctx) = fp;
    tmp1 = ((sexp_proc1)sexp_opcode_func(_WORD0))(ctx, _WORD0, 0);
    sexp_fcall_return(tmp1, -1)
    break;
  case SEXP_OP_FCALL1:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    sexp_context_last_fp(ctx) = fp;
    tmp1 = ((sexp_proc2)sexp_opcode_func(_WORD0))(ctx, _WORD0, 1, _ARG1);
    sexp_fcall_return(tmp1, 0)
    break;
  case SEXP_OP_FCALL2:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    sexp_context_last_fp(ctx) = fp;
    tmp1 = ((sexp_proc3)sexp_opcode_func(_WORD0))(ctx, _WORD0, 2, _ARG1, _ARG2);
    sexp_fcall_return(tmp1, 1)
    break;
  case SEXP_OP_FCALL3:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    sexp_context_last_fp(ctx) = fp;
    tmp1 = ((sexp_proc4)sexp_opcode_func(_WORD0))(ctx, _WORD0, 3, _ARG1, _ARG2, _ARG3);
    sexp_fcall_return(tmp1, 2)
    break;
  case SEXP_OP_FCALL4:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    sexp_context_last_fp(ctx) = fp;
    tmp1 = ((sexp_proc5)sexp_opcode_func(_WORD0))(ctx, _WORD0, 4, _ARG1, _ARG2, _ARG3, _ARG4);
    sexp_fcall_return(tmp1, 3)
    break;
#if SEXP_USE_EXTENDED_FCALL
  case SEXP_OP_FCALLN:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    sexp_context_last_fp(ctx) = fp;
    i = sexp_opcode_num_args(_WORD0);
    tmp1 = sexp_fcall(ctx, self, i, _WORD0);
    sexp_fcall_return(tmp1, i-1)
    break;
#endif
  case SEXP_OP_JUMP_UNLESS:
    _ALIGN_IP();
    if (stack[--top] == SEXP_FALSE)
      ip += _SWORD0;
    else
      ip += sizeof(sexp_sint_t);
    break;
  case SEXP_OP_JUMP:
    _ALIGN_IP();
    ip += _SWORD0;
    break;
  case SEXP_OP_PUSH:
    _ALIGN_IP();
    _PUSH(_WORD0);
    ip += sizeof(sexp);
    break;
#if SEXP_USE_RESERVE_OPCODE
  case SEXP_OP_RESERVE:
    _ALIGN_IP();
    for (i=_SWORD0; i > 0; i--)
      stack[top++] = SEXP_VOID;
    ip += sizeof(sexp);
    break;
#endif
  case SEXP_OP_DROP:
    top--;
    break;
  case SEXP_OP_GLOBAL_REF:
    _ALIGN_IP();
    if (sexp_cdr(_WORD0) == SEXP_UNDEF)
      sexp_raise("undefined variable", sexp_list1(ctx, sexp_car(_WORD0)));
    /* ... FALLTHROUGH ... */
  case SEXP_OP_GLOBAL_KNOWN_REF:
    _ALIGN_IP();
    _PUSH(sexp_cdr(_WORD0));
    ip += sizeof(sexp);
    break;
#if SEXP_USE_GREEN_THREADS
  case SEXP_OP_PARAMETER_REF:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    tmp2 = _WORD0;
    ip += sizeof(sexp);
    for (tmp1=sexp_context_params(ctx); sexp_pairp(tmp1); tmp1=sexp_cdr(tmp1))
      if (sexp_caar(tmp1) == tmp2) {
        _PUSH(sexp_car(tmp1));
        goto loop;
      }
    _PUSH(sexp_opcode_data(tmp2));
    break;
#endif
  case SEXP_OP_STACK_REF:
    _ALIGN_IP();
    stack[top] = stack[top - _SWORD0];
    ip += sizeof(sexp);
    top++;
    break;
  case SEXP_OP_LOCAL_REF:
    _ALIGN_IP();
    stack[top] = stack[fp - 1 - _SWORD0];
    ip += sizeof(sexp);
    top++;
    break;
  case SEXP_OP_LOCAL_SET:
    _ALIGN_IP();
    stack[fp - 1 - _SWORD0] = _POP();
    ip += sizeof(sexp);
    break;
  case SEXP_OP_CLOSURE_REF:
    _ALIGN_IP();
    _PUSH(sexp_vector_ref(cp, sexp_make_fixnum(_SWORD0)));
    ip += sizeof(sexp);
    break;
  case SEXP_OP_CLOSURE_VARS:
    _ARG1 = sexp_procedure_vars(_ARG1);
    break;
  case SEXP_OP_VECTOR_REF:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-ref: not a vector", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("vector-ref: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_vector_length(_ARG1)))
      sexp_raise("vector-ref: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_vector_ref(_ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_VECTOR_SET:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-set!: not a vector", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("vector-set!: immutable vector", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("vector-set!: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_vector_length(_ARG1)))
      sexp_raise("vector-set!: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    sexp_vector_set(_ARG1, _ARG2, _ARG3);
    top-=3;
    break;
  case SEXP_OP_VECTOR_LENGTH:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-length: not a vector", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_vector_length(_ARG1));
    break;
  case SEXP_OP_BYTES_REF:
    if (! sexp_bytesp(_ARG1))
      sexp_raise("byte-vector-ref: not a byte-vector", sexp_list1(ctx, _ARG1));
    if (! sexp_fixnump(_ARG2))
      sexp_raise("byte-vector-ref: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_bytes_length(_ARG1)))
      sexp_raise("byte-vector-ref: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_bytes_ref(_ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_STRING_REF:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-ref: not a string", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("string-ref: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_string_length(_ARG1)))
      sexp_raise("string-ref: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
#if SEXP_USE_UTF8_STRINGS
      _ARG2 = sexp_string_utf8_ref(ctx, _ARG1, _ARG2);
#else
      _ARG2 = sexp_string_ref(_ARG1, _ARG2);
#endif
    top--;
#if SEXP_USE_UTF8_STRINGS
    sexp_check_exception();
#endif
    break;
  case SEXP_OP_BYTES_SET:
    if (! sexp_bytesp(_ARG1))
      sexp_raise("byte-vector-set!: not a byte-vector", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("byte-vector-set!: immutable byte-vector", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("byte-vector-set!: not an integer", sexp_list1(ctx, _ARG2));
    else if (!(sexp_fixnump(_ARG3) && sexp_unbox_fixnum(_ARG3)>=0
               && sexp_unbox_fixnum(_ARG3)<0x100))
      sexp_raise("byte-vector-set!: not an octet", sexp_list1(ctx, _ARG3));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_bytes_length(_ARG1)))
      sexp_raise("byte-vector-set!: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    sexp_bytes_set(_ARG1, _ARG2, _ARG3);
    top-=3;
    break;
#if SEXP_USE_MUTABLE_STRINGS
  case SEXP_OP_STRING_SET:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-set!: not a string", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("string-set!: immutable string", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("string-set!: not an integer", sexp_list1(ctx, _ARG2));
    else if (! sexp_charp(_ARG3))
      sexp_raise("string-set!: not a char", sexp_list1(ctx, _ARG3));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_string_length(_ARG1)))
      sexp_raise("string-set!: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
#if SEXP_USE_UTF8_STRINGS
    sexp_context_top(ctx) = top;
    sexp_string_utf8_set(ctx, _ARG1, _ARG2, _ARG3);
#else
    sexp_string_set(_ARG1, _ARG2, _ARG3);
#endif
    top-=3;
    break;
#endif
#if SEXP_USE_UTF8_STRINGS
  case SEXP_OP_STRING_CURSOR_NEXT:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-cursor-next: not a string", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("string-cursor-next: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    _ARG2 = sexp_make_fixnum(i + sexp_utf8_initial_byte_count(((unsigned char*)sexp_string_data(_ARG1))[i]));
    top--;
    sexp_check_exception();
    break;
  case SEXP_OP_STRING_CURSOR_PREV:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-cursor-prev: not a string", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("string-cursor-prev: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    _ARG2 = sexp_make_fixnum(sexp_string_utf8_prev((unsigned char*)sexp_string_data(_ARG1)+i) - sexp_string_data(_ARG1));
    top--;
    sexp_check_exception();
    break;
  case SEXP_OP_STRING_SIZE:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-size: not a string", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_string_length(_ARG1));
    break;
#endif
  case SEXP_OP_BYTES_LENGTH:
    if (! sexp_bytesp(_ARG1))
      sexp_raise("bytes-length: not a byte-vector", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_bytes_length(_ARG1));
    break;
  case SEXP_OP_STRING_LENGTH:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-length: not a string", sexp_list1(ctx, _ARG1));
#if SEXP_USE_UTF8_STRINGS
    _ARG1 = sexp_make_fixnum(sexp_string_utf8_length((unsigned char*)sexp_string_data(_ARG1), sexp_string_length(_ARG1)));
#else
    _ARG1 = sexp_make_fixnum(sexp_string_length(_ARG1));
#endif
    break;
  case SEXP_OP_MAKE_PROCEDURE:
    sexp_context_top(ctx) = top;
    _ALIGN_IP();
    _ARG1 = sexp_make_procedure(ctx, _WORD0, _WORD1, _WORD2, _ARG1);
    ip += (3 * sizeof(sexp));
    break;
  case SEXP_OP_MAKE_VECTOR:
    sexp_context_top(ctx) = top;
    if (! sexp_fixnump(_ARG1))
      sexp_raise("make-vector: not an integer", sexp_list1(ctx, _ARG1));
    _ARG2 = sexp_make_vector(ctx, _ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_MAKE_EXCEPTION:
    sexp_context_top(ctx) = top;
    _ARG5 = sexp_make_exception(ctx, _ARG1, _ARG2, _ARG3, _ARG4, _ARG5);
    top -= 4;
    break;
  case SEXP_OP_AND:
    _ARG2 = sexp_make_boolean((_ARG1 != SEXP_FALSE) && (_ARG2 != SEXP_FALSE));
    top--;
    break;
  case SEXP_OP_EOFP:
    _ARG1 = sexp_make_boolean(_ARG1 == SEXP_EOF); break;
  case SEXP_OP_NULLP:
    _ARG1 = sexp_make_boolean(sexp_nullp(_ARG1)); break;
  case SEXP_OP_FIXNUMP:
    _ARG1 = sexp_make_boolean(sexp_fixnump(_ARG1)); break;
  case SEXP_OP_SYMBOLP:
    _ARG1 = sexp_make_boolean(sexp_symbolp(_ARG1)); break;
  case SEXP_OP_CHARP:
    _ARG1 = sexp_make_boolean(sexp_charp(_ARG1)); break;
  case SEXP_OP_ISA:
    tmp1 = _ARG1, tmp2 = _ARG2;
    if (! sexp_typep(tmp2)) sexp_raise("is-a?: not a type", tmp2);
    top--;
    goto do_check_type;
  case SEXP_OP_TYPEP:
    _ALIGN_IP();
    tmp1 = _ARG1, tmp2 = sexp_type_by_index(ctx, _UWORD0);
    ip += sizeof(sexp);
  do_check_type:
    _ARG1 = sexp_make_boolean(sexp_check_type(ctx, tmp1, tmp2));
    break;
  case SEXP_OP_MAKE:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _PUSH(sexp_alloc_tagged(ctx, _UWORD1, _UWORD0));
    ip += sizeof(sexp)*2;
    break;
  case SEXP_OP_SLOT_REF:
    _ALIGN_IP();
    if (! sexp_check_type(ctx, _ARG1, sexp_type_by_index(ctx, _UWORD0)))
      sexp_raise("slot-ref: bad type", sexp_list2(ctx, sexp_type_name_by_index(ctx, _UWORD0), _ARG1));
    _ARG1 = sexp_slot_ref(_ARG1, _UWORD1);
    ip += sizeof(sexp)*2;
    break;
  case SEXP_OP_SLOT_SET:
    _ALIGN_IP();
    if (! sexp_check_type(ctx, _ARG1, sexp_type_by_index(ctx, _UWORD0)))
      sexp_raise("slot-set!: bad type", sexp_list2(ctx, sexp_type_name_by_index(ctx, _UWORD0), _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("slot-set!: immutable object", sexp_list1(ctx, _ARG1));
    sexp_slot_set(_ARG1, _UWORD1, _ARG2);
    ip += sizeof(sexp)*2;
    top-=2;
    break;
  case SEXP_OP_SLOTN_REF:
    if (! sexp_typep(_ARG1))
      sexp_raise("slotn-ref: not a record type", sexp_list1(ctx, _ARG1));
    else if (! sexp_check_type(ctx, _ARG2, _ARG1))
      sexp_raise("slotn-ref: bad type", sexp_list1(ctx, _ARG2));
    else if (! sexp_fixnump(_ARG3))
      sexp_raise("slotn-ref: not an integer", sexp_list1(ctx, _ARG3));
    _ARG3 = sexp_slot_ref(_ARG2, sexp_unbox_fixnum(_ARG3));
    top-=2;
    if (!_ARG1) _ARG1 = SEXP_VOID;
    break;
  case SEXP_OP_SLOTN_SET:
    if (! sexp_typep(_ARG1))
      sexp_raise("slotn-set!: not a record type", sexp_list1(ctx, _ARG1));
    else if (! sexp_check_type(ctx, _ARG2, _ARG1))
      sexp_raise("slotn-set!: bad type", sexp_list1(ctx, _ARG2));
    else if (sexp_immutablep(_ARG2))
      sexp_raise("slotn-set!: immutable object", sexp_list1(ctx, _ARG2));
    else if (! sexp_fixnump(_ARG3))
      sexp_raise("slotn-set!: not an integer", sexp_list1(ctx, _ARG3));
    sexp_slot_set(_ARG2, sexp_unbox_fixnum(_ARG3), _ARG4);
    top-=4;
    break;
  case SEXP_OP_CAR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("car: not a pair", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_car(_ARG1); break;
  case SEXP_OP_CDR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("cdr: not a pair", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_cdr(_ARG1); break;
  case SEXP_OP_SET_CAR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-car!: not a pair", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("set-car!: immutable pair", sexp_list1(ctx, _ARG1));
    sexp_car(_ARG1) = _ARG2;
    top-=2;
    break;
  case SEXP_OP_SET_CDR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-cdr!: not a pair", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("set-cdr!: immutable pair", sexp_list1(ctx, _ARG1));
    sexp_cdr(_ARG1) = _ARG2;
    top-=2;
    break;
  case SEXP_OP_CONS:
    sexp_context_top(ctx) = top;
    _ARG2 = sexp_cons(ctx, _ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_ADD:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
#if SEXP_USE_BIGNUMS
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      j = sexp_unbox_fixnum(tmp1) + sexp_unbox_fixnum(tmp2);
      if ((j < SEXP_MIN_FIXNUM) || (j > SEXP_MAX_FIXNUM))
        _ARG1 = sexp_add(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG1 = sexp_make_fixnum(j);
    }
    else {
      _ARG1 = sexp_add(ctx, tmp1, tmp2);
      sexp_check_exception();
    }
#else
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2))
      _ARG1 = sexp_fx_add(tmp1, tmp2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(tmp1) && sexp_flonump(tmp2))
      _ARG1 = sexp_fp_add(ctx, tmp1, tmp2);
    else if (sexp_flonump(tmp1) && sexp_fixnump(tmp2))
      _ARG1 = sexp_make_flonum(ctx, sexp_flonum_value(tmp1) + (double)sexp_unbox_fixnum(tmp2));
    else if (sexp_fixnump(tmp1) && sexp_flonump(tmp2))
      _ARG1 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(tmp1) + sexp_flonum_value(tmp2));
#endif
    else sexp_raise("+: not a number", sexp_list2(ctx, tmp1, tmp2));
#endif
    break;
  case SEXP_OP_SUB:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
#if SEXP_USE_BIGNUMS
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      j = sexp_unbox_fixnum(tmp1) - sexp_unbox_fixnum(tmp2);
      if ((j < SEXP_MIN_FIXNUM) || (j > SEXP_MAX_FIXNUM))
        _ARG1 = sexp_sub(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG1 = sexp_make_fixnum(j);
    }
    else {
      _ARG1 = sexp_sub(ctx, tmp1, tmp2);
      sexp_check_exception();
    }
#else
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2))
      _ARG1 = sexp_fx_sub(tmp1, tmp2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(tmp1) && sexp_flonump(tmp2))
      _ARG1 = sexp_fp_sub(ctx, tmp1, tmp2);
    else if (sexp_flonump(tmp1) && sexp_fixnump(tmp2))
      _ARG1 = sexp_make_flonum(ctx, sexp_flonum_value(tmp1) - (double)sexp_unbox_fixnum(tmp2));
    else if (sexp_fixnump(tmp1) && sexp_flonump(tmp2))
      _ARG1 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(tmp1) - sexp_flonum_value(tmp2));
#endif
    else sexp_raise("-: not a number", sexp_list2(ctx, tmp1, tmp2));
#endif
    break;
  case SEXP_OP_MUL:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
#if SEXP_USE_BIGNUMS
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      prod = (sexp_lsint_t)sexp_unbox_fixnum(tmp1) * sexp_unbox_fixnum(tmp2);
      if ((prod < SEXP_MIN_FIXNUM) || (prod > SEXP_MAX_FIXNUM))
        _ARG1 = sexp_mul(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG1 = sexp_make_fixnum(prod);
    }
    else {
      _ARG1 = sexp_mul(ctx, tmp1, tmp2);
      sexp_check_exception();
    }
#else
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2))
      _ARG1 = sexp_fx_mul(tmp1, tmp2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(tmp1) && sexp_flonump(tmp2))
      _ARG1 = sexp_fp_mul(ctx, tmp1, tmp2);
    else if (sexp_flonump(tmp1) && sexp_fixnump(tmp2))
      _ARG1 = sexp_make_flonum(ctx, sexp_flonum_value(tmp1) * (double)sexp_unbox_fixnum(tmp2));
    else if (sexp_fixnump(tmp1) && sexp_flonump(tmp2))
      _ARG1 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(tmp1) * sexp_flonum_value(tmp2));
#endif
    else sexp_raise("*: not a number", sexp_list2(ctx, tmp1, tmp2));
#endif
    break;
  case SEXP_OP_DIV:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
    if (tmp2 == SEXP_ZERO) {
#if SEXP_USE_FLONUMS
      if (sexp_flonump(tmp1) && sexp_flonum_value(tmp1) == 0.0)
        _ARG1 = sexp_make_flonum(ctx, 0.0);
      else
#endif
        sexp_raise("divide by zero", SEXP_NULL);
    } else if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
#if SEXP_USE_RATIOS
      _ARG1 = sexp_make_ratio(ctx, tmp1, tmp2);
      _ARG1 = sexp_ratio_normalize(ctx, _ARG1, SEXP_FALSE);
#else
#if SEXP_USE_FLONUMS
      tmp1 = sexp_fixnum_to_flonum(ctx, tmp1);
      tmp2 = sexp_fixnum_to_flonum(ctx, tmp2);
      _ARG1 = sexp_fp_div(ctx, tmp1, tmp2);
      if (sexp_flonum_value(_ARG1) == trunc(sexp_flonum_value(_ARG1)))
        _ARG1 = sexp_make_fixnum(sexp_flonum_value(_ARG1));
#else
      _ARG1 = sexp_fx_div(tmp1, tmp2);
#endif
#endif
    }
#if SEXP_USE_BIGNUMS
    else {
      _ARG1 = sexp_div(ctx, tmp1, tmp2);
      sexp_check_exception();
    }
#else
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(tmp1) && sexp_flonump(tmp2))
      _ARG1 = sexp_fp_div(ctx, tmp1, tmp2);
    else if (sexp_flonump(tmp1) && sexp_fixnump(tmp2))
      _ARG1 = sexp_make_flonum(ctx, sexp_flonum_value(tmp1) / (double)sexp_unbox_fixnum(tmp2));
    else if (sexp_fixnump(tmp1) && sexp_flonump(tmp2))
      _ARG1 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(tmp1) / sexp_flonum_value(tmp2));
#endif
    else sexp_raise("/: not a number", sexp_list2(ctx, tmp1, tmp2));
#endif
    break;
  case SEXP_OP_QUOTIENT:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      if (tmp2 == SEXP_ZERO)
        sexp_raise("divide by zero", SEXP_NULL);
      _ARG1 = sexp_fx_div(tmp1, tmp2);
    }
#if SEXP_USE_BIGNUMS
    else {
      _ARG1 = sexp_quotient(ctx, tmp1, tmp2);
      sexp_check_exception();
    }
#else
    else sexp_raise("quotient: not an integer", sexp_list2(ctx, _ARG1, tmp2));
#endif
    break;
  case SEXP_OP_REMAINDER:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      if (tmp2 == SEXP_ZERO)
        sexp_raise("divide by zero", SEXP_NULL);
      _ARG1 = sexp_fx_rem(tmp1, tmp2);
    }
#if SEXP_USE_BIGNUMS
    else {
      _ARG1 = sexp_remainder(ctx, tmp1, tmp2);
      sexp_check_exception();
    }
#else
    else sexp_raise("remainder: not an integer", sexp_list2(ctx, _ARG1, tmp2));
#endif
    break;
  case SEXP_OP_LT:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      i = (sexp_sint_t)tmp1 < (sexp_sint_t)tmp2;
#if SEXP_USE_BIGNUMS
      _ARG1 = sexp_make_boolean(i);
    } else {
      _ARG1 = sexp_compare(ctx, tmp1, tmp2);
      if (sexp_exceptionp(_ARG1)) {
        if (strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(_ARG1))) == 0)
          _ARG1 = SEXP_FALSE;
        else
          goto call_error_handler;
      } else {
        _ARG1 = sexp_make_boolean(sexp_unbox_fixnum(_ARG1) < 0);
      }
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(tmp1) && sexp_flonump(tmp2)) {
      i = sexp_flonum_value(tmp1) < sexp_flonum_value(tmp2);
    } else if (sexp_flonump(tmp1) && sexp_fixnump(tmp2)) {
      i = sexp_flonum_value(tmp1) < (double)sexp_unbox_fixnum(tmp2); 
    } else if (sexp_fixnump(tmp1) && sexp_flonump(tmp2)) {
      i = (double)sexp_unbox_fixnum(tmp1) < sexp_flonum_value(tmp2);
#endif
    } else sexp_raise("<: not a number", sexp_list2(ctx, tmp1, tmp2));
    _ARG1 = sexp_make_boolean(i);
#endif
    break;
  case SEXP_OP_LE:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      i = (sexp_sint_t)tmp1 <= (sexp_sint_t)tmp2;
#if SEXP_USE_BIGNUMS
      _ARG1 = sexp_make_boolean(i);
    } else {
      _ARG1 = sexp_compare(ctx, tmp1, tmp2);
      if (sexp_exceptionp(_ARG1)) {
        if (strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(_ARG1))) == 0)
          _ARG1 = SEXP_FALSE;
        else
          goto call_error_handler;
      } else {
        _ARG1 = sexp_make_boolean(sexp_unbox_fixnum(_ARG1) <= 0);
      }
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(tmp1) && sexp_flonump(tmp2)) {
      i = sexp_flonum_value(tmp1) <= sexp_flonum_value(tmp2);
    } else if (sexp_flonump(tmp1) && sexp_fixnump(tmp2)) {
      i = sexp_flonum_value(tmp1) <= (double)sexp_unbox_fixnum(tmp2);
    } else if (sexp_fixnump(tmp1) && sexp_flonump(tmp2)) {
      i = (double)sexp_unbox_fixnum(tmp1) <= sexp_flonum_value(tmp2);
#endif
    } else sexp_raise("<=: not a number", sexp_list2(ctx, tmp1, tmp2));
    _ARG1 = sexp_make_boolean(i);
#endif
    break;
  case SEXP_OP_EQN:
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = --top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      i = tmp1 == tmp2;
#if SEXP_USE_BIGNUMS
      _ARG1 = sexp_make_boolean(i);
    } else {
#if SEXP_USE_COMPLEX
      if (sexp_complexp(tmp1)) {
        if (sexp_flonump(sexp_complex_imag(tmp1))
            && sexp_flonum_value(sexp_complex_imag(tmp1)) == 0.0) {
          tmp1 = sexp_complex_real(tmp1);
        } else if (sexp_complexp(tmp2)) { /* both complex */
          _ARG1 = sexp_make_boolean(
            (sexp_compare(ctx, sexp_complex_real(tmp1), sexp_complex_real(tmp2))
             == SEXP_ZERO)
            && (sexp_compare(ctx, sexp_complex_imag(tmp1), sexp_complex_imag(tmp2))
                == SEXP_ZERO));
          break;
        } else if (sexp_numberp(tmp2)) {
          _ARG1 = SEXP_FALSE;
          break;
        }
      }
      if (sexp_complexp(tmp2)) {
        if (sexp_flonump(sexp_complex_imag(tmp2))
            && sexp_flonum_value(sexp_complex_imag(tmp2)) == 0.0) {
          tmp2 = sexp_complex_real(tmp2);
        } else if (sexp_numberp(tmp1)) {
          _ARG1 = SEXP_FALSE;
          break;
        }
      }
#endif
      /* neither is complex */
      _ARG1 = sexp_compare(ctx, tmp1, tmp2);
      if (sexp_exceptionp(_ARG1)) {
        if (strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(_ARG1))) == 0)
          _ARG1 = SEXP_FALSE;
        else
          goto call_error_handler;
      } else {
        _ARG1 = sexp_make_boolean(_ARG1 == SEXP_ZERO);
      }
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(tmp1) && sexp_flonump(tmp2)) {
      i = sexp_flonum_value(tmp1) == sexp_flonum_value(tmp2);
    } else if (sexp_flonump(tmp1) && sexp_fixnump(tmp2)) {
      i = sexp_flonum_value(tmp1) == (double)sexp_unbox_fixnum(tmp2);
    } else if (sexp_fixnump(tmp1) && sexp_flonump(tmp2)) {
      i = (double)sexp_unbox_fixnum(tmp1) == sexp_flonum_value(tmp2);
#endif
    } else sexp_raise("=: not a number", sexp_list2(ctx, tmp1, tmp2));
    _ARG1 = sexp_make_boolean(i);
#endif
    break;
  case SEXP_OP_EQ:
    _ARG2 = sexp_make_boolean(_ARG1 == _ARG2);
    top--;
    break;
  case SEXP_OP_FIX2FLO:
#if SEXP_USE_FLONUMS
    sexp_context_top(ctx) = top;
    if (sexp_fixnump(_ARG1))
      _ARG1 = sexp_fixnum_to_flonum(ctx, _ARG1);
#if SEXP_USE_BIGNUMS
    else if (sexp_bignump(_ARG1))
      _ARG1 = sexp_make_flonum(ctx, sexp_bignum_to_double(_ARG1));
#endif
#if SEXP_USE_RATIOS
    else if (sexp_ratiop(_ARG1))
      _ARG1 = sexp_make_flonum(ctx, sexp_ratio_to_double(_ARG1));
#endif
    else if (! sexp_flonump(_ARG1))
      sexp_raise("inexact: not a number", sexp_list1(ctx, _ARG1));
#endif
    break;
  case SEXP_OP_FLO2FIX:
#if SEXP_USE_FLONUMS
    if (sexp_flonump(_ARG1)) {
      if (isinf(sexp_flonum_value(_ARG1)) || isnan(sexp_flonum_value(_ARG1))) {
        sexp_raise("exact: not an finite number", sexp_list1(ctx, _ARG1));
      } else if (sexp_flonum_value(_ARG1) != trunc(sexp_flonum_value(_ARG1))) {
#if SEXP_USE_RATIOS
        _ARG1 = sexp_double_to_ratio(ctx, sexp_flonum_value(_ARG1));
#else
        sexp_raise("exact: not an integer", sexp_list1(ctx, _ARG1));
#endif
#if SEXP_USE_BIGNUMS
      } else if ((sexp_flonum_value(_ARG1) > SEXP_MAX_FIXNUM)
                 || sexp_flonum_value(_ARG1) < SEXP_MIN_FIXNUM) {
        sexp_context_top(ctx) = top;
        _ARG1 = sexp_double_to_bignum(ctx, sexp_flonum_value(_ARG1));
#endif
      } else {
        _ARG1 = sexp_make_fixnum((sexp_sint_t)sexp_flonum_value(_ARG1));
      }
    } else if (!sexp_exactp(_ARG1)) {
      sexp_raise("exact: not a number", sexp_list1(ctx, _ARG1));
    }
#endif
    break;
  case SEXP_OP_CHAR2INT:
    if (! sexp_charp(_ARG1))
      sexp_raise("char->integer: not a character", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_unbox_character(_ARG1));
    break;
  case SEXP_OP_INT2CHAR:
    if (! sexp_fixnump(_ARG1))
      sexp_raise("integer->char: not an integer", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_character(sexp_unbox_fixnum(_ARG1));
    break;
  case SEXP_OP_CHAR_UPCASE:
    if (! sexp_charp(_ARG1))
      sexp_raise("char-upcase: not a character", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_character(sexp_toupper(sexp_unbox_character(_ARG1)));
    break;
  case SEXP_OP_CHAR_DOWNCASE:
    if (! sexp_charp(_ARG1))
      sexp_raise("char-downcase: not a character", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_character(sexp_tolower(sexp_unbox_character(_ARG1)));
    break;
  case SEXP_OP_WRITE_CHAR:
    if (! sexp_charp(_ARG1))
      sexp_raise("write-char: not a character", sexp_list1(ctx, _ARG1));
    if (! sexp_oportp(_ARG2))
      sexp_raise("write-char: not an output-port", sexp_list1(ctx, _ARG2));
    sexp_context_top(ctx) = top;
#if SEXP_USE_GREEN_THREADS
    errno = 0;
#endif
#if SEXP_USE_UTF8_STRINGS
    if (sexp_unbox_character(_ARG1) >= 0x80)
      i = sexp_write_utf8_char(ctx, sexp_unbox_character(_ARG1), _ARG2);
    else
#endif
    i = sexp_write_char(ctx, sexp_unbox_character(_ARG1), _ARG2);
    if (i == EOF) {
#if SEXP_USE_GREEN_THREADS
      if ((sexp_port_stream(_ARG2) ? ferror(sexp_port_stream(_ARG2)) : 1)
          && (errno == EAGAIN)) {
        if (sexp_port_stream(_ARG2)) clearerr(sexp_port_stream(_ARG2));
        if (sexp_applicablep(sexp_global(ctx, SEXP_G_THREADS_BLOCKER)))
          sexp_apply1(ctx, sexp_global(ctx, SEXP_G_THREADS_BLOCKER), _ARG2);
        fuel = 0;
        ip--;      /* try again */
        goto loop;
      } else
#endif
      sexp_raise("failed to write char to port", _ARG2);
    }
    top--;
    _ARG1 = SEXP_VOID;
    break;
  case SEXP_OP_WRITE_STRING:
    if (sexp_stringp(_ARG1))
#if SEXP_USE_PACKED_STRINGS
      tmp1 = _ARG1;
#else
      tmp1 = sexp_string_bytes(_ARG1);
#endif
    else if (sexp_bytesp(_ARG1))
      tmp1 = _ARG1;
    else
      sexp_raise("write-string: not a string or bytes", sexp_list1(ctx, _ARG1));
    if (! sexp_fixnump(_ARG2)) {
      if (_ARG2 == SEXP_TRUE)
        _ARG2 = sexp_make_fixnum(sexp_bytes_length(tmp1));
      else
        sexp_raise("write-string: not an integer", sexp_list1(ctx, _ARG2));
    }
    if (sexp_unbox_fixnum(_ARG2) < 0 || sexp_unbox_fixnum(_ARG2) > sexp_bytes_length(tmp1))
      sexp_raise("write-string: not a valid string count", sexp_list2(ctx, tmp1, _ARG2));
    if (! sexp_oportp(_ARG3))
      sexp_raise("write-string: not an output-port", sexp_list1(ctx, _ARG3));
    sexp_context_top(ctx) = top;
#if SEXP_USE_GREEN_THREADS
    errno = 0;
#endif
    i = sexp_write_string_n(ctx, sexp_bytes_data(tmp1), sexp_unbox_fixnum(_ARG2), _ARG3);
#if SEXP_USE_GREEN_THREADS
    if (i < sexp_unbox_fixnum(_ARG2) && errno == EAGAIN) {
      if (sexp_port_stream(_ARG3)) clearerr(sexp_port_stream(_ARG3));
      /* modify stack in-place so we continue where we left off next time */
      if (i > 0) {
        if (sexp_stringp(_ARG1))
          _ARG1 = sexp_substring(ctx, _ARG1, sexp_make_fixnum(i), SEXP_FALSE);
        else
          _ARG1 = sexp_subbytes(ctx, _ARG1, sexp_make_fixnum(i), SEXP_FALSE);
        _ARG2 = sexp_make_fixnum(sexp_unbox_fixnum(_ARG2) - i);
      }
      /* yield if threads are enabled (otherwise busy loop) */
      /* TODO: the wait seems necessary on OS X to stop a print loop to ptys */
      if (sexp_applicablep(sexp_global(ctx, SEXP_G_THREADS_BLOCKER)))
        sexp_apply1(ctx, sexp_global(ctx, SEXP_G_THREADS_BLOCKER), _ARG3);
      else               /* no scheduler but output full, wait 5ms */
        usleep(5*1000);
      fuel = 0;
      ip--;      /* try again */
      goto loop;
    }
#endif
    tmp1 = sexp_make_fixnum(i);     /* return the number of bytes written */
    top-=2;
    _ARG1 = tmp1;
    break;
  case SEXP_OP_READ_CHAR:
    if (! sexp_iportp(_ARG1))
      sexp_raise("read-char: not an input-port", sexp_list1(ctx, _ARG1));
    sexp_context_top(ctx) = top;
#if SEXP_USE_GREEN_THREADS
    errno = 0;
#endif
    i = sexp_read_char(ctx, _ARG1);
#if SEXP_USE_UTF8_STRINGS
    if (i >= 0x80)
      _ARG1 = sexp_read_utf8_char(ctx, _ARG1, i);
    else
#endif
    if (i == EOF) {
#if SEXP_USE_GREEN_THREADS
      if ((sexp_port_stream(_ARG1) ? ferror(sexp_port_stream(_ARG1)) : 1)
          && (errno == EAGAIN)) {
        if (sexp_port_stream(_ARG1)) clearerr(sexp_port_stream(_ARG1));
        /* TODO: block and unblock */
        if (sexp_applicablep(sexp_global(ctx, SEXP_G_THREADS_BLOCKER)))
          sexp_apply1(ctx, sexp_global(ctx, SEXP_G_THREADS_BLOCKER), _ARG1);
        fuel = 0;
        ip--;      /* try again */
      } else
#endif
        _ARG1 = SEXP_EOF;
    } else {
      if (i == '\n') sexp_port_line(_ARG1)++;
      _ARG1 = sexp_make_character(i);
    }
    sexp_check_exception();
    break;
  case SEXP_OP_PEEK_CHAR:
    if (! sexp_iportp(_ARG1))
      sexp_raise("peek-char: not an input-port", sexp_list1(ctx, _ARG1));
    sexp_context_top(ctx) = top;
#if SEXP_USE_GREEN_THREADS
    errno = 0;
#endif
    i = sexp_read_char(ctx, _ARG1);
    if (i == EOF) {
#if SEXP_USE_GREEN_THREADS
      if ((sexp_port_stream(_ARG1) ? ferror(sexp_port_stream(_ARG1)) : 1)
          && (errno == EAGAIN)) {
        if (sexp_port_stream(_ARG1)) clearerr(sexp_port_stream(_ARG1));
        if (sexp_applicablep(sexp_global(ctx, SEXP_G_THREADS_BLOCKER)))
          sexp_apply1(ctx, sexp_global(ctx, SEXP_G_THREADS_BLOCKER), _ARG1);
        fuel = 0;
        ip--;      /* try again */
      } else
#endif
        _ARG1 = SEXP_EOF;
    } else {
      sexp_push_char(ctx, i, _ARG1);
      _ARG1 = sexp_make_character(i);
    }
    sexp_check_exception();
    break;
  case SEXP_OP_YIELD:
#if SEXP_USE_GREEN_THREADS
    fuel = 0;
#endif
    break;
  case SEXP_OP_FORCE:
#if SEXP_USE_AUTO_FORCE
    sexp_context_top(ctx) = top;
    while (sexp_promisep(_ARG1)) {
      if (sexp_promise_donep(_ARG1)) {
        _ARG1 = sexp_promise_value(_ARG1);
      } else {
        sexp_context_top(ctx) = top;
        tmp1 = sexp_apply(ctx, sexp_promise_value(_ARG1), SEXP_NULL);
        if (!sexp_promise_donep(_ARG1)) {
          sexp_promise_value(_ARG1) = tmp1;
          sexp_promise_donep(_ARG1) = 1;
        }
        _ARG1 = tmp1;
      }
    }
#endif
    break;
  case SEXP_OP_RET:
    i = sexp_unbox_fixnum(stack[fp]);
    stack[fp-i] = _ARG1;
    top = fp-i+1;
    self = stack[fp+2];
    bc = sexp_procedure_code(self);
    ip = sexp_bytecode_data(bc) + sexp_unbox_fixnum(stack[fp+1]);
    cp = sexp_procedure_vars(self);
    fp = sexp_unbox_fixnum(stack[fp+3]);
    break;
  case SEXP_OP_DONE:
    sexp_context_last_fp(ctx) = fp;
    goto end_loop;
  default:
    sexp_raise("unknown opcode", sexp_list1(ctx, sexp_make_fixnum(*(ip-1))));
  }
#if SEXP_USE_DEBUG_VM
  if (sexp_context_tracep(ctx))
    fprintf(stderr, "****** VM => %p (%d)\n", _ARG1,
            sexp_pointerp(_ARG1) && sexp_in_heap_p(ctx, _ARG1)
            ? sexp_pointer_tag(_ARG1) : -1);
#endif
  goto loop;

 end_loop:
#if SEXP_USE_GREEN_THREADS
  sexp_context_result(ctx) = _ARG1;
  if (ctx != root_thread) {
    if (sexp_context_refuel(root_thread) <= 0) {
      /* the root already terminated */
      _ARG1 = sexp_context_result(root_thread);
    } else {
      /* don't return from child threads */
      if (sexp_exceptionp(_ARG1)) {
        tmp1 = sexp_current_error_port(ctx);
        sexp_write_string(ctx, "ERROR in child thread: ", tmp1);
        sexp_write(ctx, ctx, tmp1);
        sexp_newline(ctx, tmp1);
        sexp_print_exception(ctx, _ARG1, tmp1);
      }
#if SEXP_USE_DEBUG_THREADS
      fprintf(stderr, "****** schedule %p: terminating %p (%s)\n",
              root_thread, ctx, sexp_thread_debug_name(ctx));
#endif
      sexp_context_refuel(ctx) = fuel = 0;
      goto loop;
    }
  }
#endif
  sexp_gc_release3(ctx);
  tmp1 = _ARG1;
  sexp_context_top(ctx) = --top;
  return tmp1;
}

sexp sexp_apply1 (sexp ctx, sexp f, sexp x) {
  sexp res;
  sexp_gc_var1(args);
  if (sexp_opcodep(f) && sexp_opcode_func(f)) {
    res = ((sexp_proc2)sexp_opcode_func(f))(ctx, f, 1, x);
  } else {
    sexp_gc_preserve1(ctx, args);
    res = sexp_apply(ctx, f, args=sexp_list1(ctx, x));
    sexp_gc_release1(ctx);
  }
  return res;
}

#endif
