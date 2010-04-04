/*  vm.c -- stack-based virtual machine backend               */
/*  Copyright (c) 2009-2010 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

/*********************** the virtual machine **************************/

static sexp sexp_save_stack (sexp ctx, sexp *stack, sexp_uint_t to) {
  sexp res, *data;
  sexp_uint_t i;
  res = sexp_make_vector(ctx, sexp_make_fixnum(to), SEXP_VOID);
  data = sexp_vector_data(res);
  for (i=0; i<to; i++)
    data[i] = stack[i];
  return res;
}

static sexp_uint_t sexp_restore_stack (sexp saved, sexp *current) {
  sexp_uint_t len = sexp_vector_length(saved), i;
  sexp *from = sexp_vector_data(saved);
  for (i=0; i<len; i++)
    current[i] = from[i];
  return len;
}

#define _ARG1 stack[top-1]
#define _ARG2 stack[top-2]
#define _ARG3 stack[top-3]
#define _ARG4 stack[top-4]
#define _ARG5 stack[top-5]
#define _ARG6 stack[top-6]
#define _PUSH(x) (stack[top++]=(x))

#if SEXP_USE_ALIGNED_BYTECODE
#define _ALIGN_IP() ip = (unsigned char *)sexp_word_align((unsigned long)ip)
#else
#define _ALIGN_IP()
#endif

#define _WORD0 ((sexp*)ip)[0]
#define _UWORD0 ((sexp_uint_t*)ip)[0]
#define _SWORD0 ((sexp_sint_t*)ip)[0]
#define _WORD1 ((sexp*)ip)[1]
#define _UWORD1 ((sexp_uint_t*)ip)[1]
#define _SWORD1 ((sexp_sint_t*)ip)[1]

#define sexp_raise(msg, args)                                       \
  do {sexp_context_top(ctx) = top+1;                                \
      stack[top] = args;                                            \
      stack[top] = sexp_user_exception(ctx, self, msg, stack[top]); \
      top++;                                                        \
      goto call_error_handler;}                                     \
  while (0)

#define sexp_check_exception() do {if (sexp_exceptionp(_ARG1)) \
                                     goto call_error_handler;} \
                                while (0)

sexp sexp_vm (sexp ctx, sexp proc) {
  sexp bc = sexp_procedure_code(proc), cp = sexp_procedure_vars(proc);
  sexp *stack = sexp_stack_data(sexp_context_stack(ctx));
  unsigned char *ip = sexp_bytecode_data(bc);
  sexp_sint_t i, j, k, fp, top = sexp_stack_top(sexp_context_stack(ctx));
#if SEXP_USE_BIGNUMS
  sexp_lsint_t prod;
#endif
  sexp_gc_var3(self, tmp1, tmp2);
  sexp_gc_preserve3(ctx, self, tmp1, tmp2);
  fp = top - 4;
  self = proc;

 loop:
#if SEXP_USE_DEBUG_VM
  if (sexp_context_tracep(ctx)) {
    sexp_print_stack(ctx, stack, top, fp, SEXP_FALSE);
    fprintf(stderr, "%s\n", (*ip<=SEXP_OP_NUM_OPCODES) ?
            reverse_opcode_names[*ip] : "UNKNOWN");
  }
#endif
  switch (*ip++) {
  case SEXP_OP_NOOP:
    break;
  case SEXP_OP_RAISE:
  call_error_handler:
    tmp1 = sexp_cdr(sexp_global(ctx, SEXP_G_ERR_HANDLER));
    if (! sexp_procedurep(tmp1)) goto end_loop;
    stack[top] = (sexp) 1;
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
    tmp1 = stack[fp-1];
    top = sexp_restore_stack(sexp_vector_ref(cp, 0), stack);
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
    i = sexp_unbox_fixnum(sexp_length(ctx, tmp2));
    top += (i-2);
    for ( ; sexp_pairp(tmp2); tmp2=sexp_cdr(tmp2), top--)
      _ARG1 = sexp_car(tmp2);
    top += i+1;
    ip -= sizeof(sexp);
    goto make_call;
  case SEXP_OP_TAIL_CALL:
    _ALIGN_IP();
    i = sexp_unbox_fixnum(_WORD0);    /* number of params */
    tmp1 = _ARG1;                              /* procedure to call */
    /* save frame info */
    tmp2 = stack[fp+3];
    j = sexp_unbox_fixnum(stack[fp]);
    self = stack[fp+2];
    bc = sexp_procedure_vars(self);
    cp = sexp_procedure_vars(self);
    ip = (sexp_bytecode_data(bc)
          + sexp_unbox_fixnum(stack[fp+1])) - sizeof(sexp);
    /* copy new args into place */
    for (k=0; k<i; k++)
      stack[fp-j+k] = stack[top-1-i+k];
    top = fp+i-j+1;
    fp = sexp_unbox_fixnum(tmp2);
    goto make_call;
  case SEXP_OP_CALL:
#if SEXP_USE_CHECK_STACK
    if (top+16 >= SEXP_INIT_STACK_SIZE) {
      _ARG1 = sexp_global(ctx, SEXP_G_OOS_ERROR);
      goto end_loop;
    }
#endif
    _ALIGN_IP();
    i = sexp_unbox_fixnum(_WORD0);
    tmp1 = _ARG1;
  make_call:
    if (sexp_opcodep(tmp1)) {
      /* compile non-inlined opcode applications on the fly */
      sexp_context_top(ctx) = top;
      tmp1 = make_opcode_procedure(ctx, tmp1, i);
      if (sexp_exceptionp(tmp1)) {
        _ARG1 = tmp1;
        goto call_error_handler;
      }
    }
    if (! sexp_procedurep(tmp1))
      sexp_raise("non procedure application", sexp_list1(ctx, tmp1));
    j = i - sexp_unbox_fixnum(sexp_procedure_num_args(tmp1));
    if (j < 0)
      sexp_raise("not enough args",
                 sexp_list2(ctx, tmp1, sexp_make_fixnum(i)));
    if (j > 0) {
      if (sexp_procedure_variadic_p(tmp1)) {
        stack[top-i-1] = sexp_cons(ctx, stack[top-i-1], SEXP_NULL);
        for (k=top-i; k<top-(i-j)-1; k++)
          stack[top-i-1] = sexp_cons(ctx, stack[k], stack[top-i-1]);
        for ( ; k<top; k++)
          stack[k-j+1] = stack[k];
        top -= (j-1);
        i -= (j-1);
      } else {
        sexp_raise("too many args",
                   sexp_list2(ctx, tmp1, sexp_make_fixnum(i)));
      }
    } else if (sexp_procedure_variadic_p(tmp1)) {
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
    _PUSH(((sexp_proc1)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 0)));
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL1:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG1 = ((sexp_proc2)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 1), _ARG1);
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL2:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG2 = ((sexp_proc3)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 2), _ARG1, _ARG2);
    top--;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL3:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG3 = ((sexp_proc4)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 3), _ARG1, _ARG2, _ARG3);
    top -= 2;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL4:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG4 = ((sexp_proc5)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 4), _ARG1, _ARG2, _ARG3, _ARG4);
    top -= 3;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL5:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG5 = ((sexp_proc6)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 5), _ARG1, _ARG2, _ARG3, _ARG4, _ARG5);
    top -= 4;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
  case SEXP_OP_FCALL6:
    _ALIGN_IP();
    sexp_context_top(ctx) = top;
    _ARG6 = ((sexp_proc7)sexp_opcode_func(_WORD0))(ctx sexp_api_pass(_WORD0, 6), _ARG1, _ARG2, _ARG3, _ARG4, _ARG5, _ARG6);
    top -= 5;
    ip += sizeof(sexp);
    sexp_check_exception();
    break;
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
  case SEXP_OP_STACK_REF:            /* `pick' in forth */
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
    stack[fp - 1 - _SWORD0] = _ARG1;
    _ARG1 = SEXP_VOID;
    ip += sizeof(sexp);
    break;
  case SEXP_OP_CLOSURE_REF:
    _ALIGN_IP();
    _PUSH(sexp_vector_ref(cp, sexp_make_fixnum(_WORD0)));
    ip += sizeof(sexp);
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
    _ARG3 = SEXP_VOID;
    top-=2;
    break;
  case SEXP_OP_VECTOR_LENGTH:
    if (! sexp_vectorp(_ARG1))
      sexp_raise("vector-length: not a vector", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_vector_length(_ARG1));
    break;
  case SEXP_OP_STRING_REF:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-ref: not a string", sexp_list1(ctx, _ARG1));
    else if (! sexp_fixnump(_ARG2))
      sexp_raise("string-ref: not an integer", sexp_list1(ctx, _ARG2));
    i = sexp_unbox_fixnum(_ARG2);
    if ((i < 0) || (i >= sexp_string_length(_ARG1)))
      sexp_raise("string-ref: index out of range", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_string_ref(_ARG1, _ARG2);
    top--;
    break;
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
    sexp_string_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_VOID;
    top-=2;
    break;
  case SEXP_OP_STRING_LENGTH:
    if (! sexp_stringp(_ARG1))
      sexp_raise("string-length: not a string", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_fixnum(sexp_string_length(_ARG1));
    break;
  case SEXP_OP_MAKE_PROCEDURE:
    sexp_context_top(ctx) = top;
    _ARG4 = sexp_make_procedure(ctx, _ARG1, _ARG2, _ARG3, _ARG4);
    top-=3;
    break;
  case SEXP_OP_MAKE_VECTOR:
    sexp_context_top(ctx) = top;
    if (! sexp_fixnump(_ARG1))
      sexp_raise("make-vector: not an integer", sexp_list1(ctx, _ARG1));
    _ARG2 = sexp_make_vector(ctx, _ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_MAKE_EXCEPTION:
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
  case SEXP_OP_TYPEP:
    _ALIGN_IP();
    _ARG1 = sexp_make_boolean(sexp_check_tag(_ARG1, _UWORD0));
    ip += sizeof(sexp);
    break;
  case SEXP_OP_MAKE:
    _ALIGN_IP();
    _PUSH(sexp_alloc_tagged(ctx, _UWORD1, _UWORD0));
    ip += sizeof(sexp)*2;
    break;
  case SEXP_OP_SLOT_REF:
    _ALIGN_IP();
    if (! sexp_check_tag(_ARG1, _UWORD0))
      sexp_raise("slot-ref: bad type", sexp_list2(ctx, sexp_c_string(ctx, sexp_type_name_by_index(ctx, _UWORD0), -1), _ARG1));
    _ARG1 = sexp_slot_ref(_ARG1, _UWORD1);
    ip += sizeof(sexp)*2;
    break;
  case SEXP_OP_SLOT_SET:
    _ALIGN_IP();
    if (! sexp_check_tag(_ARG1, _UWORD0))
      sexp_raise("slot-set!: bad type", sexp_list2(ctx, sexp_c_string(ctx, sexp_type_name_by_index(ctx, _UWORD0), -1), _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("slot-set!: immutable object", sexp_list1(ctx, _ARG1));
    sexp_slot_set(_ARG1, _UWORD1, _ARG2);
    _ARG2 = SEXP_VOID;
    ip += sizeof(sexp)*2;
    top--;
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
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case SEXP_OP_SET_CDR:
    if (! sexp_pairp(_ARG1))
      sexp_raise("set-cdr!: not a pair", sexp_list1(ctx, _ARG1));
    else if (sexp_immutablep(_ARG1))
      sexp_raise("set-cdr!: immutable pair", sexp_list1(ctx, _ARG1));
    sexp_cdr(_ARG1) = _ARG2;
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case SEXP_OP_CONS:
    sexp_context_top(ctx) = top;
    _ARG2 = sexp_cons(ctx, _ARG1, _ARG2);
    top--;
    break;
  case SEXP_OP_ADD:
#if SEXP_USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      j = sexp_unbox_fixnum(tmp1) + sexp_unbox_fixnum(tmp2);
      if ((j < SEXP_MIN_FIXNUM) || (j > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_add(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_fixnum(j);
    }
    else
      _ARG2 = sexp_add(ctx, tmp1, tmp2);
#else
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_fx_add(_ARG1, _ARG2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_add(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) + (double)sexp_unbox_fixnum(_ARG2));
    else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(_ARG1) + sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("+: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case SEXP_OP_SUB:
#if SEXP_USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      j = sexp_unbox_fixnum(tmp1) - sexp_unbox_fixnum(tmp2);
      if ((j < SEXP_MIN_FIXNUM) || (j > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_sub(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_fixnum(j);
    }
    else
      _ARG2 = sexp_sub(ctx, tmp1, tmp2);
#else
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_fx_sub(_ARG1, _ARG2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_sub(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) - (double)sexp_unbox_fixnum(_ARG2));
    else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(_ARG1) - sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("-: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case SEXP_OP_MUL:
#if SEXP_USE_BIGNUMS
    tmp1 = _ARG1, tmp2 = _ARG2;
    sexp_context_top(ctx) = top;
    if (sexp_fixnump(tmp1) && sexp_fixnump(tmp2)) {
      prod = (sexp_lsint_t)sexp_unbox_fixnum(tmp1) * sexp_unbox_fixnum(tmp2);
      if ((prod < SEXP_MIN_FIXNUM) || (prod > SEXP_MAX_FIXNUM))
        _ARG2 = sexp_mul(ctx, tmp1=sexp_fixnum_to_bignum(ctx, tmp1), tmp2);
      else
        _ARG2 = sexp_make_fixnum(prod);
    }
    else
      _ARG2 = sexp_mul(ctx, tmp1, tmp2);
#else
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_fx_mul(_ARG1, _ARG2);
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_mul(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) * (double)sexp_unbox_fixnum(_ARG2));
    else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(_ARG1) * sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("*: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case SEXP_OP_DIV:
    sexp_context_top(ctx) = top;
    if (_ARG2 == SEXP_ZERO) {
#if SEXP_USE_FLONUMS
      if (sexp_flonump(_ARG1) && sexp_flonum_value(_ARG1) == 0.0)
        _ARG2 = sexp_make_flonum(ctx, 0.0/0.0);
      else
#endif
        sexp_raise("divide by zero", SEXP_NULL);
    } else if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
#if SEXP_USE_FLONUMS
      _ARG1 = sexp_fixnum_to_flonum(ctx, _ARG1);
      _ARG2 = sexp_fixnum_to_flonum(ctx, _ARG2);
      _ARG2 = sexp_fp_div(ctx, _ARG1, _ARG2);
      if (sexp_flonum_value(_ARG2) == trunc(sexp_flonum_value(_ARG2)))
        _ARG2 = sexp_make_fixnum(sexp_flonum_value(_ARG2));
#else
      _ARG2 = sexp_fx_div(_ARG1, _ARG2);
#endif
    }
#if SEXP_USE_BIGNUMS
    else
      _ARG2 = sexp_div(ctx, _ARG1, _ARG2);
#else
#if SEXP_USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_div(ctx, _ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, sexp_flonum_value(_ARG1) / (double)sexp_unbox_fixnum(_ARG2));
    else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_make_flonum(ctx, (double)sexp_unbox_fixnum(_ARG1) / sexp_flonum_value(_ARG2));
#endif
    else sexp_raise("/: not a number", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    top--;
    break;
  case SEXP_OP_QUOTIENT:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      if (_ARG2 == SEXP_ZERO)
        sexp_raise("divide by zero", SEXP_NULL);
      _ARG2 = sexp_fx_div(_ARG1, _ARG2);
      top--;
    }
#if SEXP_USE_BIGNUMS
    else {
      sexp_context_top(ctx) = top;
      _ARG2 = sexp_quotient(ctx, _ARG1, _ARG2);
      top--;
    }
#else
    else sexp_raise("quotient: not an integer", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    break;
  case SEXP_OP_REMAINDER:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      if (_ARG2 == SEXP_ZERO)
        sexp_raise("divide by zero", SEXP_NULL);
      tmp1 = sexp_fx_rem(_ARG1, _ARG2);
      top--;
      _ARG1 = tmp1;
    }
#if SEXP_USE_BIGNUMS
    else {
      sexp_context_top(ctx) = top;
      _ARG2 = sexp_remainder(ctx, _ARG1, _ARG2);
      top--;
    }
#else
    else sexp_raise("remainder: not an integer", sexp_list2(ctx, _ARG1, _ARG2));
#endif
    break;
  case SEXP_OP_LT:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = (sexp_sint_t)_ARG1 < (sexp_sint_t)_ARG2;
#if SEXP_USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_fixnump(tmp1)
        ? sexp_make_boolean(sexp_unbox_fixnum(tmp1) < 0) : tmp1;
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) < sexp_flonum_value(_ARG2);
    } else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) < (double)sexp_unbox_fixnum(_ARG2); 
    } else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2)) {
      i = (double)sexp_unbox_fixnum(_ARG1) < sexp_flonum_value(_ARG2);
#endif
    } else sexp_raise("<: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case SEXP_OP_LE:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = (sexp_sint_t)_ARG1 <= (sexp_sint_t)_ARG2;
#if SEXP_USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_fixnump(tmp1)
        ? sexp_make_boolean(sexp_unbox_fixnum(tmp1) <= 0) : tmp1;
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) <= sexp_flonum_value(_ARG2);
    } else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) <= (double)sexp_unbox_fixnum(_ARG2);
    } else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2)) {
      i = (double)sexp_unbox_fixnum(_ARG1) <= sexp_flonum_value(_ARG2);
#endif
    } else sexp_raise("<=: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case SEXP_OP_EQN:
    if (sexp_fixnump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = _ARG1 == _ARG2;
#if SEXP_USE_BIGNUMS
      _ARG2 = sexp_make_boolean(i);
    } else {
      tmp1 = sexp_compare(ctx, _ARG1, _ARG2);
      _ARG2 = sexp_fixnump(tmp1)
        ? sexp_make_boolean(sexp_unbox_fixnum(tmp1) == 0) : tmp1;
    }
#else
#if SEXP_USE_FLONUMS
    } else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) == sexp_flonum_value(_ARG2);
    } else if (sexp_flonump(_ARG1) && sexp_fixnump(_ARG2)) {
      i = sexp_flonum_value(_ARG1) == (double)sexp_unbox_fixnum(_ARG2);
    } else if (sexp_fixnump(_ARG1) && sexp_flonump(_ARG2)) {
      i = (double)sexp_unbox_fixnum(_ARG1) == sexp_flonum_value(_ARG2);
#endif
    } else sexp_raise("=: not a number", sexp_list2(ctx, _ARG1, _ARG2));
    _ARG2 = sexp_make_boolean(i);
#endif
    top--;
    break;
  case SEXP_OP_EQ:
    _ARG2 = sexp_make_boolean(_ARG1 == _ARG2);
    top--;
    break;
  case SEXP_OP_FIX2FLO:
    if (sexp_fixnump(_ARG1))
      _ARG1 = sexp_fixnum_to_flonum(ctx, _ARG1);
#if SEXP_USE_BIGNUMS
    else if (sexp_bignump(_ARG1))
      _ARG1 = sexp_make_flonum(ctx, sexp_bignum_to_double(_ARG1));
#endif
    else if (! sexp_flonump(_ARG1))
      sexp_raise("exact->inexact: not a number", sexp_list1(ctx, _ARG1));
    break;
  case SEXP_OP_FLO2FIX:
    if (sexp_flonump(_ARG1)) {
      if (sexp_flonum_value(_ARG1) != trunc(sexp_flonum_value(_ARG1))) {
        sexp_raise("inexact->exact: not an integer", sexp_list1(ctx, _ARG1));
#if SEXP_USE_BIGNUMS
      } else if ((sexp_flonum_value(_ARG1) > SEXP_MAX_FIXNUM)
                 || sexp_flonum_value(_ARG1) < SEXP_MIN_FIXNUM) {
        _ARG1 = sexp_double_to_bignum(ctx, sexp_flonum_value(_ARG1));
#endif
      } else {
        _ARG1 = sexp_make_fixnum((sexp_sint_t)sexp_flonum_value(_ARG1));
      }
    } else if (! sexp_fixnump(_ARG1) && ! sexp_bignump(_ARG1)) {
      sexp_raise("inexact->exact: not a number", sexp_list1(ctx, _ARG1));
    }
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
    _ARG1 = sexp_make_character(toupper(sexp_unbox_character(_ARG1)));
    break;
  case SEXP_OP_CHAR_DOWNCASE:
    if (! sexp_charp(_ARG1))
      sexp_raise("char-downcase: not a character", sexp_list1(ctx, _ARG1));
    _ARG1 = sexp_make_character(tolower(sexp_unbox_character(_ARG1)));
    break;
  case SEXP_OP_WRITE_CHAR:
    if (! sexp_charp(_ARG1))
      sexp_raise("write-char: not a character", sexp_list1(ctx, _ARG1));
    if (! sexp_oportp(_ARG2))
      sexp_raise("write-char: not an output-port", sexp_list1(ctx, _ARG2));
    sexp_write_char(ctx, sexp_unbox_character(_ARG1), _ARG2);
    _ARG2 = SEXP_VOID;
    top--;
    break;
  case SEXP_OP_NEWLINE:
    if (! sexp_oportp(_ARG1))
      sexp_raise("newline: not an output-port", sexp_list1(ctx, _ARG1));
    sexp_newline(ctx, _ARG1);
    _ARG1 = SEXP_VOID;
    break;
  case SEXP_OP_READ_CHAR:
    if (! sexp_iportp(_ARG1))
      sexp_raise("read-char: not an intput-port", sexp_list1(ctx, _ARG1));
    i = sexp_read_char(ctx, _ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case SEXP_OP_PEEK_CHAR:
    if (! sexp_iportp(_ARG1))
      sexp_raise("peek-char: not an intput-port", sexp_list1(ctx, _ARG1));
    i = sexp_read_char(ctx, _ARG1);
    sexp_push_char(ctx, i, _ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
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
    goto end_loop;
  default:
    sexp_raise("unknown opcode", sexp_list1(ctx, sexp_make_fixnum(*(ip-1))));
  }
  goto loop;

 end_loop:
  sexp_gc_release3(ctx);
  sexp_context_top(ctx) = top;
  return _ARG1;
}

