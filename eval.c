/*  eval.c -- evaluator library implementation           */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "eval.h"

/************************************************************************/

static int scheme_initialized_p = 0;

static sexp cur_input_port, cur_output_port, cur_error_port;
static sexp exception_handler_cell;
static sexp continuation_resumer;
static sexp interaction_environment;
static sexp the_compile_error_symbol;

#if USE_DEBUG
#include "debug.c"
#else
#define print_stack(...)
#define print_bytecode(...)
#define disasm(...)
#endif

/********************** environment utilities ***************************/

static sexp env_cell(sexp e, sexp key) {
  sexp ls;

  do {
    for (ls=sexp_env_bindings(e); sexp_pairp(ls); ls=sexp_cdr(ls))
      if (sexp_caar(ls) == key)
        return sexp_car(ls);
    e = sexp_env_parent(e);
  } while (e);

  return NULL;
}

static int env_global_p (sexp e, sexp id) {
  while (sexp_env_parent(e)) {
    if (sexp_assq(id, sexp_env_bindings(e)) != SEXP_FALSE)
      return 0;
    else
      e = sexp_env_parent(e);
  }
  return 1;
}

static void env_define(sexp e, sexp key, sexp value) {
  sexp cell = env_cell(e, key);
  if (cell) {
    sexp_cdr(cell) = value;
  } else {
    sexp_env_bindings(e)
      = sexp_cons(sexp_cons(key, value), sexp_env_bindings(e));
  }
}

static sexp extend_env_closure (sexp e, sexp fv, int offset) {
  int i;
  sexp e2 = (sexp) SEXP_ALLOC(sexp_sizeof(env));
  e2->tag = SEXP_ENV;
  sexp_env_parent(e2) = e;
  sexp_env_bindings(e2) = SEXP_NULL;
  for (i=offset; sexp_pairp(fv); fv = sexp_cdr(fv), i--)
    sexp_env_bindings(e2)
      = sexp_cons(sexp_cons(sexp_car(fv), sexp_make_integer(i)),
                  sexp_env_bindings(e2));
  return e2;
}

static int core_code (sexp e, sexp sym) {
  sexp cell = env_cell(e, sym);
  if (! cell || ! sexp_corep(sexp_cdr(cell))) return 0;
  return (sexp_core_code(sexp_cdr(cell)));
}

static sexp sexp_reverse_flatten_dot (sexp ls) {
  sexp res;
  for (res=SEXP_NULL; sexp_pairp(ls); ls=sexp_cdr(ls))
    res = sexp_cons(sexp_car(ls), res);
  return (sexp_nullp(ls) ? res : sexp_cons(ls, res));
}

static sexp sexp_flatten_dot (sexp ls) {
  return sexp_nreverse(sexp_reverse_flatten_dot(ls));
}

/************************* bytecode utilities ***************************/

static void shrink_bcode(sexp *bc, sexp_uint_t i) {
  sexp tmp;
  if (sexp_bytecode_length(*bc) != i) {
    tmp = (sexp) SEXP_ALLOC(sexp_sizeof(bytecode) + i);
    tmp->tag = SEXP_BYTECODE;
    sexp_bytecode_length(tmp) = i;
    memcpy(sexp_bytecode_data(tmp), sexp_bytecode_data(*bc), i);
    SEXP_FREE(*bc);
    *bc = tmp;
  }
}

static void expand_bcode(sexp *bc, sexp_uint_t *i, sexp_uint_t size) {
  sexp tmp;
  if (sexp_bytecode_length(*bc) < (*i)+size) {
    tmp = (sexp) SEXP_ALLOC(sexp_sizeof(bytecode)
                            + sexp_bytecode_length(*bc)*2);
    tmp->tag = SEXP_BYTECODE;
    sexp_bytecode_length(tmp) = sexp_bytecode_length(*bc)*2;
    memcpy(sexp_bytecode_data(tmp),
           sexp_bytecode_data(*bc),
           sexp_bytecode_length(*bc));
    SEXP_FREE(*bc);
    *bc = tmp;
  }
}

static void emit(sexp *bc, sexp_uint_t *i, char c)  {
  expand_bcode(bc, i, 1);
  sexp_bytecode_data(*bc)[(*i)++] = c;
}

static void emit_word(sexp *bc, sexp_uint_t *i, sexp_uint_t val)  {
  expand_bcode(bc, i, sizeof(sexp));
  *((sexp_uint_t*)(&(sexp_bytecode_data(*bc)[*i]))) = val;
  *i += sizeof(sexp_uint_t);
}

#define emit_push(bc,i,obj) (emit(bc,i,OP_PUSH), \
                             emit_word(bc,i,(sexp_uint_t)obj))

static sexp sexp_make_procedure(char flags, unsigned short num_args,
                                sexp bc, sexp vars) {
  sexp proc = (sexp) SEXP_ALLOC(sexp_sizeof(procedure));
  proc->tag = SEXP_PROCEDURE;
  sexp_procedure_flags(proc) = flags;
  sexp_procedure_num_args(proc) = num_args;
  sexp_procedure_code(proc) = bc;
  sexp_procedure_vars(proc) = vars;
  return proc;
}

static sexp sexp_make_macro (sexp p, sexp e) {
  sexp mac = (sexp) SEXP_ALLOC(sexp_sizeof(macro));
  mac->tag = SEXP_MACRO;
  sexp_macro_env(mac) = e;
  sexp_macro_proc(mac) = p;
  return mac;
}

/************************* the compiler ***************************/

sexp sexp_compile_error(char *message, sexp irritants) {
  return sexp_make_exception(the_compile_error_symbol,
                             sexp_make_string(message),
                             irritants, SEXP_FALSE, SEXP_FALSE);
}

sexp sexp_expand_macro (sexp mac, sexp form, sexp e) {
  sexp bc, res, *stack = SEXP_ALLOC(sizeof(sexp)*INIT_STACK_SIZE);
  sexp_uint_t i=0;
/*   fprintf(stderr, "expanding: "); */
/*   sexp_write(form, cur_error_port); */
/*   fprintf(stderr, "\n => "); */
  bc = (sexp) SEXP_ALLOC(sexp_sizeof(bytecode)+64);
  bc->tag = SEXP_BYTECODE;
  sexp_bytecode_length(bc) = 32;
  emit_push(&bc, &i, sexp_macro_env(mac));
  emit_push(&bc, &i, e);
  emit_push(&bc, &i, form);
  emit_push(&bc, &i, sexp_macro_proc(mac));
  emit(&bc, &i, OP_CALL);
  emit_word(&bc, &i, (sexp_uint_t) sexp_make_integer(3));
  emit(&bc, &i, OP_DONE);
  res = vm(bc, e, stack, 0);
/*   sexp_write(res, cur_error_port); */
/*   fprintf(stderr, "\n"); */
  SEXP_FREE(bc);
  SEXP_FREE(stack);
  return res;
}

sexp analyze(sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
             sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp) {
  int tmp1, tmp2;
  sexp o1, o2, e2, exn;

 loop:
  if (sexp_pairp(obj)) {
    if (sexp_symbolp(sexp_car(obj))) {
      o1 = env_cell(e, sexp_car(obj));
      if (! o1) {
        return analyze_app(obj, bc, i, e, params, fv, sv, d, tailp);
      }
      o1 = sexp_cdr(o1);
      if (sexp_corep(o1)) {
        switch (sexp_core_code(o1)) {
        case CORE_LAMBDA:
          return analyze_lambda(SEXP_FALSE, sexp_cadr(obj), sexp_cddr(obj),
                                bc, i, e, params, fv, sv, d, tailp);
        case CORE_DEFINE_SYNTAX:
          o2 = eval(sexp_caddr(obj), e);
          if (sexp_exceptionp(o2)) return o2;
          env_define(e, sexp_cadr(obj), sexp_make_macro(o2, e));
          emit_push(bc, i, SEXP_UNDEF);
          (*d)++;
          break;
        case CORE_DEFINE:
          if ((sexp_core_code(o1) == CORE_DEFINE)
              && sexp_pairp(sexp_cadr(obj))) {
            o2 = sexp_car(sexp_cadr(obj));
            exn = analyze_lambda(sexp_caadr(obj), sexp_cdadr(obj),
                                 sexp_cddr(obj),
                                 bc, i, e, params, fv, sv, d, 0);
          } else {
            o2 = sexp_cadr(obj);
            exn = analyze(sexp_caddr(obj), bc, i, e, params, fv, sv, d, 0);
          }
          if (sexp_exceptionp(exn)) return exn;
          if (sexp_env_global_p(e)) {
            emit(bc, i, OP_GLOBAL_SET);
            emit_word(bc, i, (sexp_uint_t) o2);
            emit_push(bc, i, SEXP_UNDEF);
          } else {
            o1 = env_cell(e, o2);
            return sexp_compile_error("define in bad position",
                                      sexp_list1(obj));
            emit(bc, i, OP_STACK_SET);
            emit_word(bc, i, sexp_unbox_integer(sexp_cdr(o1)));
          }
          (*d)++;
          break;
        case CORE_SET:
          exn = analyze(sexp_caddr(obj), bc, i, e, params, fv, sv, d, 0);
          if (sexp_exceptionp(exn)) return exn;
          if (sexp_list_index(sv, sexp_cadr(obj)) >= 0) {
            analyze_var_ref(sexp_cadr(obj), bc, i, e, params, fv, SEXP_NULL, d);
            emit(bc, i, OP_SET_CAR);
          } else {
            emit(bc, i, OP_GLOBAL_SET);
            emit_word(bc, i, (sexp_uint_t) sexp_cadr(obj));
            emit_push(bc, i, SEXP_UNDEF);
          }
          break;
        case CORE_BEGIN:
          return
            analyze_sequence(sexp_cdr(obj), bc, i, e, params, fv, sv, d, tailp);
        case CORE_IF:
          exn = analyze(sexp_cadr(obj), bc, i, e, params, fv, sv, d, 0);
          if (sexp_exceptionp(exn)) return exn;
          emit(bc, i, OP_JUMP_UNLESS);              /* jumps if test fails */
          (*d)--;
          tmp1 = *i;
          emit(bc, i, 0);
          exn = analyze(sexp_caddr(obj), bc, i, e, params, fv, sv, d, tailp);
          if (sexp_exceptionp(exn)) return exn;
          emit(bc, i, OP_JUMP);
          (*d)--;
          tmp2 = *i;
          emit(bc, i, 0);
          ((signed char*) sexp_bytecode_data(*bc))[tmp1] = (*i)-tmp1;
          if (sexp_pairp(sexp_cdddr(obj))) {
            exn = analyze(sexp_cadddr(obj), bc, i, e, params, fv, sv, d, tailp);
            if (sexp_exceptionp(exn)) return exn;
          } else {
            emit_push(bc, i, SEXP_UNDEF);
            (*d)++;
          }
          ((signed char*) sexp_bytecode_data(*bc))[tmp2] = (*i)-tmp2;
          break;
        case CORE_QUOTE:
          emit_push(bc, i, sexp_cadr(obj));
          (*d)++;
          break;
        default:
          return sexp_compile_error("unknown core form", sexp_list1(o1));
        }
      } else if (sexp_opcodep(o1)) {
        return analyze_opcode(o1, obj, bc, i, e, params, fv, sv, d, tailp);
      } else if (sexp_macrop(o1)) {
        obj = sexp_expand_macro(o1, obj, e);
        if (sexp_exceptionp(obj)) return obj;
        goto loop;
      } else {
        /* general procedure call */
        return analyze_app(obj, bc, i, e, params, fv, sv, d, tailp);
      }
    } else if (sexp_pairp(sexp_car(obj))) {
#if USE_FAST_LET
      o2 = env_cell(e, sexp_caar(obj));
      if (o2
          && sexp_corep(sexp_cdr(o2))
          && (sexp_core_code(o2) == CORE_LAMBDA)
          && sexp_listp(sexp_cadr(sexp_car(obj)))) {
        /* let */
        tmp1 = sexp_unbox_integer(sexp_length(sexp_cadar(obj)));
        /* push params as local stack variables */
        for (o2=sexp_reverse(sexp_cdr(obj)); sexp_pairp(o2); o2=sexp_cdr(o2)) {
          exn = analyze(sexp_car(o2), bc, i, e, params, fv, sv, d, 0);
          if (sexp_exceptionp(exn)) return exn;
        }
        /* analyze the body in a new local env */
        e2 = extend_env_closure(e, sexp_cadar(obj), (*d)+(tmp1-1));
        params = sexp_append(sexp_cadar(obj), params);
        exn =
          analyze_sequence(sexp_cddar(obj), bc, i, e, params, fv, sv, d, tailp);
        if (sexp_exceptionp(exn)) return exn;
        /* set the result and pop off the local vars */
        emit(bc, i, OP_STACK_SET);
        emit_word(bc, i, tmp1+1);
        (*d) -= (tmp1-1);
        for ( ; tmp1>0; tmp1--)
          emit(bc, i, OP_DROP);
      } else
#endif
        /* computed application */
        return analyze_app(obj, bc, i, e, params, fv, sv, d, tailp);
    } else {
      return sexp_compile_error("invalid operator", sexp_list1(sexp_car(obj)));
    }
  } else if (sexp_symbolp(obj)) {
    analyze_var_ref(obj, bc, i, e, params, fv, sv, d);
  } else {                      /* literal */
    emit_push(bc, i, obj);
    (*d)++;
  }
  return SEXP_TRUE;
}

sexp analyze_sequence (sexp ls, sexp *bc, sexp_uint_t *i, sexp e,
                       sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp)
{
  sexp exn;
  for ( ; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    if (sexp_pairp(sexp_cdr(ls))) {
      exn = analyze(sexp_car(ls), bc, i, e, params, fv, sv, d, 0);
      if (sexp_exceptionp(exn)) return exn;
      emit(bc, i, OP_DROP);
      (*d)--;
    } else {
      analyze(sexp_car(ls), bc, i, e, params, fv, sv, d, tailp);
    }
  }
  return SEXP_TRUE;
}

sexp analyze_opcode (sexp op, sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
                     sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp)
{
  int tmp1;
  sexp o1, exn;

  switch (sexp_opcode_class(op)) {
  case OPC_TYPE_PREDICATE:
  case OPC_PREDICATE:
  case OPC_ARITHMETIC:
  case OPC_ARITHMETIC_INV:
  case OPC_ARITHMETIC_CMP:
  case OPC_CONSTRUCTOR:
  case OPC_ACCESSOR:
  case OPC_GENERIC:
    tmp1 = sexp_unbox_integer(sexp_length(sexp_cdr(obj)));
    if (tmp1 == 0) {
      return sexp_compile_error("opcode with no arguments", sexp_list1(op));
    } else if (tmp1 == 1) {
      exn = analyze(sexp_cadr(obj), bc, i, e, params, fv, sv, d, 0);
      if (sexp_exceptionp(exn)) return exn;
      if (sexp_opcode_class(op) == OPC_ARITHMETIC_INV) {
        emit(bc, i, sexp_opcode_inverse(op));
        (*d)++;
      } else if (sexp_opcode_class(op) != OPC_ARITHMETIC) {
        emit(bc, i, sexp_opcode_code(op));
      }
    } else {
      for (o1=sexp_reverse(sexp_cdr(obj)); sexp_pairp(o1); o1=sexp_cdr(o1)) {
        exn = analyze(sexp_car(o1), bc, i, e, params, fv, sv, d, 0);
        if (sexp_exceptionp(exn)) return exn;
      }
      emit(bc, i, sexp_opcode_code(op));
      (*d) -= (tmp1-1);
      if (sexp_opcode_class(op) == OPC_ARITHMETIC)
        for (tmp1-=2; tmp1>0; tmp1--)
          emit(bc, i, sexp_opcode_code(op));
    }
    break;
  case OPC_IO:
    tmp1 = sexp_unbox_integer(sexp_length(sexp_cdr(obj)));
    if (tmp1 == sexp_opcode_num_args(op) && sexp_opcode_variadic_p(op)) {
      emit(bc, i, OP_PARAMETER);
      emit_word(bc, i, (sexp_uint_t) sexp_opcode_data(op));
      (*d)++;
      tmp1++;
    }
    for (o1=sexp_reverse(sexp_cdr(obj)); sexp_pairp(o1); o1=sexp_cdr(o1)) {
      exn = analyze(sexp_car(o1), bc, i, e, params, fv, sv, d, 0);
      if (sexp_exceptionp(exn)) return exn;
    }
    emit(bc, i, sexp_opcode_code(op));
    (*d) -= (tmp1-1);
    break;
  case OPC_PARAMETER:
    emit(bc, i, sexp_opcode_code(op));
    emit_word(bc, i, (sexp_uint_t) sexp_opcode_data(op));
    break;
  case OPC_FOREIGN:
    for (o1=sexp_reverse(sexp_cdr(obj)); sexp_pairp(o1); o1=sexp_cdr(o1)) {
      exn = analyze(sexp_car(o1), bc, i, e, params, fv, sv, d, 0);
      if (sexp_exceptionp(exn)) return exn;
    }
    emit_push(bc, i, sexp_opcode_data(op));
    emit(bc, i, sexp_opcode_code(op));
    (*d) -= (sexp_unbox_integer(sexp_length(sexp_cdr(obj)))-1);
    break;
  default:
    return sexp_compile_error("unknown opcode class", sexp_list1(op));
  }
  return SEXP_TRUE;
}

void analyze_var_ref (sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
                      sexp params, sexp fv, sexp sv, sexp_uint_t *d) {
  int tmp;
  sexp o1;
  if ((tmp = sexp_list_index(params, obj)) >= 0) {
    o1 = env_cell(e, obj);
    emit(bc, i, OP_STACK_REF);
    emit_word(bc, i, *d - sexp_unbox_integer(sexp_cdr(o1)));
  } else if ((tmp = sexp_list_index(fv, obj)) >= 0) {
    emit(bc, i, OP_CLOSURE_REF);
    emit_word(bc, i, (sexp_uint_t) sexp_make_integer(tmp));
  } else {
    emit(bc, i, OP_GLOBAL_REF);
    emit_word(bc, i, (sexp_uint_t) obj);
  }
  (*d)++;
  if (sexp_list_index(sv, obj) >= 0) {
    emit(bc, i, OP_CAR);
  }
}

sexp analyze_app (sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
                  sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp) {
  sexp o1, exn;
  sexp_uint_t len = sexp_unbox_integer(sexp_length(sexp_cdr(obj)));

  /* push the arguments onto the stack */
  for (o1 = sexp_reverse(sexp_cdr(obj)); sexp_pairp(o1); o1 = sexp_cdr(o1)) {
    exn = analyze(sexp_car(o1), bc, i, e, params, fv, sv, d, 0);
    if (sexp_exceptionp(exn)) return exn;
  }

  /* push the operator onto the stack */
  exn = analyze(sexp_car(obj), bc, i, e, params, fv, sv, d, 0);
  if (sexp_exceptionp(exn)) return exn;

  /* maybe overwrite the current frame */
  if (tailp) {
    emit(bc, i, OP_TAIL_CALL);
    emit_word(bc, i, (sexp_uint_t) sexp_make_integer(sexp_unbox_integer(sexp_length(params))+(*d)+3));
    emit_word(bc, i, (sexp_uint_t) sexp_make_integer(len));
  } else {
    /* normal call */
    emit(bc, i, OP_CALL);
    emit_word(bc, i, (sexp_uint_t) sexp_make_integer(len));
  }

  (*d) -= (len);
  return SEXP_TRUE;
}

sexp free_vars (sexp e, sexp formals, sexp obj, sexp fv) {
  sexp o1;
  if (sexp_symbolp(obj)) {
    if (env_global_p(e, obj)
        || (sexp_list_index(formals, obj) >= 0)
        || (sexp_list_index(fv, obj) >= 0))
      return fv;
    else
      return sexp_cons(obj, fv);
  } else if (sexp_pairp(obj)) {
    if (sexp_symbolp(sexp_car(obj))) {
      if ((o1 = env_cell(e, sexp_car(obj)))
          && sexp_corep(o1)
          && (sexp_core_code(sexp_cdr(o1)) == CORE_LAMBDA)) {
        return free_vars(e, sexp_cadr(obj), sexp_caddr(obj), fv);
      }
    }
    while (sexp_pairp(obj)) {
      fv = free_vars(e, formals, sexp_car(obj), fv);
      obj = sexp_cdr(obj);
    }
    return fv;
  } else {
    return fv;
  }
}

sexp set_vars (sexp e, sexp formals, sexp obj, sexp sv) {
  sexp tmp;
  if (sexp_nullp(formals))
    return sv;
  if (sexp_pairp(obj)) {
    if (sexp_symbolp(sexp_car(obj))) {
      if ((tmp = env_cell(e, sexp_car(obj))) && sexp_corep(sexp_cdr(tmp))) {
        if (sexp_core_code(sexp_cdr(tmp)) == CORE_LAMBDA) {
          formals = sexp_lset_diff(formals, sexp_cadr(obj));
          return set_vars(e, formals, sexp_caddr(obj), sv);
        } else if (sexp_core_code(sexp_cdr(tmp)) == CORE_SET
                   && (sexp_list_index(formals, sexp_cadr(obj)) >= 0)
                   && ! (sexp_list_index(sv, sexp_cadr(obj)) >= 0)) {
          sv = sexp_cons(sexp_cadr(obj), sv);
          return set_vars(e, formals, sexp_caddr(obj), sv);
        }
      }
    }
    while (sexp_pairp(obj)) {
      sv = set_vars(e, formals, sexp_car(obj), sv);
      obj = sexp_cdr(obj);
    }
  }
  return sv;
}

sexp analyze_lambda (sexp name, sexp formals, sexp body,
                     sexp *bc, sexp_uint_t *i, sexp e,
                     sexp params, sexp fv, sexp sv, sexp_uint_t *d,
                     int tailp) {
  sexp obj, ls, flat_formals, fv2, e2;
  int k;
  flat_formals = sexp_flatten_dot(formals);
  fv2 = free_vars(e, flat_formals, body, SEXP_NULL);
  e2 = extend_env_closure(e, flat_formals, -4);
  /* compile the body with respect to the new params */
  obj = compile(flat_formals, body, e2, fv2, sv, 0);
  if (sexp_exceptionp(obj)) return obj;
  /* push the closed vars */
  emit_push(bc, i, SEXP_UNDEF);
  emit_push(bc, i, sexp_length(fv2));
  emit(bc, i, OP_MAKE_VECTOR);
  (*d)++;
  for (ls=fv2, k=0; sexp_pairp(ls); ls=sexp_cdr(ls), k++) {
    analyze_var_ref(sexp_car(ls), bc, i, e, params, fv, SEXP_NULL, d);
    emit_push(bc, i, sexp_make_integer(k));
    emit(bc, i, OP_STACK_REF);
    emit_word(bc, i, 3);
    emit(bc, i, OP_VECTOR_SET);
    emit(bc, i, OP_DROP);
    (*d)--;
  }
  /* push the additional procedure info and make the closure */
  emit_push(bc, i, obj);
  emit_push(bc, i, sexp_length(formals));
  emit_push(bc, i, sexp_make_integer(sexp_listp(formals) ? 0 : 1));
  emit(bc, i, OP_MAKE_PROCEDURE);
  return SEXP_TRUE;
}

sexp make_param_list(sexp_uint_t i) {
  sexp res = SEXP_NULL;
  char sym[2]="a";
  for (sym[0]+=i; i>0; i--) {
    sym[0] = sym[0]-1;
    res = sexp_cons(sexp_intern(sym), res);
  }
  return res;
}

sexp make_opcode_procedure(sexp op, sexp_uint_t i, sexp e) {
  sexp bc, params, res;
  sexp_uint_t pos=0, d=0;
  if (i == sexp_opcode_num_args(op) && sexp_opcode_proc(op))
    return sexp_opcode_proc(op);
  bc = (sexp) SEXP_ALLOC(sexp_sizeof(bytecode)+INIT_BCODE_SIZE);
  params = make_param_list(i);
  e = extend_env_closure(e, params, -4);
  bc->tag = SEXP_BYTECODE;
  sexp_bytecode_length(bc) = INIT_BCODE_SIZE;
  analyze_opcode(op, sexp_cons(op, params), &bc, &pos, e, params,
                 SEXP_NULL, SEXP_NULL, &d, 0);
  emit(&bc, &pos, OP_RET);
  shrink_bcode(&bc, pos);
  /* disasm(bc); */
  res = sexp_make_procedure(0, (int) sexp_make_integer(i), bc, SEXP_UNDEF);
  if (i == sexp_opcode_num_args(op))
    sexp_opcode_proc(op) = res;
  return res;
}

sexp compile(sexp params, sexp obj, sexp e, sexp fv, sexp sv, int done_p) {
  sexp_uint_t i=0, j=0, d=0, define_ok=1, core;
  sexp bc = (sexp) SEXP_ALLOC(sexp_sizeof(bytecode)+INIT_BCODE_SIZE);
  sexp sv2 = set_vars(e, params, obj, SEXP_NULL), internals=SEXP_NULL, ls;
  bc->tag = SEXP_BYTECODE;
  sexp_bytecode_length(bc) = INIT_BCODE_SIZE;
  /* box mutable vars */
  for (ls=params; sexp_pairp(ls); ls=sexp_cdr(ls)) {
    if ((j = sexp_list_index(sv2, sexp_car(ls)) >= 0)) {
      emit_push(&bc, &i, SEXP_NULL);
      emit(&bc, &i, OP_STACK_REF);
      emit_word(&bc, &i, j+4);
      emit(&bc, &i, OP_CONS);
      emit(&bc, &i, OP_STACK_SET);
      emit_word(&bc, &i, j+4);
      emit(&bc, &i, OP_DROP);
    }
  }
  sv = sexp_append(sv2, sv);
  /* determine internal defines */
  if (sexp_env_parent(e)) {
    for (ls=SEXP_NULL; sexp_pairp(obj); obj=sexp_cdr(obj)) {
      core = (sexp_pairp(sexp_car(obj)) && sexp_symbolp(sexp_caar(obj))
              ? core_code(e, sexp_caar(obj)) : 0);
      if (core == CORE_BEGIN) {
        obj = sexp_cons(sexp_car(obj),
                        sexp_append(sexp_cdar(obj), sexp_cdr(obj)));
      } else {
        if (core == CORE_DEFINE) {
          return sexp_compile_error("definition in non-definition context",
                                    sexp_list1(obj));
          internals = sexp_cons(sexp_pairp(sexp_cadar(obj))
                                ? sexp_car(sexp_cadar(obj)) : sexp_cadar(obj),
                                internals);
        } else {
          define_ok = 0;
        }
        ls = sexp_cons(sexp_car(obj), ls);
      }
    }
    obj = sexp_reverse(ls);
    j = sexp_unbox_integer(sexp_length(internals));
    if (sexp_pairp(internals)) {
      e = extend_env_closure(e, internals, 2);
      params = sexp_append(internals, params);
      for (ls=internals; sexp_pairp(ls); ls=sexp_cdr(ls))
        emit_push(&bc, &i, (sexp_uint_t) SEXP_UNDEF);
      d+=j;
    }
  }
  /* analyze body sequence */
  analyze_sequence(obj, &bc, &i, e, params, fv, sv, &d,
                   (! done_p) && (! sexp_pairp(internals)));
  if (sexp_pairp(internals)) {
    emit(&bc, &i, OP_STACK_SET);
    emit_word(&bc, &i, j+1);
    for ( ; j>0; j--)
      emit(&bc, &i, OP_DROP);
  }
  emit(&bc, &i, done_p ? OP_DONE : OP_RET);
  shrink_bcode(&bc, i);
/*   print_bytecode(bc); */
/*   disasm(bc); */
  return bc;
}

/*********************** the virtual machine **************************/

sexp sexp_save_stack(sexp *stack, sexp_uint_t to) {
  sexp res, *data;
  sexp_uint_t i;
  res = sexp_make_vector(sexp_make_integer(to), SEXP_UNDEF);
  data = sexp_vector_data(res);
  for (i=0; i<to; i++)
    data[i] = stack[i];
  return res;
}

sexp_uint_t sexp_restore_stack(sexp saved, sexp *current) {
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
#define _PUSH(x) (stack[top++]=(x))
#define _POP() (stack[--top])

#define sexp_raise(msg, args) {stack[top]=sexp_compile_error(msg, args); top++; goto call_error_handler;}

sexp vm(sexp bc, sexp e, sexp* stack, sexp_sint_t top) {
  unsigned char *ip=sexp_bytecode_data(bc);
  sexp cp=SEXP_UNDEF, tmp1, tmp2;
  sexp_sint_t i, j, k;

 loop:
/*   print_stack(stack, top); */
/*   fprintf(stderr, "OP: %s (%d)\n", (*ip<=71) ? reverse_opcode_names[*ip] : "<unknown>", *ip); */
  switch (*ip++) {
  case OP_NOOP:
    fprintf(stderr, "noop\n");
    break;
  case OP_GLOBAL_REF:
    tmp1 = env_cell(e, ((sexp*)ip)[0]);
    if (! tmp1) sexp_raise("undefined-variable", sexp_list1(tmp1));
    stack[top++]=sexp_cdr(tmp1);
    ip += sizeof(sexp);
    break;
  case OP_GLOBAL_SET:
    env_define(e, ((sexp*)ip)[0], _POP());
    ip += sizeof(sexp);
    break;
  case OP_STACK_REF:
    stack[top] = stack[top - (sexp_sint_t) ((sexp*)ip)[0]];
    ip += sizeof(sexp);
    top++;
    break;
  case OP_STACK_SET:
    stack[top - (sexp_sint_t) ((sexp*)ip)[0]] = _ARG1;
    _ARG1 = SEXP_UNDEF;
    ip += sizeof(sexp);
    break;
  case OP_CLOSURE_REF:
    _PUSH(sexp_vector_ref(cp, ((sexp*)ip)[0]));
    ip += sizeof(sexp);
    break;
  case OP_VECTOR_REF:
    _ARG2 = sexp_vector_ref(_ARG1, _ARG2);
    top--;
    break;
  case OP_VECTOR_SET:
    sexp_vector_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_UNDEF;
    top-=2;
    break;
  case OP_STRING_REF:
    _ARG2 = sexp_string_ref(_ARG1, _ARG2);
    top--;
    break;
  case OP_STRING_SET:
    sexp_string_set(_ARG1, _ARG2, _ARG3);
    _ARG3 = SEXP_UNDEF;
    top-=2;
    break;
  case OP_MAKE_PROCEDURE:
    _ARG4 = sexp_make_procedure((int) _ARG1, (int) _ARG2, _ARG3, _ARG4);
    top-=3;
    break;
  case OP_MAKE_VECTOR:
    _ARG2 = sexp_make_vector(_ARG1, _ARG2);
    top--;
    break;
  case OP_PUSH:
    _PUSH(((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_DROP:
    top--;
    break;
  case OP_PARAMETER:
    _PUSH(*(sexp*)((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_PAIRP:
    _ARG1 = sexp_make_boolean(sexp_pairp(_ARG1)); break;
  case OP_NULLP:
    _ARG1 = sexp_make_boolean(sexp_nullp(_ARG1)); break;
  case OP_CHARP:
    _ARG1 = sexp_make_boolean(sexp_charp(_ARG1)); break;
  case OP_INTEGERP:
    _ARG1 = sexp_make_boolean(sexp_integerp(_ARG1)); break;
  case OP_SYMBOLP:
    _ARG1 = sexp_make_boolean(sexp_symbolp(_ARG1)); break;
  case OP_STRINGP:
    _ARG1 = sexp_make_boolean(sexp_stringp(_ARG1)); break;
  case OP_VECTORP:
    _ARG1 = sexp_make_boolean(sexp_vectorp(_ARG1)); break;
  case OP_PROCEDUREP:
    _ARG1 = sexp_make_boolean(sexp_procedurep(_ARG1)); break;
  case OP_IPORTP:
    _ARG1 = sexp_make_boolean(sexp_iportp(_ARG1)); break;
  case OP_OPORTP:
    _ARG1 = sexp_make_boolean(sexp_oportp(_ARG1)); break;
  case OP_EOFP:
    _ARG1 = sexp_make_boolean(_ARG1 == SEXP_EOF); break;
  case OP_CAR:
    if (! sexp_pairp(_ARG1)) sexp_raise("not a pair", sexp_list1(_ARG1));
    _ARG1 = sexp_car(_ARG1); break;
  case OP_CDR:
    if (! sexp_pairp(_ARG1)) sexp_raise("not a pair", sexp_list1(_ARG1));
    _ARG1 = sexp_cdr(_ARG1); break;
  case OP_SET_CAR:
    if (! sexp_pairp(_ARG1)) sexp_raise("not a pair", sexp_list1(_ARG1));
    sexp_car(_ARG1) = _ARG2;
    _ARG2 = SEXP_UNDEF;
    top--;
    break;
  case OP_SET_CDR:
    if (! sexp_pairp(_ARG1)) sexp_raise("not a pair", sexp_list1(_ARG1));
    sexp_cdr(_ARG1) = _ARG2;
    _ARG2 = SEXP_UNDEF;
    top--;
    break;
  case OP_CONS:
    _ARG2 = sexp_cons(_ARG1, _ARG2);
    top--;
    break;
  case OP_ADD:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_add(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_add(_ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fp_add(_ARG1, sexp_integer_to_flonum(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_add(sexp_integer_to_flonum(_ARG1), _ARG2);
#endif
    else sexp_raise("not a number", sexp_list2(_ARG1, _ARG2));
    top--;
    break;
  case OP_SUB:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_sub(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_sub(_ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fp_sub(_ARG1, sexp_integer_to_flonum(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_sub(sexp_integer_to_flonum(_ARG1), _ARG2);
#endif
    else sexp_raise("not a number", sexp_list2(_ARG1, _ARG2));
    top--;
    break;
  case OP_MUL:
    if (sexp_integerp(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fx_mul(_ARG1, _ARG2);
#if USE_FLONUMS
    else if (sexp_flonump(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_mul(_ARG1, _ARG2);
    else if (sexp_flonump(_ARG1) && sexp_integerp(_ARG2))
      _ARG2 = sexp_fp_mul(_ARG1, sexp_integer_to_flonum(_ARG2));
    else if (sexp_integerp(_ARG1) && sexp_flonump(_ARG2))
      _ARG2 = sexp_fp_mul(sexp_integer_to_flonum(_ARG1), _ARG2);
#endif
    else sexp_raise("not a number", sexp_list2(_ARG1, _ARG2));
    top--;
    break;
  case OP_DIV:
    _ARG2 = sexp_fx_div(_ARG1, _ARG2);
    top--;
    break;
  case OP_MOD:
    _ARG2 = sexp_fx_mod(_ARG1, _ARG2);
    top--;
    break;
  case OP_LT:
    _ARG2 = sexp_make_boolean(_ARG1 < _ARG2);
    top--;
    break;
  case OP_LE:
    _ARG2 = sexp_make_boolean(_ARG1 <= _ARG2);
    top--;
    break;
  case OP_GT:
    _ARG2 = sexp_make_boolean(_ARG1 > _ARG2);
    top--;
    break;
  case OP_GE:
    _ARG2 = sexp_make_boolean(_ARG1 >= _ARG2);
    top--;
    break;
  case OP_EQ:
  case OP_EQN:
    _ARG2 = sexp_make_boolean(_ARG1 == _ARG2);
    top--;
    break;
  case OP_TAIL_CALL:
    /* old-args ... n ret-ip ret-cp new-args ...   proc  */
    /* [================= j ===========================] */
    /*                              [==== i =====]       */
    j = sexp_unbox_integer(((sexp*)ip)[0]);    /* current depth */
    i = sexp_unbox_integer(((sexp*)ip)[1]);    /* number of params */
    tmp1 = _ARG1;                       /* procedure to call */
    /* save frame info */
    ip = ((unsigned char*) sexp_unbox_integer(stack[top-i-3])) - sizeof(sexp);
    cp = stack[top-i-2];
    /* copy new args into place */
    for (k=0; k<i; k++)
      stack[top-j+k] = stack[top-i-1+k];
    top -= (j-i-1);
    goto make_call;
  case OP_CALL:
    if (top >= INIT_STACK_SIZE)
      sexp_raise("out of stack space", SEXP_NULL);
    i = sexp_unbox_integer(((sexp*)ip)[0]);
    tmp1 = _ARG1;
  make_call:
    if (sexp_opcodep(tmp1)) {
      /* compile non-inlined opcode applications on the fly */
      tmp1 = make_opcode_procedure(tmp1, i, e);
      if (sexp_exceptionp(tmp1)) {
        _ARG1 = tmp1;
        goto call_error_handler;
      }
    }
    if (! sexp_procedurep(tmp1))
      sexp_raise("non procedure application", sexp_list1(tmp1));
    j = i - sexp_unbox_integer(sexp_procedure_num_args(tmp1));
    if (j < 0)
      sexp_raise("not enough args", sexp_list2(tmp1, sexp_make_integer(i)));
    if (j > 0) {
      if (sexp_procedure_variadic_p(tmp1)) {
        stack[top-i-1] = sexp_cons(stack[top-i-1], SEXP_NULL);
        for (k=top-i; k<top-(i-j)-1; k++)
          stack[top-i-1] = sexp_cons(stack[k], stack[top-i-1]);
        for ( ; k<top; k++)
          stack[k-j+1] = stack[k];
        top -= (j-1);
        i-=(j-1);
      } else {
        sexp_raise("too many args", sexp_list2(tmp1, sexp_make_integer(i)));
      }
    } else if (sexp_procedure_variadic_p(tmp1)) {
      /* shift stack, set extra arg to null */
      for (k=top; k>=top-i; k--)
        stack[k] = stack[k-1];
      stack[top-i-1] = SEXP_NULL;
      top++;
      i++;
    }
    _ARG1 = sexp_make_integer(i);
    stack[top] = sexp_make_integer(ip+sizeof(sexp));
    stack[top+1] = cp;
    top+=2;
    bc = sexp_procedure_code(tmp1);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
    break;
  case OP_APPLY1:
    tmp1 = _ARG1;
    tmp2 = _ARG2;
    i = sexp_unbox_integer(sexp_length(tmp2));
    top += (i-2);
    for ( ; sexp_pairp(tmp2); tmp2=sexp_cdr(tmp2), top--)
      _ARG1 = sexp_car(tmp2);
    top += i+1;
    ip -= sizeof(sexp);
    goto make_call;
  case OP_CALLCC:
    tmp1 = _ARG1;
    i = 1;
    stack[top] = sexp_make_integer(1);
    stack[top+1] = sexp_make_integer(ip);
    stack[top+2] = cp;
    _ARG1
      = sexp_make_procedure(0, (int) sexp_make_integer(1),
                            continuation_resumer,
                            sexp_vector(1, sexp_save_stack(stack, top+3)));
    top++;
    ip -= sizeof(sexp);
    goto make_call;
    break;
  case OP_RESUMECC:
    tmp1 = _ARG4;
    top = sexp_restore_stack(sexp_vector_ref(cp, 0), stack);
    cp = _ARG1;
    ip = (unsigned char*) sexp_unbox_integer(_ARG2);
    i = sexp_unbox_integer(_ARG3);
    top -= 3;
    _ARG1 = tmp1;
    break;
  case OP_ERROR:
  call_error_handler:
    tmp1 = sexp_cdr(exception_handler_cell);
    _ARG1 = SEXP_UNDEF;
    stack[top] = (sexp) 1;
    stack[top+1] = sexp_make_integer(ip+4);
    stack[top+2] = cp;
    top+=3;
    bc = sexp_procedure_code(tmp1);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
    break;
  case OP_FCALL0:
    _ARG1 = ((sexp_proc0)_ARG1)();
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_FCALL1:
    _ARG2 = ((sexp_proc1)_ARG1)(_ARG2);
    top--;
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_FCALL2:
    _ARG3 = ((sexp_proc2)_ARG1)(_ARG2, _ARG3);
    top-=2;
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_FCALL3:
    _ARG4 =((sexp_proc3)_ARG1)(_ARG2, _ARG3, _ARG4);
    top-=3;
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_JUMP_UNLESS:
    if (stack[--top] == SEXP_FALSE) {
      ip += ((signed char*)ip)[0];
    } else {
      ip++;
    }
    break;
  case OP_JUMP:
    ip += ((signed char*)ip)[0];
    break;
  case OP_DISPLAY:
    if (sexp_stringp(_ARG1)) {
      sexp_write_string(sexp_string_data(_ARG1), _ARG2);
      break;
    }
  case OP_WRITE:
    sexp_write(_ARG1, _ARG2);
    _ARG2 = SEXP_UNDEF;
    top--;
    break;
  case OP_WRITE_CHAR:
    sexp_write_char(sexp_unbox_character(_ARG1), _ARG2);
    break;
  case OP_NEWLINE:
    sexp_write_char('\n', _ARG1);
    _ARG1 = SEXP_UNDEF;
    break;
  case OP_FLUSH_OUTPUT:
    sexp_flush(_ARG1);
    _ARG1 = SEXP_UNDEF;
    break;
  case OP_READ:
    _ARG1 = sexp_read(_ARG1);
    if (sexp_exceptionp(_ARG1)) goto call_error_handler;
    break;
  case OP_READ_CHAR:
    i = sexp_read_char(_ARG1);
    _ARG1 = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case OP_RET:
    if (top<4)
      goto end_loop;
    cp = _ARG2;
    ip = (unsigned char*) sexp_unbox_integer(_ARG3);
    i = sexp_unbox_integer(_ARG4);
    stack[top-i-4] = _ARG1;
    top = top-i-3;
    break;
  case OP_DONE:
    goto end_loop;
  default:
    sexp_raise("unknown opcode", sexp_list1(sexp_make_integer(*(ip-1))));
  }
  goto loop;

 end_loop:
  return _ARG1;
}

/************************ library procedures **************************/

sexp sexp_open_input_file (sexp path) {
  return sexp_make_input_port(fopen(sexp_string_data(path), "r"));
}

sexp sexp_open_output_file (sexp path) {
  return sexp_make_input_port(fopen(sexp_string_data(path), "w"));
}

sexp sexp_close_port (sexp port) {
  fclose(sexp_port_stream(port));
  return SEXP_UNDEF;
}

sexp sexp_load (sexp source) {
  sexp obj, res, *stack=SEXP_ALLOC(sizeof(sexp)*INIT_STACK_SIZE);
  int closep = 0;
  if (sexp_stringp(source)) {
    source = sexp_open_input_file(source);
    closep = 1;
  }
  while ((obj=sexp_read(source)) != (sexp) SEXP_EOF) {
    res = eval_in_stack(obj, interaction_environment, stack, 0);
    if (sexp_exceptionp(res)) goto done;
  }
  res = SEXP_UNDEF;
 done:
  if (closep) sexp_close_port(source);
  SEXP_FREE(stack);
  return res;
}

/*********************** standard environment *************************/

static struct sexp_struct core_forms[] = {
  {.tag=SEXP_CORE, .value={.core={CORE_DEFINE, "define"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_SET, "set!"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_LAMBDA, "lambda"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_IF, "if"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_BEGIN, "begin"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_QUOTE, "quote"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_DEFINE_SYNTAX, "define-syntax"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_LET_SYNTAX, "let-syntax"}}},
  {.tag=SEXP_CORE, .value={.core={CORE_LETREC_SYNTAX, "letrec-syntax"}}},
};

static struct sexp_struct opcodes[] = {
#define _OP(c,o,n,m,t,u,i,s,d,p) {.tag=SEXP_OPCODE, .value={.opcode={c, o, n, m, t, u, i, s, d, p}}}
#define _FN(o,n,t,u,s,f) _OP(OPC_FOREIGN, o, n, 0, t, u, 0, s, (sexp)f, NULL)
#define _FN0(s, f) _FN(OP_FCALL0, 0, 0, 0, s, f)
#define _FN1(t, s, f) _FN(OP_FCALL1, 1, t, 0, s, f)
#define _FN2(t, u, s, f) _FN(OP_FCALL2, 2, t, u, s, f)
#define _PARAM(n,a,t) _OP(OPC_PARAMETER, OP_PARAMETER, 0, 1, t, 0, 0, n, a, NULL)
_OP(OPC_ACCESSOR, OP_CAR, 1, 0, SEXP_PAIR, 0, 0, "car", NULL, NULL),
_OP(OPC_ACCESSOR, OP_SET_CAR, 2, 0, SEXP_PAIR, 0, 0, "set-car!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_CDR, 1, 0, SEXP_PAIR, 0, 0, "cdr", NULL, NULL),
_OP(OPC_ACCESSOR, OP_SET_CDR, 2, 0, SEXP_PAIR, 0, 0, "set-cdr!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_VECTOR_REF,2,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-ref", NULL, NULL),
_OP(OPC_ACCESSOR, OP_VECTOR_SET,3,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-set!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_STRING_REF,2,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-ref", NULL, NULL),
_OP(OPC_ACCESSOR, OP_STRING_SET,3,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-set!", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_ADD, 0, 1, SEXP_FIXNUM, 0, 0, "+", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_MUL, 0, 1, SEXP_FIXNUM, 0, 0, "*", NULL, NULL),
_OP(OPC_ARITHMETIC_INV, OP_SUB, 0, 1, SEXP_FIXNUM, 0, OP_NEG, "-", NULL, NULL),
_OP(OPC_ARITHMETIC_INV, OP_DIV, 0, 1, SEXP_FIXNUM, 0, OP_INV, "/", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_MOD, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, 0, "%", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_LT,  0, 1, SEXP_FIXNUM, 0, 0, "<", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_LE,  0, 1, SEXP_FIXNUM, 0, 0, "<=", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_GT,  0, 1, SEXP_FIXNUM, 0, 0, ">", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_GE,  0, 1, SEXP_FIXNUM, 0, 0, ">=", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_EQN, 0, 1, SEXP_FIXNUM, 0, 0, "=", NULL, NULL),
_OP(OPC_PREDICATE,      OP_EQ,  2, 0, 0, 0, 0, "eq?", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_CONS, 2, 0, 0, 0, 0, "cons", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_MAKE_VECTOR, 2, 0, SEXP_FIXNUM, 0, 0, "make-vector", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_MAKE_PROCEDURE, 4, 0, 0, 0, 0, "make-procedure", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_PAIRP,  1, 0, 0, 0, 0, "pair?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_NULLP,  1, 0, 0, 0, 0, "null?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_STRINGP,  1, 0, 0, 0, 0, "string?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_SYMBOLP,  1, 0, 0, 0, 0, "symbol?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_CHARP,  1, 0, 0, 0, 0, "char?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_VECTORP,  1, 0, 0, 0, 0, "vector?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_PROCEDUREP,  1, 0, 0, 0, 0, "procedure?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_IPORTP,  1, 0, 0, 0, 0, "input-port?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_OPORTP,  1, 0, 0, 0, 0, "output-port?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_EOFP,  1, 0, 0, 0, 0, "eof-object?", NULL, NULL),
_OP(OPC_GENERIC, OP_APPLY1, 2, 0, SEXP_PROCEDURE, SEXP_PAIR, 0, "apply1", NULL, NULL),
_OP(OPC_GENERIC, OP_CALLCC, 1, SEXP_PROCEDURE, 0, 0, 0, "call-with-current-continuation", NULL, NULL),
_OP(OPC_GENERIC, OP_ERROR, 1, SEXP_STRING, 0, 0, 0, "error", NULL, NULL),
_OP(OPC_IO, OP_WRITE, 1, 1, 0, SEXP_OPORT, 0, "write", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_DISPLAY, 1, 1, 0, SEXP_OPORT, 0, "display", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_WRITE_CHAR, 1, 1, 0, SEXP_OPORT, 0, "write-char", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_NEWLINE, 0, 1, 0, SEXP_OPORT, 0, "newline", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_FLUSH_OUTPUT, 0, 1, 0, SEXP_OPORT, 0, "flush-output", (sexp)&cur_output_port, NULL),
_OP(OPC_IO, OP_READ, 0, 1, 0, SEXP_IPORT, 0, "read", (sexp)&cur_input_port, NULL),
_OP(OPC_IO, OP_READ_CHAR, 0, 1, 0, SEXP_IPORT, 0, "read-char", (sexp)&cur_input_port, NULL),
_FN1(SEXP_PAIR, "length", sexp_length),
_FN1(SEXP_PAIR, "reverse", sexp_reverse),
_FN1(SEXP_PAIR, "list->vector", sexp_list_to_vector),
_FN1(SEXP_STRING, "open-input-file", sexp_open_input_file),
_FN1(SEXP_STRING, "open-output-file", sexp_open_output_file),
_FN1(SEXP_IPORT, "close-input-port", sexp_close_port),
_FN1(SEXP_OPORT, "close-output-port", sexp_close_port),
_FN1(0, "load", sexp_load),
_FN2(0, SEXP_PAIR, "memq", sexp_memq),
_FN2(0, SEXP_PAIR, "assq", sexp_assq),
_FN2(SEXP_PAIR, SEXP_PAIR, "diffq", sexp_lset_diff),
_PARAM("current-input-port", (sexp)&cur_input_port, SEXP_IPORT),
_PARAM("current-output-port", (sexp)&cur_output_port, SEXP_OPORT),
_PARAM("current-error-port", (sexp)&cur_error_port, SEXP_OPORT),
_PARAM("interaction-environment", (sexp)&interaction_environment, SEXP_ENV),
#undef _OP
#undef _FN
#undef _FN0
#undef _FN1
#undef _FN2
#undef _PARAM
};

sexp make_standard_env() {
  sexp_uint_t i;
  sexp e = (sexp) SEXP_ALLOC(sexp_sizeof(env));
  e->tag = SEXP_ENV;
  sexp_env_parent(e) = NULL;
  sexp_env_bindings(e) = SEXP_NULL;
  for (i=0; i<(sizeof(core_forms)/sizeof(core_forms[0])); i++)
    env_define(e, sexp_intern(sexp_core_name(&core_forms[i])), &core_forms[i]);
  for (i=0; i<(sizeof(opcodes)/sizeof(opcodes[0])); i++)
    env_define(e, sexp_intern(sexp_opcode_name(&opcodes[i])), &opcodes[i]);
  return e;
}

/************************** eval interface ****************************/

sexp eval_in_stack(sexp obj, sexp e, sexp* stack, sexp_sint_t top) {
  sexp bc;
  bc = compile(SEXP_NULL, sexp_cons(obj, SEXP_NULL), e, SEXP_NULL, SEXP_NULL, 1);
  return vm(bc, e, stack, top);
}

sexp eval(sexp obj, sexp e) {
  sexp* stack = (sexp*) SEXP_ALLOC(sizeof(sexp) * INIT_STACK_SIZE);
  sexp res = eval_in_stack(obj, e, stack, 0);
  SEXP_FREE(stack);
  return res;
}

void scheme_init() {
  sexp bc;
  sexp_uint_t i=0;
  if (! scheme_initialized_p) {
    scheme_initialized_p = 1;
    sexp_init();
    cur_input_port = sexp_make_input_port(stdin);
    cur_output_port = sexp_make_output_port(stdout);
    cur_error_port = sexp_make_output_port(stderr);
    the_compile_error_symbol = sexp_intern("compile-error");
    bc = (sexp) SEXP_ALLOC(sexp_sizeof(bytecode)+16);
    bc->tag = SEXP_BYTECODE;
    sexp_bytecode_length(bc) = 16;
    emit(&bc, &i, OP_RESUMECC);
    continuation_resumer = (sexp) bc;
  }
}

void repl (sexp e, sexp *stack) {
  sexp obj, res;
  while (1) {
    sexp_write_string("> ", cur_output_port);
    sexp_flush(cur_output_port);
    obj = sexp_read(cur_input_port);
    if (obj == SEXP_EOF)
      break;
    res = eval_in_stack(obj, e, stack, 0);
    if (res != SEXP_UNDEF) {
      sexp_write(res, cur_output_port);
      sexp_write_char('\n', cur_output_port);
    }
  }
}

int main (int argc, char **argv) {
  sexp bc, e, obj, res, *stack, err_handler, err_handler_sym;
  sexp_uint_t i, quit=0, init_loaded=0;

  scheme_init();
  stack = (sexp*) SEXP_ALLOC(sizeof(sexp) * INIT_STACK_SIZE);
  e = make_standard_env();
  interaction_environment = e;
  bc = (sexp) SEXP_ALLOC(sexp_sizeof(bytecode)+16);
  bc->tag = SEXP_BYTECODE;
  sexp_bytecode_length(bc) = 16;
  i = 0;
  emit_push(&bc, &i, (sexp_uint_t) SEXP_UNDEF);
  emit(&bc, &i, OP_DONE);
  err_handler = sexp_make_procedure(0, 0, bc, sexp_make_vector(0, SEXP_UNDEF));
  err_handler_sym = sexp_intern("*error-handler*");
  env_define(e, err_handler_sym, err_handler);
  exception_handler_cell = env_cell(e, err_handler_sym);

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'e':
    case 'p':
      if (! init_loaded) {
        sexp_load(sexp_make_string(sexp_init_file));
        init_loaded = 1;
      }
      obj = sexp_read_from_string(argv[i+1]);
      res = eval_in_stack(obj, e, stack, 0);
      if (argv[i][1] == 'p') {
        sexp_write(res, cur_output_port);
        sexp_write_char('\n', cur_output_port);
      }
      quit=1;
      i++;
      break;
    case 'q':
      init_loaded = 1;
      break;
    default:
      errx(1, "unknown option: %s", argv[i]);
    }
  }

  if (! quit) {
    if (! init_loaded)
      sexp_load(sexp_make_string(sexp_init_file));
    if (i < argc)
      for ( ; i < argc; i++)
        sexp_load(sexp_make_string(argv[i]));
    else
      repl(e, stack);
  }
  return 0;
}

