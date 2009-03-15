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
    for (ls=sexp_env_bindings(e); SEXP_PAIRP(ls); ls=SEXP_CDR(ls))
      if (SEXP_CAAR(ls) == key)
        return SEXP_CAR(ls);
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
    SEXP_CDR(cell) = value;
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
  for (i=offset; SEXP_PAIRP(fv); fv = SEXP_CDR(fv), i--)
    sexp_env_bindings(e2)
      = sexp_cons(sexp_cons(SEXP_CAR(fv), sexp_make_integer(i)),
                  sexp_env_bindings(e2));
  return e2;
}

static int core_code (sexp e, sexp sym) {
  sexp cell = env_cell(e, sym);
  if (! cell || ! SEXP_COREP(SEXP_CDR(cell))) return 0;
  return (sexp_core_code(SEXP_CDR(cell)));
}

static sexp sexp_reverse_flatten_dot (sexp ls) {
  sexp res;
  for (res=SEXP_NULL; SEXP_PAIRP(ls); ls=SEXP_CDR(ls))
    res = sexp_cons(SEXP_CAR(ls), res);
  return (SEXP_NULLP(ls) ? res : sexp_cons(ls, res));
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

sexp sexp_expand_macro (sexp mac, sexp form, sexp e) {
  sexp bc, res, *stack = SEXP_ALLOC(sizeof(sexp)*INIT_STACK_SIZE);
  sexp_uint_t i=0;
  fprintf(stderr, "expanding: ");
  sexp_write(form, cur_error_port);
  fprintf(stderr, "\n => ");
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
  sexp_write(res, cur_error_port);
  fprintf(stderr, "\n");
  SEXP_FREE(bc);
  SEXP_FREE(stack);
  return res;
}

void analyze(sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
             sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp) {
  int tmp1, tmp2, tmp3;
  sexp o1, o2, e2, cell;

 loop:
  if (SEXP_PAIRP(obj)) {
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      o1 = env_cell(e, SEXP_CAR(obj));
      if (! o1) {
        analyze_app(obj, bc, i, e, params, fv, sv, d, tailp);
        return;
      }
      o1 = SEXP_CDR(o1);
      if (SEXP_COREP(o1)) {
        switch (sexp_core_code(o1)) {
        case CORE_LAMBDA:
          analyze_lambda(SEXP_FALSE, SEXP_CADR(obj), SEXP_CDDR(obj),
                         bc, i, e, params, fv, sv, d, tailp);
          break;
        case CORE_DEFINE_SYNTAX:
          env_define(e, SEXP_CADR(obj),
                     sexp_make_macro(eval(SEXP_CADDR(obj), e), e));
          emit_push(bc, i, SEXP_UNDEF);
          (*d)++;
          break;
        case CORE_DEFINE:
          if ((sexp_core_code(o1) == CORE_DEFINE)
              && SEXP_PAIRP(SEXP_CADR(obj))) {
            o2 = SEXP_CAR(SEXP_CADR(obj));
            analyze_lambda(SEXP_CAR(SEXP_CADR(obj)),
                           SEXP_CDR(SEXP_CADR(obj)),
                           SEXP_CDDR(obj),
                           bc, i, e, params, fv, sv, d, 0);
          } else {
            o2 = SEXP_CADR(obj);
            analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d, 0);
          }
          if (sexp_env_global_p(e)) {
            emit(bc, i, OP_GLOBAL_SET);
            emit_word(bc, i, (sexp_uint_t) o2);
            emit_push(bc, i, SEXP_UNDEF);
          } else {
            o1 = env_cell(e, o2);
            if (! o1)
              errx(1, "define in bad position: %p", o2);
            emit(bc, i, OP_STACK_SET);
            emit_word(bc, i, sexp_unbox_integer(SEXP_CDR(o1)));
          }
          (*d)++;
          break;
        case CORE_SET:
          analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d, 0);
          if (sexp_list_index(sv, SEXP_CADR(obj)) >= 0) {
            analyze_var_ref(SEXP_CADR(obj), bc, i, e, params, fv, SEXP_NULL, d);
            emit(bc, i, OP_SET_CAR);
          } else {
            emit(bc, i, OP_GLOBAL_SET);
            emit_word(bc, i, (sexp_uint_t) SEXP_CADR(obj));
            emit_push(bc, i, SEXP_UNDEF);
          }
          break;
        case CORE_BEGIN:
          for (o2 = SEXP_CDR(obj); SEXP_PAIRP(o2); o2 = SEXP_CDR(o2)) {
            if (SEXP_PAIRP(SEXP_CDR(o2))) {
              analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d, 0);
              emit(bc, i, OP_DROP);
              (*d)--;
            } else
              analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d, tailp);
          }
          break;
        case CORE_IF:
          analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d, 0);
          emit(bc, i, OP_JUMP_UNLESS);              /* jumps if test fails */
          (*d)--;
          tmp1 = *i;
          emit(bc, i, 0);
          analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d, tailp);
          emit(bc, i, OP_JUMP);
          (*d)--;
          tmp2 = *i;
          emit(bc, i, 0);
          /* ((signed char*) (*bc)->data)[tmp1] = (*i)-tmp1;    /\* patch *\/ */
          ((signed char*) sexp_bytecode_data(*bc))[tmp1] = (*i)-tmp1;
          if (SEXP_PAIRP(SEXP_CDDDR(obj))) {
            analyze(SEXP_CADDDR(obj), bc, i, e, params, fv, sv, d, tailp);
          } else {
            emit_push(bc, i, SEXP_UNDEF);
            (*d)++;
          }
          /* ((signed char*) (*bc)->data)[tmp2] = (*i)-tmp2;    /\* patch *\/ */
          ((signed char*) sexp_bytecode_data(*bc))[tmp2] = (*i)-tmp2;
          break;
        case CORE_QUOTE:
          emit_push(bc, i, SEXP_CADR(obj));
          (*d)++;
          break;
        default:
          errx(1, "unknown core form: %s", sexp_core_code(o1));
        }
      } else if (SEXP_OPCODEP(o1)) {
        analyze_opcode(o1, obj, bc, i, e, params, fv, sv, d, tailp);
      } else if (SEXP_MACROP(o1)) {
        obj = sexp_expand_macro(o1, obj, e);
        goto loop;
      } else {
        /* general procedure call */
        analyze_app(obj, bc, i, e, params, fv, sv, d, tailp);
      }
    } else if (SEXP_PAIRP(SEXP_CAR(obj))) {
#if USE_FAST_LET
      o2 = env_cell(e, SEXP_CAAR(obj));
      if (o2
          && SEXP_COREP(SEXP_CDR(o2))
          && (sexp_core_code(o2) == CORE_LAMBDA)
          && sexp_listp(SEXP_CADR(SEXP_CAR(obj)))) {
        /* let */
        tmp1 = sexp_unbox_integer(sexp_length(SEXP_CADR(SEXP_CAR(obj))));
        e2 = extend_env_closure(e, SEXP_CADR(SEXP_CAR(obj)), (*d)+(tmp1-1));
        for (o2=sexp_reverse(SEXP_CDR(obj)); SEXP_PAIRP(o2); o2=SEXP_CDR(o2))
          analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d, 0);
        params = sexp_append(SEXP_CADR(SEXP_CAR(obj)), params);
        for (o2=SEXP_CDDR(SEXP_CAR(obj)); SEXP_PAIRP(o2); o2=SEXP_CDR(o2)) {
          if (SEXP_PAIRP(SEXP_CDR(o2))) {
            analyze(SEXP_CAR(o2), bc, i, e2, params, fv, sv, d, 0);
            emit(bc, i, OP_DROP);
            (*d)--;
          } else {
            analyze(SEXP_CAR(o2), bc, i, e2, params, fv, sv, d, tailp);
          }
        }
        emit(bc, i, OP_STACK_SET);
        emit_word(bc, i, tmp1+1);
        (*d) -= (tmp1-1);
        for (tmp1; tmp1>0; tmp1--)
          emit(bc, i, OP_DROP);
      } else
#endif
        /* computed application */
        analyze_app(obj, bc, i, e, params, fv, sv, d, tailp);
    } else {
      errx(1, "invalid operator: %s", SEXP_CAR(obj));
    }
  } else if (SEXP_SYMBOLP(obj)) {
    analyze_var_ref(obj, bc, i, e, params, fv, sv, d);
  } else {                      /* literal */
    emit_push(bc, i, obj);
    (*d)++;
  }
}

void analyze_opcode (sexp op, sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
                     sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp)
{
  int tmp1;
  sexp o1;

  switch (sexp_opcode_class(op)) {
  case OPC_TYPE_PREDICATE:
  case OPC_PREDICATE:
  case OPC_ARITHMETIC:
  case OPC_ARITHMETIC_INV:
  case OPC_ARITHMETIC_CMP:
  case OPC_CONSTRUCTOR:
  case OPC_ACCESSOR:
  case OPC_GENERIC:
    tmp1 = sexp_unbox_integer(sexp_length(SEXP_CDR(obj)));
    if (tmp1 == 0) {
      errx(1, "opcode with no arguments: %s", sexp_opcode_name(op));
    } else if (tmp1 == 1) {
      analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d, 0);
      if (sexp_opcode_class(op) == OPC_ARITHMETIC_INV) {
        emit(bc, i, sexp_opcode_inverse(op));
        (*d)++;
      } else if (sexp_opcode_class(op) != OPC_ARITHMETIC) {
        emit(bc, i, sexp_opcode_code(op));
      }
    } else {
      for (o1=sexp_reverse(SEXP_CDR(obj)); SEXP_PAIRP(o1); o1=SEXP_CDR(o1))
        analyze(SEXP_CAR(o1), bc, i, e, params, fv, sv, d, 0);
      emit(bc, i, sexp_opcode_code(op));
      (*d) -= (tmp1-1);
      if (sexp_opcode_class(op) == OPC_ARITHMETIC)
        for (tmp1-=2; tmp1>0; tmp1--)
          emit(bc, i, sexp_opcode_code(op));
    }
    break;
  case OPC_IO:
    tmp1 = sexp_unbox_integer(sexp_length(SEXP_CDR(obj)));
    if (tmp1 == sexp_opcode_num_args(op) && sexp_opcode_variadic_p(op)) {
      emit(bc, i, OP_PARAMETER);
      emit_word(bc, i, (sexp_uint_t) sexp_opcode_data(op));
      (*d)++;
      tmp1++;
    }
    for (o1=sexp_reverse(SEXP_CDR(obj)); SEXP_PAIRP(o1); o1=SEXP_CDR(o1))
      analyze(SEXP_CAR(o1), bc, i, e, params, fv, sv, d, 0);
    emit(bc, i, sexp_opcode_code(op));
    (*d) -= (tmp1-1);
    break;
  case OPC_PARAMETER:
    emit(bc, i, sexp_opcode_code(op));
    emit_word(bc, i, (sexp_uint_t) sexp_opcode_data(op));
    break;
  case OPC_FOREIGN:
    for (o1=sexp_reverse(SEXP_CDR(obj)); SEXP_PAIRP(o1); o1=SEXP_CDR(o1))
      analyze(SEXP_CAR(o1), bc, i, e, params, fv, sv, d, 0);
    emit_push(bc, i, sexp_opcode_data(op));
    emit(bc, i, sexp_opcode_code(op));
    (*d) -= (sexp_unbox_integer(sexp_length(SEXP_CDR(obj)))-1);
    break;
  default:
    errx(1, "unknown opcode class: %d", sexp_opcode_class(op));
  }
}

void analyze_var_ref (sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
                      sexp params, sexp fv, sexp sv, sexp_uint_t *d) {
  int tmp;
  sexp o1;
/*   fprintf(stderr, "symbol lookup, param length: %d sv: ", length(params)); */
/*   sexp_write(sv, stderr); */
/*   fprintf(stderr, "\n"); */
  if ((tmp = sexp_list_index(params, obj)) >= 0) {
    o1 = env_cell(e, obj);
    fprintf(stderr, "compiling local ref: ");
    sexp_write(obj, cur_error_port);
    fprintf(stderr, " => %d\n", *d - sexp_unbox_integer(SEXP_CDR(o1)));
    emit(bc, i, OP_STACK_REF);
    emit_word(bc, i, *d - sexp_unbox_integer(SEXP_CDR(o1)));
  } else if ((tmp = sexp_list_index(fv, obj)) >= 0) {
    fprintf(stderr, "compiling closure ref: %p => %d\n", obj, tmp);
    emit(bc, i, OP_CLOSURE_REF);
    emit_word(bc, i, (sexp_uint_t) sexp_make_integer(tmp));
  } else {
    fprintf(stderr, "compiling global ref: %p\n", obj);
    emit(bc, i, OP_GLOBAL_REF);
    emit_word(bc, i, (sexp_uint_t) obj);
  }
  (*d)++;
  if (sexp_list_index(sv, obj) >= 0) {
    /* fprintf(stderr, "mutable variable, fetching CAR\n"); */
    emit(bc, i, OP_CAR);
  }
}

void analyze_app (sexp obj, sexp *bc, sexp_uint_t *i, sexp e,
                  sexp params, sexp fv, sexp sv, sexp_uint_t *d, int tailp) {
  sexp o1;
  sexp_uint_t len = sexp_unbox_integer(sexp_length(SEXP_CDR(obj)));

  /* push the arguments onto the stack */
  for (o1 = sexp_reverse(SEXP_CDR(obj)); SEXP_PAIRP(o1); o1 = SEXP_CDR(o1)) {
    analyze(SEXP_CAR(o1), bc, i, e, params, fv, sv, d, 0);
  }

  /* push the operator onto the stack */
  analyze(SEXP_CAR(obj), bc, i, e, params, fv, sv, d, 0);

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
}

sexp free_vars (sexp e, sexp formals, sexp obj, sexp fv) {
  sexp o1;
  if (SEXP_SYMBOLP(obj)) {
    if (env_global_p(e, obj)
        || (sexp_list_index(formals, obj) >= 0)
        || (sexp_list_index(fv, obj) >= 0))
      return fv;
    else
      return sexp_cons(obj, fv);
  } else if (SEXP_PAIRP(obj)) {
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      if ((o1 = env_cell(e, SEXP_CAR(obj)))
          && SEXP_COREP(o1)
          && (sexp_core_code(SEXP_CDR(o1)) == CORE_LAMBDA)) {
        return free_vars(e, SEXP_CADR(obj), SEXP_CADDR(obj), fv);
      }
    }
    while (SEXP_PAIRP(obj)) {
      fv = free_vars(e, formals, SEXP_CAR(obj), fv);
      obj = SEXP_CDR(obj);
    }
    return fv;
  } else {
    return fv;
  }
}

sexp set_vars (sexp e, sexp formals, sexp obj, sexp sv) {
  sexp tmp;
  if (SEXP_NULLP(formals))
    return sv;
  if (SEXP_PAIRP(obj)) {
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      if ((tmp = env_cell(e, SEXP_CAR(obj))) && SEXP_COREP(SEXP_CDR(tmp))) {
        if (sexp_core_code(SEXP_CDR(tmp)) == CORE_LAMBDA) {
          formals = sexp_lset_diff(formals, SEXP_CADR(obj));
          return set_vars(e, formals, SEXP_CADDR(obj), sv);
        } else if (sexp_core_code(SEXP_CDR(tmp)) == CORE_SET
                   && (sexp_list_index(formals, SEXP_CADR(obj)) >= 0)
                   && ! (sexp_list_index(sv, SEXP_CADR(obj)) >= 0)) {
          sv = sexp_cons(SEXP_CADR(obj), sv);
          return set_vars(e, formals, SEXP_CADDR(obj), sv);
        }
      }
    }
    while (SEXP_PAIRP(obj)) {
      sv = set_vars(e, formals, SEXP_CAR(obj), sv);
      obj = SEXP_CDR(obj);
    }
  }
  return sv;
}

void analyze_lambda (sexp name, sexp formals, sexp body,
                     sexp *bc, sexp_uint_t *i, sexp e,
                     sexp params, sexp fv, sexp sv, sexp_uint_t *d,
                     int tailp) {
  sexp obj, ls, flat_formals, fv2, e2;
  int k;
  flat_formals = sexp_flatten_dot(formals);
  fv2 = free_vars(e, flat_formals, body, SEXP_NULL);
  e2 = extend_env_closure(e, flat_formals, -4);
/*   fprintf(stderr, "%d free-vars\n", sexp_length(fv2)); */
/*   sexp_write(fv2, cur_error_port); */
/*   fprintf(stderr, "\n"); */
  /* compile the body with respect to the new params */
  obj = compile(flat_formals, body, e2, fv2, sv, 0);
  /* push the closed vars */
  emit_push(bc, i, SEXP_UNDEF);
  emit_push(bc, i, sexp_length(fv2));
  emit(bc, i, OP_MAKE_VECTOR);
  (*d)++;
  for (ls=fv2, k=0; SEXP_PAIRP(ls); ls=SEXP_CDR(ls), k++) {
    analyze_var_ref(SEXP_CAR(ls), bc, i, e, params, fv, SEXP_NULL, d);
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
  sexp_uint_t i = 0, j, d = 0, core, define_ok=1;
  sexp bc = (sexp) SEXP_ALLOC(sexp_sizeof(bytecode)+INIT_BCODE_SIZE);
  sexp sv2 = set_vars(e, params, obj, SEXP_NULL), internals=SEXP_NULL, ls;
  bc->tag = SEXP_BYTECODE;
  sexp_bytecode_length(bc) = INIT_BCODE_SIZE;
  sexp_debug("set-vars: ", sv2);
  /* box mutable vars */
  for (ls=params; SEXP_PAIRP(ls); ls=SEXP_CDR(ls)) {
    if ((j = sexp_list_index(sv2, SEXP_CAR(ls)) >= 0)) {
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
    for (ls=SEXP_NULL; SEXP_PAIRP(obj); obj=SEXP_CDR(obj)) {
      core = (SEXP_PAIRP(SEXP_CAR(obj)) && SEXP_SYMBOLP(SEXP_CAAR(obj))
              ? core_code(e, SEXP_CAAR(obj)) : 0);
      if (core == CORE_BEGIN) {
        obj = sexp_cons(SEXP_CAR(obj),
                        sexp_append(SEXP_CDAR(obj), SEXP_CDR(obj)));
      } else {
        if (core == CORE_DEFINE) {
          if (! define_ok)
            errx(1, "definition in non-definition context: %p", obj);
          internals = sexp_cons(SEXP_PAIRP(SEXP_CADAR(obj))
                                ? SEXP_CAR(SEXP_CADAR(obj)) : SEXP_CADAR(obj),
                                internals);
        } else {
          define_ok = 0;
        }
        ls = sexp_cons(SEXP_CAR(obj), ls);
      }
    }
    obj = sexp_reverse(ls);
    j = sexp_unbox_integer(sexp_length(internals));
    if (SEXP_PAIRP(internals)) {
/*       sexp_write_string("internals: ", cur_error_port); */
/*       sexp_write(internals, cur_error_port); */
/*       sexp_write_string("\n", cur_error_port); */
      e = extend_env_closure(e, internals, 2);
      params = sexp_append(internals, params);
      for (ls=internals; SEXP_PAIRP(ls); ls=SEXP_CDR(ls))
        emit_push(&bc, &i, (sexp_uint_t) SEXP_UNDEF);
      d+=j;
    }
  }
  /* analyze body sequence */
  for ( ; SEXP_PAIRP(obj); obj=SEXP_CDR(obj)) {
    if (SEXP_PAIRP(SEXP_CDR(obj))) {
      analyze(SEXP_CAR(obj), &bc, &i, e, params, fv, sv, &d, 0);
      emit(&bc, &i, OP_DROP);
      d--;
    } else {
      analyze(SEXP_CAR(obj), &bc, &i, e, params, fv, sv, &d,
              (! done_p) && (! SEXP_PAIRP(internals))
              );
    }
  }
  if (SEXP_PAIRP(internals)) {
    emit(&bc, &i, OP_STACK_SET);
    emit_word(&bc, &i, j+1);
    for (j; j>0; j--)
      emit(&bc, &i, OP_DROP);
  }
  emit(&bc, &i, done_p ? OP_DONE : OP_RET);
  shrink_bcode(&bc, i);
  print_bytecode(bc);
  disasm(bc);
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

#define sexp_raise(exn) {stack[top++]=(exn); goto call_error_handler;}

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
    if (! tmp1)
      sexp_raise(sexp_intern("undefined-variable"));
/*     fprintf(stderr, "global-ref: "); */
/*     sexp_write(((sexp*)ip)[0], cur_error_port); */
/*     fprintf(stderr, " => "); */
/*     sexp_write(SEXP_CDR(tmp1), cur_error_port); */
/*     fprintf(stderr, "\n"); */
    stack[top++]=SEXP_CDR(tmp1);
    ip += sizeof(sexp);
    break;
  case OP_GLOBAL_SET:
    env_define(e, ((sexp*)ip)[0], stack[--top]);
    ip += sizeof(sexp);
    break;
  case OP_STACK_REF:
/*     fprintf(stderr, "stack-ref: %d => ", (sexp_sint_t) ((sexp*)ip)[0]); */
/*     sexp_write(stack[top - (sexp_sint_t) ((sexp*)ip)[0]], cur_error_port); */
/*     fprintf(stderr, "\n"); */
/*     print_stack(stack, top); */
    stack[top] = stack[top - (sexp_sint_t) ((sexp*)ip)[0]];
    ip += sizeof(sexp);
    top++;
    break;
  case OP_STACK_SET:
/*     print_stack(stack, top); */
/*     fprintf(stderr, "stack-set: %d => ", (sexp_sint_t) ((sexp*)ip)[0]); */
/*     sexp_write(stack[top-1], cur_error_port); */
/*     fprintf(stderr, "\n"); */
    stack[top - (sexp_sint_t) ((sexp*)ip)[0]] = stack[top-1];
    stack[top-1] = SEXP_UNDEF;
/*     print_stack(stack, top); */
    ip += sizeof(sexp);
    break;
  case OP_CLOSURE_REF:
/*     fprintf(stderr, "closure-ref: %d => ", sexp_unbox_integer(((sexp*)ip)[0])); */
/*     sexp_write(sexp_vector_ref(cp, ((sexp*)ip)[0]), cur_error_port); */
/*     fprintf(stderr, "\n"); */
    stack[top++]=sexp_vector_ref(cp, ((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_VECTOR_REF:
    stack[top-2]=sexp_vector_ref(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_VECTOR_SET:
    sexp_vector_set(stack[top-1], stack[top-2], stack[top-3]);
    stack[top-3]=SEXP_UNDEF;
/*     fprintf(stderr, "vector-set: %d => ", sexp_unbox_integer(stack[top-2])); */
/*     sexp_write(stack[top-1], cur_error_port); */
/*     fprintf(stderr, "\n"); */
    top-=2;
    break;
  case OP_STRING_REF:
    stack[top-2]=sexp_string_ref(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_STRING_SET:
    sexp_string_set(stack[top-1], stack[top-2], stack[top-3]);
    stack[top-3]=SEXP_UNDEF;
    top-=2;
    break;
  case OP_MAKE_PROCEDURE:
    stack[top-4]=sexp_make_procedure((int) stack[top-1], (int) stack[top-2], stack[top-3], stack[top-4]);
    top-=3;
    break;
  case OP_MAKE_VECTOR:
    stack[top-2]=sexp_make_vector(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_PUSH:
/*     fprintf(stderr, "pushing: "); */
/*     sexp_write(((sexp*)ip)[0], cur_error_port); */
/*     fprintf(stderr, "\n"); */
    stack[top++]=((sexp*)ip)[0];
    ip += sizeof(sexp);
    break;
  case OP_DROP:
    top--;
    break;
  case OP_PARAMETER:
    stack[top] = *(sexp*)((sexp*)ip)[0];
    top++;
    ip += sizeof(sexp);
    break;
  case OP_PAIRP:
    stack[top-1]=SEXP_PAIRP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_NULLP:
    stack[top-1]=SEXP_NULLP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_CHARP:
    stack[top-1]=SEXP_CHARP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_INTEGERP:
    stack[top-1]=SEXP_INTEGERP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_SYMBOLP:
    stack[top-1]=SEXP_SYMBOLP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_STRINGP:
    stack[top-1]=SEXP_STRINGP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_VECTORP:
    stack[top-1]=SEXP_VECTORP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_PROCEDUREP:
    stack[top-1]=SEXP_PROCEDUREP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_IPORTP:
    stack[top-1]=SEXP_IPORTP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_OPORTP:
    stack[top-1]=SEXP_OPORTP(stack[top-1]) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_EOFP:
    stack[top-1]=(stack[top-1] == SEXP_EOF) ? SEXP_TRUE : SEXP_FALSE; break;
  case OP_CAR:
    /* print_stack(stack, top); */
    if (! SEXP_PAIRP(stack[top-1])) sexp_raise(sexp_intern("not-a-pair"));
    stack[top-1]=SEXP_CAR(stack[top-1]); break;
  case OP_CDR:
    if (! SEXP_PAIRP(stack[top-1])) sexp_raise(sexp_intern("not-a-pair"));
    stack[top-1]=SEXP_CDR(stack[top-1]); break;
  case OP_SET_CAR:
    if (! SEXP_PAIRP(stack[top-1])) sexp_raise(sexp_intern("not-a-pair"));
    SEXP_CAR(stack[top-1]) = stack[top-2];
    stack[top-2]=SEXP_UNDEF;
    top--;
    break;
  case OP_SET_CDR:
    if (! SEXP_PAIRP(stack[top-1])) sexp_raise(sexp_intern("not-a-pair"));
    SEXP_CDR(stack[top-1]) = stack[top-2];
    stack[top-2]=SEXP_UNDEF;
    top--;
    break;
  case OP_CONS:
    stack[top-2]=sexp_cons(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_ADD:
    stack[top-2]=sexp_add(stack[top-1],stack[top-2]);
    top--;
    break;
  case OP_SUB:
    stack[top-2]=sexp_sub(stack[top-1],stack[top-2]);
    top--;
    break;
  case OP_MUL:
    stack[top-2]=sexp_mul(stack[top-1],stack[top-2]);
    top--;
    break;
  case OP_DIV:
    stack[top-2]=sexp_div(stack[top-1],stack[top-2]);
    top--;
    break;
  case OP_MOD:
    stack[top-2]=sexp_mod(stack[top-1],stack[top-2]);
    top--;
    break;
  case OP_LT:
    stack[top-2]=((stack[top-1] < stack[top-2]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_LE:
    stack[top-2]=((stack[top-1] <= stack[top-2]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_GT:
    stack[top-2]=((stack[top-1] > stack[top-2]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_GE:
    stack[top-2]=((stack[top-1] >= stack[top-2]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_EQ:
  case OP_EQN:
    stack[top-2]=((stack[top-1] == stack[top-2]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_TAIL_CALL:
    /* old-args ... n ret-ip ret-cp new-args ...   proc  */
    /* [================= j ===========================] */
    /*                              [==== i =====]       */
    j = sexp_unbox_integer(((sexp*)ip)[0]);    /* current depth */
    i = sexp_unbox_integer(((sexp*)ip)[1]);    /* number of params */
    tmp1 = stack[top-1];                       /* procedure to call */
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
      errx(1, "out of stack space: %d", top);
    i = sexp_unbox_integer(((sexp*)ip)[0]);
    tmp1 = stack[top-1];
  make_call:
    if (SEXP_OPCODEP(tmp1))
      /* hack, compile an opcode application on the fly */
      tmp1 = make_opcode_procedure(tmp1, i, e);
    /* print_stack(stack, top); */
    if (! SEXP_PROCEDUREP(tmp1)) {
      fprintf(stderr, "error: non-procedure app: ");
      sexp_write(tmp1, cur_error_port);
      fprintf(stderr, "\n");
      sexp_raise(sexp_intern("non-procedure-application"));
    }
    j = i - sexp_unbox_integer(sexp_procedure_num_args(tmp1));
    if (j < 0) {
      fprintf(stderr, "error: expected %d args but got %d\n",
              sexp_unbox_integer(sexp_procedure_num_args(tmp1)),
              i);
      sexp_raise(sexp_intern("not-enough-args"));
    }
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
        fprintf(stderr, "got: %d, expected: %d\n", i, sexp_procedure_num_args(tmp1));
        sexp_raise(sexp_intern("too-many-args"));
      }
    } else if (sexp_procedure_variadic_p(tmp1)) {
      /* shift stack, set extra arg to null */
      for (k=top; k>=top-i; k--)
        stack[k] = stack[k-1];
      stack[top-i-1] = SEXP_NULL;
      top++;
      i++;
    }
    stack[top-1] = sexp_make_integer(i);
    stack[top] = sexp_make_integer(ip+sizeof(sexp));
    stack[top+1] = cp;
    top+=2;
/*     sexp_debug("call proc: ", tmp1); */
/*     sexp_debug("bc: ", sexp_procedure_code(tmp1)); */
/*     fprintf(stderr, "data: %p\n", sexp_procedure_code(tmp1)->data); */
    bc = sexp_procedure_code(tmp1);
/*     print_bytecode(bc); */
/*     disasm(bc); */
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
/*     fprintf(stderr, "... calling procedure at %p\ncp: ", ip); */
/*     /\* sexp_write(cp, stderr); *\/ */
/*     fprintf(stderr, "\n"); */
    /* fprintf(stderr, "stack at %d\n", top); */
    /* print_stack(stack, top); */
    break;
  case OP_APPLY1:
    /* print_stack(stack, top); */
    tmp1 = stack[top-1];
    tmp2 = stack[top-2];
    i = sexp_unbox_integer(sexp_length(tmp2));
    top += (i-2);
    for ( ; SEXP_PAIRP(tmp2); tmp2=SEXP_CDR(tmp2), top--)
      stack[top-1] = SEXP_CAR(tmp2);
    top += i+1;
    ip -= sizeof(sexp);
    goto make_call;
  case OP_CALLCC:
    tmp1 = stack[top-1];
    if (! SEXP_PROCEDUREP(tmp1))
      errx(2, "non-procedure application: %p", tmp1);
    stack[top] = sexp_make_integer(1);
    stack[top+1] = sexp_make_integer(ip);
    stack[top+2] = cp;
/*     fprintf(stderr, "saved: ", top); */
/*     sexp_write(tmp2, cur_error_port); */
/*     fprintf(stderr, "\n", top); */
    stack[top-1] = sexp_make_procedure(0, (int) sexp_make_integer(1),
                                       continuation_resumer,
                                       sexp_vector(1, sexp_save_stack(stack, top+3)));
    top+=3;
    bc = sexp_procedure_code(tmp1);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
    break;
  case OP_RESUMECC:
/*     fprintf(stderr, "resuming continuation (%d)\n", top); */
/*     print_stack(stack, top); */
/*     sexp_write(sexp_vector_ref(cp, 0), cur_error_port); */
/*     fprintf(stderr, "\n"); */
    tmp1 = stack[top-4];
    top = sexp_restore_stack(sexp_vector_ref(cp, 0), stack);
/*     fprintf(stderr, "... restored stack (%d):\n", top); */
/*     print_stack(stack, top); */
    cp = stack[top-1];
    ip = (unsigned char*) sexp_unbox_integer(stack[top-2]);
    i = sexp_unbox_integer(stack[top-3]);
    top -= 3;
    stack[top-1] = tmp1;
    break;
  case OP_ERROR:
  call_error_handler:
    fprintf(stderr, "in error handler\n");
    sexp_write_string("ERROR: ", cur_error_port);
    sexp_write(stack[top-1], cur_error_port);
    sexp_write_string("\n", cur_error_port);
    tmp1 = SEXP_CDR(exception_handler_cell);
    stack[top-1] = SEXP_UNDEF;
    stack[top] = (sexp) 1;
    stack[top+1] = sexp_make_integer(ip+4);
    stack[top+2] = cp;
    top+=3;
    bc = sexp_procedure_code(tmp1);
    ip = sexp_bytecode_data(bc);
    cp = sexp_procedure_vars(tmp1);
    break;
  case OP_FCALL0:
    stack[top-1]=((sexp_proc0)stack[top-1])();
    break;
  case OP_FCALL1:
    stack[top-2]=((sexp_proc1)stack[top-1])(stack[top-2]);
    top--;
    break;
  case OP_FCALL2:
    stack[top-3]=((sexp_proc2)stack[top-1])(stack[top-2],stack[top-3]);
    top-=2;
    break;
  case OP_FCALL3:
    stack[top-4]=((sexp_proc3)stack[top-1])(stack[top-2],stack[top-3],stack[top-4]);
    top-=3;
    break;
  case OP_JUMP_UNLESS:
    /* fprintf(stderr, "JUMP UNLESS, stack top is %d\n", stack[top-1]); */
    if (stack[--top] == SEXP_FALSE) {
      /* fprintf(stderr, "test failed, jumping to + %d => %p\n", ((signed char*)ip)[0], ip + ((signed char*)ip)[0]); */
      ip += ((signed char*)ip)[0];
    } else {
      /* fprintf(stderr, "test passed, not jumping\n"); */
      ip++;
    }
    break;
  case OP_JUMP:
    /* fprintf(stderr, "jumping to + %d => %p\n", ((signed char*)ip)[0], ip + ((signed char*)ip)[0]); */
    ip += ((signed char*)ip)[0];
    break;
  case OP_DISPLAY:
    if (SEXP_STRINGP(stack[top-1])) {
      sexp_write_string(sexp_string_data(stack[top-1]), stack[top-2]);
      break;
    }
  case OP_WRITE:
    sexp_write(stack[top-1], stack[top-2]);
    stack[top-2] = SEXP_UNDEF;
    top--;
    break;
  case OP_WRITE_CHAR:
    sexp_write_char(sexp_unbox_character(stack[top-1]), stack[top-2]);
    break;
  case OP_NEWLINE:
    sexp_write_char('\n', stack[top-1]);
    stack[top-1] = SEXP_UNDEF;
    break;
  case OP_FLUSH_OUTPUT:
    sexp_flush(stack[top-1]);
    stack[top-1] = SEXP_UNDEF;
    break;
  case OP_READ:
    stack[top-1] = sexp_read(stack[top-1]);
    if (stack[top-1] == SEXP_ERROR) sexp_raise(sexp_intern("read-error"));
    break;
  case OP_READ_CHAR:
    i = sexp_read_char(stack[top-1]);
    stack[top-1] = (i == EOF) ? SEXP_EOF : sexp_make_character(i);
    break;
  case OP_RET:
/*     fprintf(stderr, "returning @ %d: ", top-1); */
/*     fflush(stderr); */
/*     sexp_write(stack[top-1], cur_error_port); */
/*     fprintf(stderr, " ...\n"); */
    /* print_stack(stack, top); */
    if (top<4)
      goto end_loop;
    cp = stack[top-2];
    ip = (unsigned char*) sexp_unbox_integer(stack[top-3]);
    i = sexp_unbox_integer(stack[top-4]);
    stack[top-i-4] = stack[top-1];
    top = top-i-3;
/*     fprintf(stderr, "... done returning\n"); */
    break;
  case OP_DONE:
    fprintf(stderr, "finally returning @ %d: ", top-1);
    fflush(stderr);
    sexp_write(stack[top-1], cur_error_port);
    fprintf(stderr, "\n");
    goto end_loop;
  default:
    fprintf(stderr, "unknown opcode: %d\n", *(ip-1));
    stack[top] = SEXP_ERROR;
    goto end_loop;
  }
  /* print_stack(stack, top); */
  goto loop;

 end_loop:
  return stack[top-1];
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
  sexp obj, *stack=SEXP_ALLOC(sizeof(sexp)*INIT_STACK_SIZE);
  int closep = 0;
  if (SEXP_STRINGP(source)) {
    source = sexp_open_input_file(source);
    closep = 1;
  }
  while ((obj=sexp_read(source)) != (sexp) SEXP_EOF)
    eval_in_stack(obj, interaction_environment, stack, 0);
  if (closep) sexp_close_port(source);
  SEXP_FREE(stack);
  return SEXP_UNDEF;
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
  sexp bc, e, obj, res, in, out, *stack, err_handler, err_handler_sym;
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

