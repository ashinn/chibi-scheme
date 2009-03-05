/*  eval.c -- evaluator library implementation           */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "eval.h"

/************************************************************************/

static int scheme_initialized_p = 0;

static sexp cur_input_port, cur_output_port, cur_error_port;
static sexp exception_handler;

#ifdef USE_DEBUG
#include "debug.c"
#else
#define print_stack(...)
#define print_bytecode(...)
#define disasm(...)
#endif

/********************** environment utilities ***************************/
sexp env_cell(env e, sexp key) {
  sexp ls, res=NULL;

  do {
    for (ls=e->bindings; SEXP_PAIRP(ls); ls=SEXP_CDR(ls)) {
      if (SEXP_CAAR(ls) == key) {
        res = SEXP_CAR(ls);
        break;
      }
    }
    e = e->parent;
  } while (e && ! res);

  return res;
}

int env_global_p (env e, sexp id) {
  while (e->parent) {
    if (sexp_assq(id, e->bindings) != SEXP_FALSE)
      return 0;
    else
      e = e->parent;
  }
  return 1;
}

void env_define(env e, sexp key, sexp value) {
  sexp cell = env_cell(e, key);
  if (cell) {
    SEXP_CDR(cell) = value;
  } else {
    e->bindings = sexp_cons(sexp_cons(key, value), e->bindings);
  }
}

env extend_env_closure (env e, sexp fv) {
  int i;
  env e2 = (env) SEXP_ALLOC(sizeof(struct env));
  e2->tag = SEXP_ENV;
  e2->parent = e;
  e2->bindings = SEXP_NULL;
  for (i=0; SEXP_PAIRP(fv); fv = SEXP_CDR(fv), i++) {
    e2->bindings = sexp_cons(sexp_cons(SEXP_CAR(fv), sexp_make_integer(i)),
                             e2->bindings);
  }
  return e2;
}

/************************* bytecode utilities ***************************/

void shrink_bcode(bytecode *bc, unsigned int i) {
  bytecode tmp;
  if ((*bc)->len != i) {
    /* fprintf(stderr, "shrinking to %d\n", i); */
    tmp = (bytecode) SEXP_ALLOC(sizeof(struct bytecode) + i);
    tmp->tag = SEXP_BYTECODE;
    tmp->len = i;
    memcpy(tmp->data, (*bc)->data, i);
    SEXP_FREE(*bc);
    *bc = tmp;
  }
}

void emit(bytecode *bc, unsigned int *i, char c)  {
  bytecode tmp;
  if ((*bc)->len < (*i)+1) {
    /* fprintf(stderr, "expanding (%d < %d)\n", (*bc)->len, (*i)+1); */
    tmp = (bytecode) SEXP_ALLOC(sizeof(unsigned int) + (*bc)->len*2);
    tmp->len = (*bc)->len*2;
    memcpy(tmp->data, (*bc)->data, (*bc)->len);
    SEXP_FREE(*bc);
    *bc = tmp;
  }
  (*bc)->data[(*i)++] = c;
}

void emit_word(bytecode *bc, unsigned int *i, sexp_uint_t val)  {
  bytecode tmp;
  if ((*bc)->len < (*i)+4) {
    tmp = (bytecode) SEXP_ALLOC(sizeof(unsigned int) + (*bc)->len*2);
    tmp->len = (*bc)->len*2;
    memcpy(tmp->data, (*bc)->data, (*bc)->len);
    SEXP_FREE(*bc);
    *bc = tmp;
  }
  *((unsigned long*)(&((*bc)->data[*i]))) = val;
  *i += sizeof(unsigned long);
}

#define emit_push(bc,i,obj) (emit(bc,i,OP_PUSH), emit_word(bc,i,(sexp_uint_t)obj))

sexp sexp_make_procedure(sexp bc, sexp vars) {
  sexp proc = SEXP_NEW();
  if (! proc) return SEXP_ERROR;
  proc->tag = SEXP_PROCEDURE;
  proc->data1 = (void*) bc;
  proc->data2 = (void*) vars;
  return proc;
}

/************************* the compiler ***************************/

void analyze(sexp obj, bytecode *bc, unsigned int *i, env e,
             sexp params, sexp fv, sexp sv, unsigned int *d) {
  int tmp1, tmp2;
  env e2 = e;
  sexp o1, o2, cell;

  if (SEXP_PAIRP(obj)) {
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      o1 = env_cell(e, SEXP_CAR(obj));
      if (! o1)
        errx(1, "unknown operator: %s", SEXP_CAR(obj));
      o1 = SEXP_CDR(o1);
      if (SEXP_COREP(o1)) {
        switch (((core_form)o1)->code) {
        case CORE_LAMBDA:
          analyze_lambda(SEXP_FALSE, SEXP_CADR(obj), SEXP_CDDR(obj),
                         bc, i, e, params, fv, sv, d);
          break;
        case CORE_DEFINE:
          if ((((core_form)o1)->code == CORE_DEFINE)
              && SEXP_PAIRP(SEXP_CADR(obj))) {
            analyze_lambda(SEXP_CAR(SEXP_CADR(obj)),
                           SEXP_CDR(SEXP_CADR(obj)),
                           SEXP_CDDR(obj),
                           bc, i, e, params, fv, sv, d);
          } else {
            analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d);
          }
          emit(bc, i, OP_GLOBAL_SET);
          emit_word(bc, i, (sexp_uint_t) (SEXP_PAIRP(SEXP_CADR(obj))
                                          ? SEXP_CAR(SEXP_CADR(obj))
                                          : SEXP_CADR(obj)));
          emit_push(bc, i, SEXP_UNDEF);
          (*d)++;
          break;
        case CORE_SET:
          analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d);
          analyze_var_ref(SEXP_CADR(obj), bc, i, e, params, fv, SEXP_NULL, d);
          emit(bc, i, OP_SET_CAR);
          break;
        case CORE_BEGIN:
          for (o2 = SEXP_CDR(obj); SEXP_PAIRP(o2); o2 = SEXP_CDR(o2)) {
            analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d);
            if (SEXP_PAIRP(SEXP_CDR(o2))) emit(bc, i, OP_DROP);
          }
          break;
        case CORE_IF:
          analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
          emit(bc, i, OP_JUMP_UNLESS);              /* jumps if test fails */
          tmp1 = *i;
          emit(bc, i, 0);
          analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d);
          emit(bc, i, OP_JUMP);
          tmp2 = *i;
          emit(bc, i, 0);
          ((signed char*) (*bc)->data)[tmp1] = (*i)-tmp1-1;    /* patch */
          if (SEXP_PAIRP(SEXP_CDDDR(obj))) {
            analyze(SEXP_CADDDR(obj), bc, i, e, params, fv, sv, d);
          } else {
            emit_push(bc, i, SEXP_UNDEF);
            (*d)++;
          }
          ((signed char*) (*bc)->data)[tmp2] = (*i)-tmp2-1;    /* patch */
          break;
        case CORE_QUOTE:
          emit_push(bc, i, SEXP_CADR(obj));
          (*d)++;
          break;
        default:
          errx(1, "unknown core form: %s", ((core_form)o1)->code);
        }
      } else if (SEXP_OPCODEP(o1)) {
        /* verify arity */
        switch (((opcode)o1)->op_class) {
        case OPC_TYPE_PREDICATE:
        case OPC_PREDICATE:
        case OPC_ARITHMETIC:
        case OPC_ARITHMETIC_INV:
        case OPC_ARITHMETIC_CMP:
        case OPC_CONSTRUCTOR:
        case OPC_ACCESSOR:
        case OPC_GENERIC:
          if (SEXP_NULLP(SEXP_CDR(obj))) {
            errx(1, "unknown opcode class: %d", ((opcode)o1)->op_class);
          } else if (SEXP_NULLP(SEXP_CDDR(obj))) {
            if (((opcode)o1)->op_class == OPC_ARITHMETIC_INV) {
              analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
              emit(bc, i, ((opcode)o1)->op_inverse);
            } else {
              analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
              if (((opcode)o1)->op_class != OPC_ARITHMETIC) {
                emit(bc, i, ((opcode)o1)->op_name);
                (*d)--;
              }
            }
          } else {
            for (o2 = sexp_reverse(SEXP_CDR(obj)); SEXP_PAIRP(o2);
                 o2 = SEXP_CDR(o2)) {
              /* fprintf(stderr, ":: arg: %d\n", SEXP_CAR(o2)); */
              analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d);
            }
            emit(bc, i, ((opcode)o1)->op_name);
            (*d) -= sexp_length(SEXP_CDDR(obj));
          }
          break;
        case OPC_FOREIGN:
          for (o2 = sexp_reverse(SEXP_CDR(obj)); SEXP_PAIRP(o2);
               o2 = SEXP_CDR(o2)) {
            analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d);
          }
          emit_push(bc, i, ((opcode)o1)->data);
          emit(bc, i, ((opcode)o1)->op_name);
          (*d) -= sexp_length(SEXP_CDR(obj));
          break;
        default:
          errx(1, "unknown opcode class: %d", ((opcode)o1)->op_class);
        }
      } else {
        /* general procedure call */
        analyze_app(obj, bc, i, e, params, fv, sv, d);
      }
    } else if (SEXP_PAIRP(SEXP_CAR(obj))) {
      o2 = env_cell(e, SEXP_CAAR(obj));
/*       if (o2 */
/*           && SEXP_COREP(SEXP_CDR(o2)) */
/*           && (((core_form)SEXP_CDR(o2))->code == CORE_LAMBDA)) { */
/*         /\* let *\/ */
/*       } else { */
        /* computed application */
      analyze_app(obj, bc, i, e, params, fv, sv, d);
/*       } */
    } else {
      errx(1, "invalid operator: %s", SEXP_CAR(obj));
    }
  } else if (SEXP_SYMBOLP(obj)) {
    analyze_var_ref(obj, bc, i, e, params, fv, sv, d);
  } else {
    emit_push(bc, i, obj);
    (*d)++;
  }
}

void analyze_var_ref (sexp obj, bytecode *bc, unsigned int *i, env e,
                      sexp params, sexp fv, sexp sv, unsigned int *d) {
  int tmp;
/*   fprintf(stderr, "symbol lookup, param length: %d sv: ", length(params)); */
/*   sexp_write(sv, stderr); */
/*   fprintf(stderr, "\n"); */
  if ((tmp = sexp_list_index(params, obj)) >= 0) {
    /* fprintf(stderr, "compiling local ref: %p => %d (d = %d)\n", obj, tmp, *d); */
    emit(bc, i, OP_STACK_REF);
    emit_word(bc, i, tmp + *d + 4);
    (*d)++;
  } else if ((tmp = sexp_list_index(fv, obj)) >= 0) {
    /* fprintf(stderr, "compiling closure ref: %p => %d\n", obj, tmp); */
    emit(bc, i, OP_CLOSURE_REF);
    emit_word(bc, i, tmp);
    (*d)++;
  } else {
    /* fprintf(stderr, "compiling global ref: %p\n", obj); */
    emit(bc, i, OP_GLOBAL_REF);
    emit_word(bc, i, (sexp_uint_t) obj);
    (*d)++;
  }
  if (sexp_list_index(sv, obj) >= 0) {
    /* fprintf(stderr, "mutable variable, fetching CAR\n"); */
    emit(bc, i, OP_CAR);
  }
}

void analyze_app (sexp obj, bytecode *bc, unsigned int *i,
                  env e, sexp params, sexp fv, sexp sv, unsigned int *d) {
  sexp o1;
  unsigned long len = sexp_length(SEXP_CDR(obj));

  /* push the arguments onto the stack */
  for (o1 = sexp_reverse(SEXP_CDR(obj)); SEXP_PAIRP(o1); o1 = SEXP_CDR(o1)) {
    analyze(SEXP_CAR(o1), bc, i, e, params, fv, sv, d);
  }

  /* push the operator onto the stack */
  analyze(SEXP_CAR(obj), bc, i, e, params, fv, sv, d);

  /* make the call */
  emit(bc, i, OP_CALL);
  emit_word(bc, i, (sexp_uint_t) sexp_make_integer(len));
}

sexp free_vars (env e, sexp formals, sexp obj, sexp fv) {
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
          && (((core_form)SEXP_CDR(o1))->code == CORE_LAMBDA)) {
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

sexp set_vars (env e, sexp formals, sexp obj, sexp sv) {
  sexp tmp;
  if (SEXP_NULLP(formals))
    return sv;
  if (SEXP_PAIRP(obj)) {
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      if ((tmp = env_cell(e, SEXP_CAR(obj))) && SEXP_COREP(SEXP_CDR(tmp))) {
        if (((core_form)SEXP_CDR(tmp))->code == CORE_LAMBDA) {
          formals = sexp_lset_diff(formals, SEXP_CADR(obj));
          return set_vars(e, formals, SEXP_CADDR(obj), sv);
        } else if (((core_form)SEXP_CDR(tmp))->code == CORE_SET
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
                     bytecode *bc, unsigned int *i, env e,
                     sexp params, sexp fv, sexp sv, unsigned int *d) {
  sexp obj, ls, fv2 = free_vars(e, formals, body, SEXP_NULL);
  env e2 = extend_env_closure(e, formals);
  int k;
  fprintf(stderr, "%d free-vars\n", sexp_length(fv2));
  sexp_write(fv2, cur_error_port);
  fprintf(stderr, "\n");
  obj = (sexp) compile(formals, body, e2, fv2, sv, 0);
  emit_push(bc, i, SEXP_UNDEF);
  emit_push(bc, i, sexp_make_integer(sexp_length(fv2)));
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
  emit_push(bc, i, obj);
  emit(bc, i, OP_MAKE_PROCEDURE);
}

bytecode compile(sexp params, sexp obj, env e, sexp fv, sexp sv, int done_p) {
  unsigned int i = 0, j, d = 0;
  bytecode bc = (bytecode) SEXP_ALLOC(sizeof(struct bytecode)+INIT_BCODE_SIZE);
  sexp sv2 = set_vars(e, params, obj, SEXP_NULL), ls;
  /* fprintf(stderr, "set-vars: "); sexp_write(sv2, stderr); fprintf(stderr, "\n"); */
  bc->tag = SEXP_BYTECODE;
  bc->len = INIT_BCODE_SIZE;
  /* fprintf(stderr, "analyzing\n"); */
  for (ls=params; SEXP_PAIRP(ls); ls=SEXP_CDR(ls)) {
    if ((j = sexp_list_index(sv2, SEXP_CAR(ls)) >= 0)) {
      /* fprintf(stderr, "consing mutable var\n"); */
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
  for ( ; SEXP_PAIRP(obj); obj=SEXP_CDR(obj)) {
    /* fprintf(stderr, "loop: "); sexp_write(obj, stderr); fprintf(stderr, "\n"); */
    analyze(SEXP_CAR(obj), &bc, &i, e, params, fv, sv, &d);
    if (SEXP_PAIRP(SEXP_CDR(obj))) emit(&bc, &i, OP_DROP);
  }
  emit(&bc, &i, done_p ? OP_DONE : OP_RET);
  shrink_bcode(&bc, i);
  print_bytecode(bc);
  disasm(bc);
  return bc;
}

/************************ library functions ***************************/

sexp sexp_set_car(sexp obj, sexp val) {
  if (SEXP_PAIRP(obj))
    return SEXP_CAR(obj) = val;
  else {
    sexp_debug("error: set-car! not a pair: ", obj);
    return SEXP_ERROR;
  }
}

sexp sexp_set_cdr(sexp obj, sexp val) {
  if (SEXP_PAIRP(obj))
    return SEXP_CDR(obj) = val;
  else
    return SEXP_ERROR;
}

/*********************** the virtual machine **************************/

sexp vm(bytecode bc, env e, sexp* stack, unsigned int top) {
  unsigned char *ip=bc->data;
  sexp cp, tmp1, tmp2;
  int i;

 loop:
  /* fprintf(stderr, "opcode: %s (%d), ip: %d\n", reverse_opcode_names[*ip], *ip, ip); */
  /* print_bytecode(bc); */
  switch (*ip++) {
  case OP_NOOP:
    fprintf(stderr, "noop\n");
    break;
  case OP_GLOBAL_REF:
/*     fprintf(stderr, "global ref: ip: %p => %p: ", ip, ((sexp*)ip)[0]); */
/*     fflush(stderr); */
/*     sexp_write(stderr, ((sexp*)ip)[0]); */
/*     fprintf(stderr, "\n"); */
    tmp1 = env_cell(e, ((sexp*)ip)[0]);
    stack[top++]=SEXP_CDR(tmp1);
    ip += sizeof(sexp);
    break;
  case OP_GLOBAL_SET:
/*     fprintf(stderr, "global set: %p: ", ((sexp*)ip)[0]); */
/*     fflush(stderr); */
/*     sexp_write(stderr, ((sexp*)ip)[0]); */
/*     fprintf(stderr, "\n"); */
    env_define(e, ((sexp*)ip)[0], stack[--top]);
    ip += sizeof(sexp);
    break;
  case OP_STACK_REF:
/*     fprintf(stderr, "stack ref: ip=%p,  %d - %d => ", */
/*             ip, top, (sexp_uint_t) ((sexp*)ip)[0]); */
/*     fflush(stderr); */
/*     sexp_write(stderr, stack[top - (unsigned int) ((sexp*)ip)[0]]); */
/*     fprintf(stderr, "\n"); */
    stack[top] = stack[top - (unsigned int) ((sexp*)ip)[0]];
    ip += sizeof(sexp);
    top++;
    break;
  case OP_STACK_SET:
    stack[top - (sexp_sint_t) ((sexp*)ip)[0]] = stack[top-1];
    stack[top-1] = SEXP_UNDEF;
    ip += sizeof(sexp);
    break;
  case OP_CLOSURE_REF:
/*     fprintf(stderr, "closure-ref %d => ", ((sexp*)ip)[0]); */
/*     fflush(stderr); */
/*     sexp_write(stderr, vector_ref(cp,((sexp*)ip)[0])); */
/*     fprintf(stderr, "\n"); */
    stack[top++]=sexp_vector_ref(cp,((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_VECTOR_REF:
    stack[top-2]=sexp_vector_ref(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_VECTOR_SET:
    sexp_vector_set(stack[top-1], stack[top-2], stack[top-3]);
    stack[top-3]=SEXP_UNDEF;
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
    stack[top-2]=sexp_make_procedure(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_MAKE_VECTOR:
    stack[top-2]=sexp_make_vector(sexp_unbox_integer(stack[top-1]), stack[top-2]);
    top--;
    break;
  case OP_PUSH:
    stack[top++]=((sexp*)ip)[0];
    ip += sizeof(sexp);
    break;
  case OP_DUP:
    stack[top]=stack[top-1];
    top++;
    break;
  case OP_DROP:
    top--;
    break;
  case OP_SWAP:
    tmp1 = stack[top-2];
    stack[top-2]=stack[top-1];
    stack[top-1]=tmp1;
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
    stack[top-1]=sexp_car(stack[top-1]); break;
  case OP_CDR:
    stack[top-1]=sexp_cdr(stack[top-1]); break;
  case OP_SET_CAR:
    sexp_set_car(stack[top-1], stack[top-2]);
    stack[top-2]=SEXP_UNDEF;
    top--;
    break;
  case OP_SET_CDR:
    sexp_set_cdr(stack[top-1], stack[top-2]);
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
    stack[top-2]=sexp_mul(stack[top-2],stack[top-1]);
    top--;
    break;
  case OP_DIV:
    stack[top-2]=sexp_div(stack[top-2],stack[top-1]);
    top--;
    break;
  case OP_MOD:
    stack[top-2]=sexp_mod(stack[top-2],stack[top-1]);
    top--;
    break;
  case OP_LT:
    stack[top-2]=((stack[top-2] < stack[top-1]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_LE:
    stack[top-2]=((stack[top-2] <= stack[top-1]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_GT:
    stack[top-2]=((stack[top-2] > stack[top-1]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_GE:
    stack[top-2]=((stack[top-2] >= stack[top-1]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_EQ:
  case OP_EQN:
    stack[top-2]=((stack[top-2] == stack[top-1]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_CALL:
    /* fprintf(stderr, "CALL\n"); */
    i = (sexp_uint_t) ((sexp*)ip)[0];
    tmp1 = stack[top-1];
    if (! SEXP_PROCEDUREP(tmp1))
      errx(2, "non-procedure application: %p", tmp1);
    stack[top-1] = (sexp) i;
    stack[top] = sexp_make_integer(ip+4);
    stack[top+1] = cp;
    top+=2;
    bc = sexp_procedure_code(tmp1);
    /* print_bytecode(bc); */
    /* disasm(bc); */
    ip = bc->data;
    cp = sexp_procedure_vars(tmp1);
    fprintf(stderr, "... calling procedure at %p\ncp: ", ip);
    /* sexp_write(cp, stderr); */
    fprintf(stderr, "\n");
    fprintf(stderr, "stack at %d\n", top);
    print_stack(stack, top);
    break;
  case OP_APPLY1:
    tmp1 = stack[top-1];
    if (! SEXP_PROCEDUREP(tmp1))
      errx(2, "non-procedure application: %p", tmp1);
    tmp2 = stack[top-2];
    i = sexp_length(tmp2);
    top += (i-2);
    for ( ; SEXP_PAIRP(tmp2); tmp2=SEXP_CDR(tmp2), top--)
      stack[top-1] = SEXP_CAR(tmp2);
    top += i+3;
    stack[top-3] = sexp_make_integer(i);
    stack[top-2] = sexp_make_integer(ip);
    stack[top-1] = cp;
    bc = sexp_procedure_code(tmp1);
    ip = bc->data;
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
    fprintf(stderr, "JUMP UNLESS, stack top is %d\n", stack[top-1]);
    if (stack[--top] == SEXP_FALSE) {
      fprintf(stderr, "test passed, jumping to + %d => %d\n", ((signed char*)ip)[0], ip + ((signed char*)ip)[0]);
      ip += ((signed char*)ip)[0];
    } else {
      fprintf(stderr, "test failed, not jumping\n");
      ip++;
    }
    break;
  case OP_JUMP:
    fprintf(stderr, "jumping to + %d => %d\n", ((signed char*)ip)[0], ip + ((signed char*)ip)[0]);
    ip += ((signed char*)ip)[0];
    break;
  case OP_RET:
    fprintf(stderr, "returning @ %d: ", top-1);
    fflush(stderr);
    sexp_write(stack[top-1], cur_error_port);
    fprintf(stderr, "...\n");
    /* print_stack(stack, top); */
    /*                      top-1  */
    /* stack: args ... n ip result */
    cp = stack[top-2];
    ip = (unsigned char*) sexp_unbox_integer(stack[top-3]);
    i = sexp_unbox_integer(stack[top-4]);
    stack[top-i-4] = stack[top-1];
    top = top-i-3;
    fprintf(stderr, "... done returning\n");
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

/************************** eval interface ****************************/

static const struct core_form core_forms[] = {
  {SEXP_CORE, CORE_DEFINE, "define"},
  {SEXP_CORE, CORE_SET, "set!"},
  {SEXP_CORE, CORE_LAMBDA, "lambda"},
  {SEXP_CORE, CORE_IF, "if"},
  {SEXP_CORE, CORE_BEGIN, "begin"},
  {SEXP_CORE, CORE_QUOTE, "quote"},
  {SEXP_CORE, CORE_DEFINE_SYNTAX, "define-syntax"},
  {SEXP_CORE, CORE_LET_SYNTAX, "let-syntax"},
  {SEXP_CORE, CORE_LETREC_SYNTAX, "letrec-syntax"},
};

static const struct opcode opcodes[] = {
#define _OP(c,o,n,m,t,u,i,s) {SEXP_OPCODE, c, o, n, m, t, u, i, s, NULL, NULL}
#define _FN(o,n,t,u,s,f) {SEXP_OPCODE, OPC_FOREIGN, o, n, 0, t,u, 0, s, (sexp)f, NULL}
#define _FN0(s, f) _FN(OP_FCALL0, 0, 0, 0, s, f)
#define _FN1(t, s, f) _FN(OP_FCALL1, 1, t, 0, s, f)
#define _FN2(t, u, s, f) _FN(OP_FCALL2, 2, t, u, s, f)
_OP(OPC_ACCESSOR, OP_CAR, 1, 0, SEXP_PAIR, 0, 0, "car"),
_OP(OPC_ACCESSOR, OP_SET_CAR, 2, 0, SEXP_PAIR, 0, 0, "set-car!"),
_OP(OPC_ACCESSOR, OP_CDR, 1, 0, SEXP_PAIR, 0, 0, "cdr"),
_OP(OPC_ACCESSOR, OP_SET_CDR, 2, 0, SEXP_PAIR, 0, 0, "set-cdr!"),
_OP(OPC_ACCESSOR, OP_VECTOR_REF,2,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-ref"),
_OP(OPC_ACCESSOR, OP_VECTOR_SET,3,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-set!"),
_OP(OPC_ACCESSOR, OP_STRING_REF,2,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-ref"),
_OP(OPC_ACCESSOR, OP_STRING_SET,3,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-set!"),
_OP(OPC_ARITHMETIC,     OP_ADD, 0, 1, SEXP_FIXNUM, 0, 0, "+"),
_OP(OPC_ARITHMETIC_INV, OP_SUB, 0, 1, SEXP_FIXNUM, 0, OP_NEG, "-"),
_OP(OPC_ARITHMETIC,     OP_MUL, 0, 1, SEXP_FIXNUM, 0, 0, "*"),
_OP(OPC_ARITHMETIC_INV, OP_DIV, 0, 1, SEXP_FIXNUM, 0, OP_INV, "/"),
_OP(OPC_ARITHMETIC,     OP_MOD, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, 0, "%"),
_OP(OPC_ARITHMETIC_CMP, OP_LT,  0, 1, SEXP_FIXNUM, 0, 0, "<"),
_OP(OPC_ARITHMETIC_CMP, OP_LE,  0, 1, SEXP_FIXNUM, 0, 0, "<="),
_OP(OPC_ARITHMETIC_CMP, OP_GT,  0, 1, SEXP_FIXNUM, 0, 0, ">"),
_OP(OPC_ARITHMETIC_CMP, OP_GE,  0, 1, SEXP_FIXNUM, 0, 0, ">="),
_OP(OPC_ARITHMETIC_CMP, OP_EQN, 0, 1, SEXP_FIXNUM, 0, 0, "="),
_OP(OPC_PREDICATE,      OP_EQ,  2, 0, 0, 0, 0, "eq?"),
_OP(OPC_CONSTRUCTOR,    OP_CONS, 2, 0, 0, 0, 0, "cons"),
_OP(OPC_CONSTRUCTOR,    OP_MAKE_VECTOR, 2, 0, SEXP_FIXNUM, 0, 0, "make-vector"),
_OP(OPC_CONSTRUCTOR,    OP_MAKE_PROCEDURE, 2, 0, 0, 0, 0, "make-procedure"),
_OP(OPC_TYPE_PREDICATE, OP_PAIRP,  1, 0, 0, 0, 0, "pair?"),
_OP(OPC_TYPE_PREDICATE, OP_NULLP,  1, 0, 0, 0, 0, "null?"),
_OP(OPC_TYPE_PREDICATE, OP_STRINGP,  1, 0, 0, 0, 0, "string?"),
_OP(OPC_TYPE_PREDICATE, OP_SYMBOLP,  1, 0, 0, 0, 0, "symbol?"),
_OP(OPC_TYPE_PREDICATE, OP_CHARP,  1, 0, 0, 0, 0, "char?"),
_OP(OPC_TYPE_PREDICATE, OP_VECTORP,  1, 0, 0, 0, 0, "vector?"),
_OP(OPC_TYPE_PREDICATE, OP_PROCEDUREP,  1, 0, 0, 0, 0, "procedure?"),
_OP(OPC_TYPE_PREDICATE, OP_IPORTP,  1, 0, 0, 0, 0, "input-port?"),
_OP(OPC_TYPE_PREDICATE, OP_OPORTP,  1, 0, 0, 0, 0, "output-port?"),
_OP(OPC_TYPE_PREDICATE, OP_EOFP,  1, 0, 0, 0, 0, "eof-object?"),
_OP(OPC_GENERIC, OP_APPLY1, 2, SEXP_PROCEDURE, SEXP_PAIR, 0, 0, "apply1"),
_FN1(SEXP_PAIR, "reverse", sexp_reverse),
_FN1(SEXP_PAIR, "list->vector", sexp_list_to_vector),
_FN2(0, SEXP_PAIR, "memq", sexp_memq),
_FN2(0, SEXP_PAIR, "assq", sexp_assq),
_FN2(SEXP_PAIR, SEXP_PAIR, "diffq", sexp_lset_diff),
#undef _OP
#undef _FN
#undef _FN0
#undef _FN1
#undef _FN2
};

env make_standard_env() {
  int i;
  env e = (env) SEXP_ALLOC(sizeof(struct env));
  e->tag = SEXP_ENV;
  e->parent = NULL;
  e->bindings = SEXP_NULL;
  for (i=0; i<(sizeof(core_forms)/sizeof(struct core_form)); i++) {
    env_define(e, sexp_intern(core_forms[i].name), (sexp)(&core_forms[i]));
  }
  for (i=0; i<(sizeof(opcodes)/sizeof(struct opcode)); i++) {
    env_define(e, sexp_intern(opcodes[i].name), (sexp)(&opcodes[i]));
  }
  return e;
}

/************************** eval interface ****************************/

sexp eval_in_stack(sexp obj, env e, sexp* stack, unsigned int top) {
  bytecode bc;
  bc = compile(SEXP_NULL, sexp_cons(obj, SEXP_NULL), e, SEXP_NULL, SEXP_NULL, 1);
  return vm(bc, e, stack, top);
}

sexp eval(sexp obj, env e) {
  sexp* stack = (sexp*) SEXP_ALLOC(sizeof(sexp) * INIT_STACK_SIZE);
  sexp res = eval_in_stack(obj, e, stack, 0);
  SEXP_FREE(stack);
  return res;
}

void scheme_init() {
  if (! scheme_initialized_p) {
    scheme_initialized_p = 1;
    sexp_init();
    cur_input_port = sexp_make_input_port(stdin);
    cur_output_port = sexp_make_output_port(stdout);
    cur_error_port = sexp_make_output_port(stderr);
  }
}

void repl (env e, sexp *stack) {
  sexp obj, res;
  while (1) {
    fprintf(stdout, "> ");
    fflush(stdout);
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
  sexp obj, res, in, out, *stack;
  env e;
  int i, quit=0;

  scheme_init();
  e = make_standard_env();
  stack = (sexp*) SEXP_ALLOC(sizeof(sexp) * INIT_STACK_SIZE);

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'e':
    case 'p':
      obj = sexp_read_from_string(argv[i+1]);
      res = eval_in_stack(obj, e, stack, 0);
      if (argv[i][1] == 'p') {
        sexp_write(res, cur_output_port);
        sexp_write_char('\n', cur_output_port);
      }
      quit=1;
      i++;
      break;
    default:
      errx(1, "unknown option: %s", argv[i]);
    }
  }

  repl(e, stack);
  return 0;
}

