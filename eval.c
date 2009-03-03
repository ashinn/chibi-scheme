/*  eval.c -- evaluator library implementation */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt */

#include "eval.h"

/* ******************************************************************** */

static struct core_form core_forms[] = {
  {SEXP_CORE, "define", CORE_DEFINE},
  {SEXP_CORE, "set!", CORE_SET},
  {SEXP_CORE, "lambda", CORE_LAMBDA},
  {SEXP_CORE, "if", CORE_IF},
  {SEXP_CORE, "begin", CORE_BEGIN},
  {SEXP_CORE, "quote", CORE_QUOTE},
  {SEXP_CORE, "define-syntax", CORE_DEFINE_SYNTAX},
  {SEXP_CORE, "let-syntax", CORE_LET_SYNTAX},
  {SEXP_CORE, "letrec-syntax", CORE_LETREC_SYNTAX},
};

static struct opcode opcodes[] = {
{SEXP_OPCODE, OPC_TYPE_PREDICATE, OP_CAR, 1, 0, SEXP_PAIR, 0, "car", 0, NULL},
{SEXP_OPCODE, OPC_TYPE_PREDICATE, OP_CDR, 1, 0, SEXP_PAIR, 0, "cdr", 0, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC,     OP_ADD, 0, 1, SEXP_FIXNUM, 0, "+", 0, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC_INV, OP_SUB, 0, 1, SEXP_FIXNUM, 0, "-", OP_NEG, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC,     OP_MUL, 0, 1, SEXP_FIXNUM, 0, "*", 0, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC_INV, OP_DIV, 0, 1, SEXP_FIXNUM, 0, "/", OP_INV, 0},
{SEXP_OPCODE, OPC_ARITHMETIC,     OP_MOD, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, "%", 0, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC_CMP, OP_LT,  0, 1, SEXP_FIXNUM, 0, "<", 0, NULL},
{SEXP_OPCODE, OPC_CONSTRUCTOR,    OP_CONS, 2, 0, 0, 0, "cons", 0, NULL},
{SEXP_OPCODE, OPC_CONSTRUCTOR,    OP_MAKE_VECTOR, 2, 0, SEXP_FIXNUM, 0, "make-vector", 0, NULL},
{SEXP_OPCODE, OPC_CONSTRUCTOR,    OP_MAKE_PROCEDURE, 2, 0, 0, 0, "make-procedure", 0, NULL},
};

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
    if (assq(id, e->bindings) != SEXP_FALSE)
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
    e->bindings = cons(cons(key, value), e->bindings);
  }
}

env extend_env_closure (env e, sexp fv) {
  int i;
  env e2 = (env) SEXP_ALLOC(sizeof(struct env));
  e2->tag = SEXP_ENV;
  e2->parent = e;
  e2->bindings = SEXP_NULL;
  for (i=0; SEXP_PAIRP(fv); fv = SEXP_CDR(fv), i++) {
    e2->bindings = cons(cons(SEXP_CAR(fv), make_integer(i)), e2->bindings);
  }
  return e2;
}

env make_standard_env() {
  int i;
  env e = (env) SEXP_ALLOC(sizeof(struct env));
  e->tag = SEXP_ENV;
  e->parent = NULL;
  e->bindings = SEXP_NULL;
  for (i=0; i<(sizeof(core_forms)/sizeof(struct core_form)); i++) {
    env_define(e, intern(core_forms[i].name), (sexp)(&core_forms[i]));
  }
  for (i=0; i<(sizeof(opcodes)/sizeof(struct opcode)); i++) {
    env_define(e, intern(opcodes[i].name), (sexp)(&opcodes[i]));
  }
  return e;
}

/************************* bytecode utilities ***************************/

void shrink_bcode(bytecode *bc, unsigned int i) {
  bytecode tmp;
  if ((*bc)->len != i) {
    fprintf(stderr, "shrinking to %d\n", i);
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
    fprintf(stderr, "expanding (%d < %d)\n", (*bc)->len, (*i)+1);
    tmp = (bytecode) SEXP_ALLOC(sizeof(unsigned int) + (*bc)->len*2);
    tmp->len = (*bc)->len*2;
    memcpy(tmp->data, (*bc)->data, (*bc)->len);
    SEXP_FREE(*bc);
    *bc = tmp;
  }
  (*bc)->data[(*i)++] = c;
}

void emit_word(bytecode *bc, unsigned int *i, unsigned long val)  {
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

sexp make_procedure(sexp bc, sexp vars) {
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
    /* fprintf(stderr, ":: pair\n"); */
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      fprintf(stderr, ":: symbol application\n");
      o1 = env_cell(e, SEXP_CAR(obj));
      /* fprintf(stderr, ":: => %p\n", o1); */
      if (! o1)
        errx(1, "unknown operator: %s", SEXP_CAR(obj));
      o1 = SEXP_CDR(o1);
      /* fprintf(stderr, ":: => %p\n", o1); */
      if (SEXP_COREP(o1)) {
        /* core form */
        fprintf(stderr, ":: core form\n");
        switch (((core_form)o1)->code) {
        case CORE_LAMBDA:
          fprintf(stderr, ":: lambda\n");
          analyze_lambda(SEXP_FALSE, SEXP_CADR(obj), SEXP_CDDR(obj),
                         bc, i, e, params, fv, sv, d);
          break;
        case CORE_DEFINE:
          fprintf(stderr, "compiling global set: %p\n", SEXP_CADR(obj));
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
          emit_word(bc, i, (unsigned long) (SEXP_PAIRP(SEXP_CADR(obj))
                                            ? SEXP_CAR(SEXP_CADR(obj))
                                            : SEXP_CADR(obj)));
          emit(bc, i, OP_PUSH);
          (*d)++;
          emit_word(bc, i, (unsigned long) SEXP_UNDEF);
          break;
        case CORE_SET:
          fprintf(stderr, "set!: "); write_sexp(stderr, SEXP_CADR(obj));
          fprintf(stderr, " sv: ");  write_sexp(stderr, sv);
          fprintf(stderr, "\n");
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
          fprintf(stderr, "test clause: %d\n", *i);
          analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
          emit(bc, i, OP_JUMP_UNLESS);              /* jumps if test fails */
          tmp1 = *i;
          emit(bc, i, 0);
          fprintf(stderr, "pass clause: %d\n", *i);
          analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d);
          emit(bc, i, OP_JUMP);
          tmp2 = *i;
          emit(bc, i, 0);
          ((signed char*) (*bc)->data)[tmp1] = (*i)-tmp1-1;    /* patch */
          fprintf(stderr, "fail clause: %d\n", *i);
          if (SEXP_PAIRP(SEXP_CDDDR(obj))) {
            analyze(SEXP_CADDDR(obj), bc, i, e, params, fv, sv, d);
          } else {
            emit(bc, i, OP_PUSH);
            (*d)++;
            emit_word(bc, i, (unsigned long) SEXP_UNDEF);
          }
          ((signed char*) (*bc)->data)[tmp2] = (*i)-tmp2-1;    /* patch */
          break;
        case CORE_QUOTE:
          emit(bc, i, OP_PUSH);
          (*d)++;
          emit_word(bc, i, (unsigned long)SEXP_CADR(obj));
          break;
        default:
          errx(1, "unknown core form: %s", ((core_form)o1)->code);
        }
      } else if (SEXP_OPCODEP(o1)) {
        fprintf(stderr, ":: opcode\n");
        /* direct opcode */
        /* verify arity */
        switch (((opcode)o1)->op_class) {
        case OPC_TYPE_PREDICATE:
        case OPC_PREDICATE:
        case OPC_ARITHMETIC:
        case OPC_ARITHMETIC_INV:
        case OPC_ARITHMETIC_CMP:
          if (SEXP_NULLP(SEXP_CDR(obj))) {
            errx(1, "unknown opcode class: %d", ((opcode)o1)->op_class);
          } else if (SEXP_NULLP(SEXP_CDDR(obj))) {
            if (((opcode)o1)->op_class == OPC_ARITHMETIC_INV) {
              analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
              emit(bc, i, ((opcode)o1)->op_inverse);
            } else {
              analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
            }
          } else {
            /* fprintf(stderr, ":: class: %d\n", ((opcode)o1)->op_class); */
            for (o2 = reverse(SEXP_CDR(obj)); SEXP_PAIRP(o2); o2 = SEXP_CDR(o2)) {
              /* fprintf(stderr, ":: arg: %d\n", SEXP_CAR(o2)); */
              analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d);
            }
            fprintf(stderr, ":: name: %d\n", ((opcode)o1)->op_name);
            emit(bc, i, ((opcode)o1)->op_name);
            (*d) -= length(SEXP_CDDR(obj));
          }
          break;
        default:
          errx(1, "unknown opcode class: %d", ((opcode)o1)->op_class);
        }
      } else {
        /* function call */
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
    fprintf(stderr, "push: %d\n", (unsigned long)obj);
    emit(bc, i, OP_PUSH);
    emit_word(bc, i, (unsigned long)obj);
    (*d)++;
  }
}

void analyze_var_ref (sexp obj, bytecode *bc, unsigned int *i, env e,
                      sexp params, sexp fv, sexp sv, unsigned int *d) {
  int tmp;
  fprintf(stderr, "symbol lookup, param length: %d sv: ", length(params));
  write_sexp(stderr, sv);
  fprintf(stderr, "\n");
  if ((tmp = list_index(params, obj)) >= 0) {
    fprintf(stderr, "compiling local ref: %p => %d (d = %d)\n", obj, tmp, *d);
    emit(bc, i, OP_STACK_REF);
    emit_word(bc, i, tmp + *d + 4);
    (*d)++;
  } else if ((tmp = list_index(fv, obj)) >= 0) {
    fprintf(stderr, "compiling closure ref: %p => %d\n", obj, tmp);
    emit(bc, i, OP_CLOSURE_REF);
    emit_word(bc, i, tmp);
    (*d)++;
  } else {
    fprintf(stderr, "compiling global ref: %p\n", obj);
    emit(bc, i, OP_GLOBAL_REF);
    emit_word(bc, i, (unsigned long) obj);
    (*d)++;
  }
  if (list_index(sv, obj) >= 0) {
    fprintf(stderr, "mutable variables, fetching CAR\n");
    emit(bc, i, OP_CAR);
  }
}

void analyze_app (sexp obj, bytecode *bc, unsigned int *i,
                  env e, sexp params, sexp fv, sexp sv, unsigned int *d) {
  sexp o1;
  unsigned long len = length(SEXP_CDR(obj));

  /* push the arguments onto the stack */
  for (o1 = reverse(SEXP_CDR(obj)); SEXP_PAIRP(o1); o1 = SEXP_CDR(o1)) {
    analyze(SEXP_CAR(o1), bc, i, e, params, fv, sv, d);
  }

  /* push the operator onto the stack */
  analyze(SEXP_CAR(obj), bc, i, e, params, fv, sv, d);

  /* make the call */
  emit(bc, i, OP_CALL);
  emit_word(bc, i, (unsigned long) make_integer(len));
}

sexp free_vars (env e, sexp formals, sexp obj, sexp fv) {
  sexp o1;
  if (SEXP_SYMBOLP(obj)) {
    if (env_global_p(e, obj)
        || (list_index(formals, obj) >= 0)
        || (list_index(fv, obj) >= 0))
      return fv;
    else
      return cons(obj, fv);
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
          formals = lset_diff(formals, SEXP_CADR(obj));
          return set_vars(e, formals, SEXP_CADDR(obj), sv);
        } else if (((core_form)SEXP_CDR(tmp))->code == CORE_SET) {
          if ((list_index(formals, SEXP_CADR(obj)) >= 0)
              && ! (list_index(sv, SEXP_CADR(obj)) >= 0)) {
            fprintf(stderr, "found set! "); write_sexp(stderr, SEXP_CADR(obj));
            fprintf(stderr, "\n");
            sv = cons(SEXP_CADR(obj), sv);
            return set_vars(e, formals, SEXP_CADDR(obj), sv);
          }
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
  sexp obj;
  sexp fv2 = free_vars(e, formals, body, SEXP_NULL), ls;
  env e2 = extend_env_closure(e, formals);
  int k;
  fprintf(stderr, "%d free-vars\n", length(fv2));
  write_sexp(stderr, fv2);
  fprintf(stderr, "\n");
  obj = (sexp) compile(formals, body, e2, fv2, sv, 0);
  emit(bc, i, OP_PUSH);
  emit_word(bc, i, (unsigned long) SEXP_UNDEF);
  emit(bc, i, OP_PUSH);
  emit_word(bc, i, (unsigned long) make_integer(length(fv2)));
  emit(bc, i, OP_MAKE_VECTOR);
  (*d)++;
  for (ls=fv2, k=0; SEXP_PAIRP(ls); ls=SEXP_CDR(ls), k++) {
    analyze_var_ref(SEXP_CAR(ls), bc, i, e, params, fv, SEXP_NULL, d);
    emit(bc, i, OP_PUSH);
    emit_word(bc, i, (unsigned long) make_integer(k));
    emit(bc, i, OP_STACK_REF);
    emit_word(bc, i, 3);
    emit(bc, i, OP_VECTOR_SET);
    emit(bc, i, OP_DROP);
    (*d)--;
  }
  emit(bc, i, OP_PUSH);
  emit_word(bc, i, (unsigned long) obj);
  emit(bc, i, OP_MAKE_PROCEDURE);
}

bytecode compile(sexp params, sexp obj, env e, sexp fv, sexp sv, int done_p) {
  unsigned int i = 0, j, d = 0;
  bytecode bc = (bytecode) SEXP_ALLOC(sizeof(struct bytecode)+INIT_BCODE_SIZE);
  sexp sv2 = set_vars(e, params, obj, SEXP_NULL), ls;
  fprintf(stderr, "set-vars: "); write_sexp(stderr, sv2); fprintf(stderr, "\n");
  bc->tag = SEXP_BYTECODE;
  bc->len = INIT_BCODE_SIZE;
  fprintf(stderr, "analyzing\n");
  for (ls=params; SEXP_PAIRP(ls); ls=SEXP_CDR(ls)) {
    if ((j = list_index(sv2, SEXP_CAR(ls)) >= 0)) {
      fprintf(stderr, "consing mutable var\n");
      emit(&bc, &i, OP_PUSH);
      emit_word(&bc, &i, (unsigned long) SEXP_NULL);
      emit(&bc, &i, OP_STACK_REF);
      emit_word(&bc, &i, j+3);
      emit(&bc, &i, OP_CONS);
      emit(&bc, &i, OP_STACK_SET);
      emit_word(&bc, &i, j+4);
      emit(&bc, &i, OP_DROP);
    }
  }
  sv = append(sv2, sv);
  for ( ; SEXP_PAIRP(obj); obj=SEXP_CDR(obj)) {
    fprintf(stderr, "loop: "); write_sexp(stderr, obj); fprintf(stderr, "\n");
    analyze(SEXP_CAR(obj), &bc, &i, e, params, fv, sv, &d);
    if (SEXP_PAIRP(SEXP_CDR(obj))) emit(&bc, &i, OP_DROP);
  }
  emit(&bc, &i, done_p ? OP_DONE : OP_RET);
  /* fprintf(stderr, "shrinking\n"); */
  shrink_bcode(&bc, i);
  fprintf(stderr, "done compiling:\n");
  print_bytecode(bc);
  disasm(bc);
  return bc;
}

/*********************** the virtual machine **************************/

sexp vm(bytecode bc, env e, sexp* stack, unsigned int top) {
  unsigned char *ip=bc->data;
  sexp cp, tmp;
  int i;

 loop:
  /* fprintf(stderr, "opcode: %d, ip: %d\n", *ip, ip); */
  /* print_bytecode(bc); */
  switch (*ip++) {
  case OP_NOOP:
    fprintf(stderr, "noop\n");
    break;
  case OP_GLOBAL_REF:
    fprintf(stderr, "global ref: ip: %p => %p: ", ip, ((sexp*)ip)[0]);
    fflush(stderr);
    write_sexp(stderr, ((sexp*)ip)[0]);
    fprintf(stderr, "\n");
    tmp = env_cell(e, ((sexp*)ip)[0]);
    stack[top++]=SEXP_CDR(tmp);
    ip += sizeof(sexp);
    break;
  case OP_GLOBAL_SET:
    fprintf(stderr, "global set: %p: ", ((sexp*)ip)[0]);
    fflush(stderr);
    write_sexp(stderr, ((sexp*)ip)[0]);
    fprintf(stderr, "\n");
    env_define(e, ((sexp*)ip)[0], stack[--top]);
    ip += sizeof(sexp);
    break;
  case OP_STACK_REF:
    fprintf(stderr, "stack ref: ip=%p,  %d - %d => ",
            ip, top, (unsigned long) ((sexp*)ip)[0]);
    fflush(stderr);
    write_sexp(stderr, stack[top - (unsigned int) ((sexp*)ip)[0]]);
    fprintf(stderr, "\n");
    stack[top] = stack[top - (unsigned int) ((sexp*)ip)[0]];
    ip += sizeof(sexp);
    top++;
    break;
  case OP_STACK_SET:
    stack[top - (unsigned int) ((sexp*)ip)[0]] = stack[top-1];
    stack[top-1] = SEXP_UNDEF;
    ip += sizeof(sexp);
    break;
  case OP_CLOSURE_REF:
    fprintf(stderr, "closure-ref %d => ", ((sexp*)ip)[0]);
    fflush(stderr);
    write_sexp(stderr, vector_ref(cp,((sexp*)ip)[0]));
    fprintf(stderr, "\n");
    stack[top++]=vector_ref(cp,((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_VECTOR_REF:
    stack[top-2]=vector_ref(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_VECTOR_SET:
    fprintf(stderr, "vector-set! %p %d => ", stack[top-1], unbox_integer(stack[top-2]));
    write_sexp(stderr, stack[top-3]);
    fprintf(stderr, "\n");
    vector_set(stack[top-1], stack[top-2], stack[top-3]);
    stack[top-3]=SEXP_UNDEF;
    top-=2;
    break;
  case OP_MAKE_PROCEDURE:
    stack[top-2]=make_procedure(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_MAKE_VECTOR:
    stack[top-2]=make_vector(unbox_integer(stack[top-1]), stack[top-2]);
    top--;
    break;
  case OP_PUSH:
    /* fprintf(stderr, " (push)\n"); */
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
    tmp = stack[top-2];
    stack[top-2]=stack[top-1];
    stack[top-1]=tmp;
    break;
  case OP_CAR:
    stack[top-1]=car(stack[top-1]);
    break;
  case OP_CDR:
    stack[top-1]=cdr(stack[top-1]);
    break;
  case OP_SET_CAR:
    set_car(stack[top-1], stack[top-2]);
    stack[top-2]=SEXP_UNDEF;
    top--;
    break;
  case OP_SET_CDR:
    set_cdr(stack[top-1], stack[top-2]);
    stack[top-2]=SEXP_UNDEF;
    top--;
    break;
  case OP_CONS:
    stack[top-2]=cons(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_ADD:
    fprintf(stderr, "OP_ADD %d %d\n", stack[top-1], stack[top-2]);
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
  case OP_CALL:
    fprintf(stderr, "CALL\n");
    i = (unsigned long) ((sexp*)ip)[0];
    tmp = stack[top-1];
    if (! SEXP_PROCEDUREP(tmp))
      errx(2, "non-procedure application: %p", tmp);
    stack[top-1] = (sexp) i;
    stack[top] = (sexp) (ip+4);
    stack[top+1] = cp;
    top+=2;
    bc = procedure_code(tmp);
    print_bytecode(bc);
    ip = bc->data;
    cp = procedure_vars(tmp);
    fprintf(stderr, "... calling procedure at %p\ncp: ", ip);
    write_sexp(stderr, cp);
    fprintf(stderr, "\n");
    /* print_stack(stack, top); */
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
    write_sexp(stderr, stack[top-1]);
    fprintf(stderr, "...\n");
    print_stack(stack, top);
    /*                      top-1  */
    /* stack: args ... n ip result */
    cp = stack[top-2];
    fprintf(stderr, "1\n");
    ip = (unsigned char*) stack[top-3];
    fprintf(stderr, "2\n");
    i = unbox_integer(stack[top-4]);
    fprintf(stderr, "3 (i=%d)\n", i);
    stack[top-i-4] = stack[top-1];
    fprintf(stderr, "4\n");
    top = top-i-3;
    fprintf(stderr, "... done returning\n");
    break;
  case OP_DONE:
    fprintf(stderr, "finally returning @ %d: ", top-1);
    fflush(stderr);
    write_sexp(stderr, stack[top-1]);
    fprintf(stderr, "\n");
    goto end_loop;
  default:
    fprintf(stderr, "unknown opcode: %d\n", *(ip-1));
    stack[top] = SEXP_ERROR;
    goto end_loop;
  }
  fprintf(stderr, "looping\n");
  goto loop;

 end_loop:
  return stack[top-1];
}

/************************** eval interface ****************************/

sexp eval_in_stack(sexp obj, env e, sexp* stack, unsigned int top) {
  bytecode bc = compile(SEXP_NULL, cons(obj, SEXP_NULL), e, SEXP_NULL, SEXP_NULL, 1);
  fprintf(stderr, "evaling\n");
  return vm(bc, e, stack, top);
}

sexp eval(sexp obj, env e) {
  sexp* stack = (sexp*) SEXP_ALLOC(sizeof(sexp) * INIT_STACK_SIZE);
  sexp res = eval_in_stack(obj, e, stack, 0);
  SEXP_FREE(stack);
  return res;
}

int main (int argc, char **argv) {
  sexp obj, res, *stack;
  env e;

  sexp_init();
  e = make_standard_env();
  stack = (sexp*) SEXP_ALLOC(sizeof(sexp) * INIT_STACK_SIZE);

  /* repl */
  fprintf(stdout, "> ");
  fflush(stdout);
  while ((obj = read_sexp(stdin)) != SEXP_EOF) {
    write_sexp(stdout, obj);
    fprintf(stdout, "\n => ");
    res = eval_in_stack(obj, e, stack, 0);
    write_sexp(stdout, res);
    fprintf(stdout, "\n> ");
    fflush(stdout);
  }
  return 0;
}

