/*  x86.c -- native amd64 backend                             */
/*  Copyright (c) 2009-2026 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>

#define SEXP_NATIVE_FRAME_SLOTS 512
#define SEXP_NATIVE_FRAME_SIZE  12312
#define SEXP_NATIVE_FRAME_ALLOC 12312

struct sexp_native_frame {
  struct sexp_gc_var_t saves[SEXP_NATIVE_FRAME_SLOTS];
  sexp slots[SEXP_NATIVE_FRAME_SLOTS];
  struct sexp_gc_var_t *old_saves;
  sexp old_proc;
  sexp self;
};

/* Forward declarations */
static void generate (sexp ctx, sexp name, sexp loc, sexp lam, sexp x);
static void generate_lit (sexp ctx, sexp value);
static sexp make_opcode_procedure (sexp ctx, sexp op, sexp_uint_t i, sexp_sint_t flags);
sexp sexp_native_call (sexp ctx, sexp callee, sexp_sint_t nargs, sexp *args);
sexp sexp_native_dispatch_op (sexp ctx, sexp self, sexp op, sexp_sint_t nargs, sexp *args);
sexp sexp_native_yield (sexp ctx);
sexp sexp_write_char_op (sexp ctx, sexp self, sexp_sint_t n, sexp ch, sexp out);
sexp sexp_read_char_op (sexp ctx, sexp self, sexp_sint_t n, sexp in);
sexp sexp_peek_char_op (sexp ctx, sexp self, sexp_sint_t n, sexp in);
sexp sexp_native_parameter_set (sexp ctx, sexp param_op, sexp val);

static void sexp_inc_context_depth (sexp ctx, sexp_sint_t off) {
  sexp_context_depth(ctx) = sexp_fx_add(sexp_context_depth(ctx), sexp_make_fixnum(off));
  if (sexp_unbox_fixnum(sexp_context_depth(ctx))
      > sexp_unbox_fixnum(sexp_context_max_depth(ctx)))
    sexp_context_max_depth(ctx) = sexp_context_depth(ctx);
}

/************************* GC Preservation ****************************/

void sexp_native_enter (sexp ctx, struct sexp_native_frame *f, sexp self,
                        sexp_sint_t nargs, sexp *args, sexp_sint_t packed_params) {
  int i;
  sexp_sint_t num_params = packed_params & 0xFFFF;
  sexp_sint_t num_slots = (packed_params >> 16) & 0xFFFF;
  if (num_slots == 0 || num_slots >= SEXP_NATIVE_FRAME_SLOTS)
    num_slots = SEXP_NATIVE_FRAME_SLOTS - 1;

  f->old_saves = sexp_context_saves(ctx);
  f->old_proc = sexp_context_proc(ctx);
  f->self = self;
  sexp_context_proc(ctx) = self;
  for (i = 0; i < num_slots; i++) {
    f->slots[i] = (i < num_params && i < nargs && args) ? args[i] : SEXP_UNDEF;
    f->saves[i].var = &f->slots[i];
    f->saves[i].next = (i == 0) ? f->old_saves : &f->saves[i - 1];
  }
  f->slots[num_slots] = self;
  f->saves[num_slots].var = &f->slots[num_slots];
  f->saves[num_slots].next = (num_slots > 0) ? &f->saves[num_slots - 1] : f->old_saves;
  sexp_context_saves(ctx) = &f->saves[num_slots];
#if SEXP_USE_GREEN_THREADS
  if (--sexp_context_refuel(ctx) <= 0) {
    sexp_context_refuel(ctx) = SEXP_DEFAULT_QUANTUM;
    if (sexp_pairp(sexp_global(ctx, SEXP_G_THREADS_FRONT)) ||
        sexp_pairp(sexp_global(ctx, SEXP_G_THREADS_PAUSED))) {
      sexp_native_yield(ctx);
    }
  }
#endif
}

void sexp_native_exit (sexp ctx, struct sexp_native_frame *f) {
  sexp_context_saves(ctx) = f->old_saves;
  sexp_context_proc(ctx) = f->old_proc;
}

static sexp native_cons (sexp ctx, sexp a, sexp b) {
  return sexp_cons(ctx, a, b);
}

/************************* Trampoline & Procedures ********************/

#define FIXED_ARGS_SIZE 64

typedef struct sexp_native_trampoline_struct {
  struct sexp_native_trampoline_struct *prev;
  sexp proc;
  sexp_sint_t nargs;
  sexp *args;
  sexp fixed_args[FIXED_ARGS_SIZE];
  struct sexp_gc_var_t saves[FIXED_ARGS_SIZE];
  sexp *dyn_args;
  size_t dyn_capacity;
  struct sexp_gc_var_t *dyn_saves;
} sexp_native_trampoline_t;

sexp sexp_make_trampoline (sexp ctx, sexp proc, sexp args) {
  return sexp_make_exception(ctx, SEXP_TRAMPOLINE, SEXP_FALSE, args, proc, SEXP_FALSE);
}

sexp sexp_native_make_trampoline (sexp ctx, sexp proc, sexp_sint_t nargs, sexp *args) {
  sexp_native_trampoline_t *tramp = (sexp_native_trampoline_t*)sexp_context_trampoline(ctx);
  if (tramp) {
    tramp->proc = proc;
    tramp->nargs = nargs;
    if (nargs + 2 <= FIXED_ARGS_SIZE) {
      tramp->args = tramp->fixed_args;
    } else {
      if (nargs + 2 > tramp->dyn_capacity) {
        tramp->dyn_capacity = (size_t)(nargs + 2) * 2;
        tramp->dyn_args = (sexp*)realloc(tramp->dyn_args, sizeof(sexp) * tramp->dyn_capacity);
        tramp->dyn_saves = (struct sexp_gc_var_t*)realloc(tramp->dyn_saves, sizeof(struct sexp_gc_var_t) * tramp->dyn_capacity);
      }
      tramp->args = tramp->dyn_args;
    }
    for (sexp_sint_t k = 0; k < nargs; k++) {
      tramp->args[k] = args[k];
    }
    return SEXP_TRAMPOLINE;
  }
  sexp_gc_var1(ls);
  sexp_gc_preserve1(ctx, ls);
  ls = SEXP_NULL;
  for (sexp_sint_t k = nargs - 1; k >= 0; k--)
    ls = sexp_cons(ctx, args[k], ls);
  sexp res = sexp_make_trampoline(ctx, proc, ls);
  sexp_gc_release1(ctx);
  return res;
}

typedef sexp (*sexp_native_func)(sexp ctx, sexp self, sexp_sint_t nargs, sexp *args);
typedef sexp (*sexp_proc8)(sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc9)(sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);

static sexp sexp_native_raise_exception (sexp ctx, sexp self, sexp exn) {
  sexp handler = sexp_parameter_ref(ctx, sexp_global(ctx, SEXP_G_ERR_HANDLER));
  if (sexp_procedurep(handler)) {
    return sexp_apply1(ctx, handler, exn);
  } else {
    if (!sexp_exceptionp(exn))
      return sexp_make_exception(ctx, SEXP_UNCAUGHT, SEXP_FALSE, exn, self, SEXP_FALSE);
    return exn;
  }
}

static sexp sexp_native_raise (sexp ctx, sexp self, const char *msg, sexp irritants) {
  sexp exn = sexp_user_exception(ctx, self, msg, irritants);
  return sexp_native_raise_exception(ctx, self, exn);
}

static sexp sexp_native_type_raise (sexp ctx, sexp self, sexp_uint_t type, sexp x) {
  sexp exn = sexp_type_exception(ctx, self, type, x);
  return sexp_native_raise_exception(ctx, self, exn);
}

sexp sexp_native_delayed_ref (sexp ctx, sexp cell) {
  if (sexp_cdr(cell) == SEXP_UNDEF && sexp_synclop(sexp_car(cell))) {
    sexp tmp1 = sexp_env_cell(ctx, sexp_synclo_env(sexp_car(cell)),
                              sexp_synclo_expr(sexp_car(cell)), 0);
    if (tmp1 != NULL) {
      sexp_cdr(cell) = sexp_cdr(tmp1);
      return sexp_cdr(tmp1);
    }
  }
  if (sexp_cdr(cell) == SEXP_UNDEF) {
    return sexp_native_raise(ctx, sexp_car(cell), "undefined variable",
                             sexp_list1(ctx, sexp_car(cell)));
  }
  return sexp_cdr(cell);
}

sexp sexp_native_call (sexp ctx, sexp callee, sexp_sint_t nargs, sexp *args) {
  sexp_gc_var1(callee_save);
  sexp_gc_preserve1(ctx, callee_save);
  callee_save = callee;

  sexp_native_trampoline_t tramp;
  tramp.prev = (sexp_native_trampoline_t*)sexp_context_trampoline(ctx);
  tramp.proc = SEXP_FALSE;
  tramp.nargs = 0;
  tramp.args = tramp.fixed_args;
  tramp.dyn_args = NULL;
  tramp.dyn_capacity = 0;
  tramp.dyn_saves = NULL;
  sexp_context_trampoline(ctx) = &tramp;

  sexp res = SEXP_VOID;
  sexp fixed_args[FIXED_ARGS_SIZE];
  sexp *dyn_args = NULL;
  size_t dyn_capacity = 0;
  sexp *call_args = args;

  while (1) {
    callee_save = callee;
    if (sexp_opcodep(callee)) {
      callee = make_opcode_procedure(ctx, callee, nargs, SEXP_PROC_NONE);
      callee_save = callee;
      if (sexp_exceptionp(callee)) {
        res = sexp_native_raise_exception(ctx, callee, callee);
        break;
      }
    }
    if (!sexp_procedurep(callee)) {
      if (sexp_exceptionp(callee)) {
        res = sexp_native_raise_exception(ctx, callee, callee);
        break;
      }
      res = sexp_native_raise(ctx, callee, "non procedure application", sexp_list1(ctx, callee));
      break;
    }
    sexp_sint_t expected = sexp_procedure_num_args(callee);
    sexp_sint_t diff = nargs - expected;
    if (diff < 0) {
      res = sexp_native_raise(ctx, callee, "not enough args", sexp_list2(ctx, callee, sexp_make_fixnum(nargs)));
      break;
    }
    if (sexp_procedure_variadic_p(callee)) {
      if (nargs + 2 <= FIXED_ARGS_SIZE) {
        call_args = fixed_args;
      } else {
        if (nargs + 2 > dyn_capacity) {
          dyn_capacity = (nargs + 2) * 2;
          dyn_args = (sexp*)realloc(dyn_args, sizeof(sexp) * dyn_capacity);
        }
        call_args = dyn_args;
      }
      for (sexp_sint_t k = 0; k < nargs; k++)
        call_args[k] = args[k];
    } else {
      call_args = args;
    }

    if (diff > 0) {
      if (sexp_procedure_variadic_p(callee)) {
        if (!sexp_procedure_unused_rest_p(callee)) {
          sexp_gc_var1(rest);
          sexp_gc_preserve1(ctx, rest);
          struct sexp_gc_var_t *saved_saves = sexp_context_saves(ctx);
          if (nargs <= FIXED_ARGS_SIZE) {
            for (sexp_sint_t k = 0; k < nargs; k++) {
              tramp.saves[k].var = &call_args[k];
              tramp.saves[k].next = (k == 0) ? saved_saves : &tramp.saves[k - 1];
            }
            sexp_context_saves(ctx) = &tramp.saves[nargs - 1];
          }
          rest = SEXP_NULL;
          for (sexp_sint_t k = nargs - 1; k >= expected; k--) {
            rest = sexp_cons(ctx, call_args[k], rest);
          }
          call_args[expected] = rest;
          nargs = expected + 1;
          sexp_context_saves(ctx) = saved_saves;
          sexp_gc_release1(ctx);
        }
      } else {
        res = sexp_native_raise(ctx, callee, "too many args", sexp_list2(ctx, callee, sexp_make_fixnum(nargs)));
        break;
      }
    } else if (sexp_procedure_variadic_p(callee) && !sexp_procedure_unused_rest_p(callee)) {
      call_args[expected] = SEXP_NULL;
      nargs = expected + 1;
    }

    sexp bc = sexp_procedure_code(callee);
    sexp_native_func fn = (sexp_native_func)sexp_bytecode_data(bc);
    res = fn(ctx, callee, nargs, call_args);

    if (res == SEXP_TRAMPOLINE) {
      callee = tramp.proc;
      callee_save = callee;
      nargs = tramp.nargs;
      args = tramp.args;
      continue;
    }

    if (sexp_trampolinep(res)) {
      if (sexp_not(sexp_trampoline_procedure(res)) && sexp_pairp(sexp_trampoline_args(res))) {
        res = sexp_car(sexp_trampoline_args(res));
        break;
      }
      callee = sexp_trampoline_procedure(res);
      callee_save = callee;
      sexp next_args = sexp_trampoline_args(res);
      nargs = sexp_unbox_fixnum(sexp_length(ctx, next_args));
      if (nargs + 2 <= FIXED_ARGS_SIZE) {
        args = fixed_args;
      } else {
        if (nargs + 2 > dyn_capacity) {
          dyn_capacity = (nargs + 2) * 2;
          dyn_args = (sexp*)realloc(dyn_args, sizeof(sexp) * dyn_capacity);
        }
        args = dyn_args;
      }
      sexp ls = next_args;
      for (sexp_sint_t k = 0; k < nargs; k++, ls = sexp_cdr(ls))
        args[k] = sexp_car(ls);
      continue;
    }
    break;
  }

  if (dyn_args) free(dyn_args);
  if (tramp.dyn_args) free(tramp.dyn_args);
  if (tramp.dyn_saves) free(tramp.dyn_saves);
  sexp_context_trampoline(ctx) = tramp.prev;
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_native_make_closure (sexp ctx, sexp flags, sexp num_args, sexp bc,
                               sexp_sint_t num_fvs, sexp *fvs) {
  sexp_gc_var2(vec, res);
  sexp_gc_preserve2(ctx, vec, res);
  vec = sexp_make_vector(ctx, sexp_make_fixnum(num_fvs), SEXP_VOID);
  for (sexp_sint_t k = 0; k < num_fvs; k++) {
    sexp_vector_set(vec, sexp_make_fixnum(k), fvs[k]);
  }
  res = sexp_make_procedure(ctx, flags, num_args, bc, vec);
  sexp_gc_release2(ctx);
  return res;
}

__attribute__((naked)) static int native_setjmp (void *env) {
  __asm__ volatile (
    "movq %rbx, 0(%rdi)\n\t"
    "movq %rsp, 8(%rdi)\n\t"
    "movq %rbp, 16(%rdi)\n\t"
    "movq %r12, 24(%rdi)\n\t"
    "movq %r13, 32(%rdi)\n\t"
    "movq %r14, 40(%rdi)\n\t"
    "movq %r15, 48(%rdi)\n\t"
    "movq (%rsp), %rax\n\t"
    "movq %rax, 56(%rdi)\n\t"
    "xorl %eax, %eax\n\t"
    "ret\n\t"
  );
}

__attribute__((naked)) static void native_longjmp (void *env, int val) {
  __asm__ volatile (
    "movl %esi, %eax\n\t"
    "testl %eax, %eax\n\t"
    "jnz 1f\n\t"
    "movl $1, %eax\n\t"
    "1:\n\t"
    "movq 0(%rdi), %rbx\n\t"
    "movq 8(%rdi), %rsp\n\t"
    "movq 16(%rdi), %rbp\n\t"
    "movq 24(%rdi), %r12\n\t"
    "movq 32(%rdi), %r13\n\t"
    "movq 40(%rdi), %r14\n\t"
    "movq 48(%rdi), %r15\n\t"
    "movq 56(%rdi), %rdx\n\t"
    "movq %rdx, (%rsp)\n\t"
    "ret\n\t"
  );
}

struct sexp_native_cont {
  uint64_t jb[8];
  sexp val;
  struct sexp_gc_var_t *saves;
  void *trampoline;
  int active;
  struct sexp_native_cont *prev;
};

static __thread struct sexp_native_cont *active_conts = NULL;

static sexp sexp_native_cont_call (sexp ctx, sexp self, sexp_sint_t n, sexp val) {
  sexp data = sexp_opcode_data(self);
  struct sexp_native_cont *cont;
  if (!sexp_cpointerp(data))
    return sexp_user_exception(ctx, self, "invalid continuation pointer", data);
  cont = (struct sexp_native_cont*)sexp_cpointer_value(data);
  if (!cont || !cont->active) {
    return sexp_user_exception(ctx, self, "continuation is no longer active", val);
  }
  /* Mark all continuations deeper on the stack (lower rsp) than target as inactive */
  while (active_conts && (uintptr_t)active_conts->jb[1] < (uintptr_t)cont->jb[1]) {
    active_conts->active = 0;
    active_conts = active_conts->prev;
  }
  cont->val = val;
  sexp_context_saves(ctx) = cont->saves;
  sexp_context_trampoline(ctx) = cont->trampoline;
  native_longjmp(cont->jb, 1);
  return SEXP_VOID;
}

__attribute__((noinline)) static sexp sexp_native_callcc_helper (sexp ctx, sexp proc) {
  volatile sexp vctx = ctx;
  struct sexp_native_cont * volatile vcont = (struct sexp_native_cont*)malloc(sizeof(struct sexp_native_cont));
  struct sexp_gc_var_t *old_saves = sexp_context_saves(ctx);
  sexp_gc_var2(cptr, k);
  sexp_gc_preserve2(ctx, cptr, k);

  vcont->saves = old_saves;
  vcont->trampoline = sexp_context_trampoline(ctx);
  vcont->active = 1;
  vcont->val = SEXP_VOID;
  vcont->prev = active_conts;
  active_conts = vcont;

  cptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, (void*)vcont, SEXP_FALSE, 0);
  k = sexp_make_foreign(ctx, "continuation", 1, 0, NULL, (sexp_proc1)sexp_native_cont_call, cptr);

  sexp res;
  if (native_setjmp(vcont->jb) == 0) {
    res = sexp_apply1(vctx, proc, k);
  } else {
    res = vcont->val;
  }
  vcont->active = 0;
  while (active_conts && (uintptr_t)active_conts->jb[1] <= (uintptr_t)vcont->jb[1]) {
    active_conts->active = 0;
    active_conts = active_conts->prev;
  }
  sexp_gc_release2(vctx);
  return res;
}

/************************* Fibers & Threads ***************************/

#if SEXP_USE_GREEN_THREADS
__attribute__((naked)) static void sexp_switch_fiber (void **old_sp, void *new_sp) {
  __asm__ volatile (
    "pushq %rbx\n\t"
    "pushq %rbp\n\t"
    "pushq %r12\n\t"
    "pushq %r13\n\t"
    "pushq %r14\n\t"
    "pushq %r15\n\t"
    "movq %rsp, (%rdi)\n\t"
    "movq %rsi, %rsp\n\t"
    "popq %r15\n\t"
    "popq %r14\n\t"
    "popq %r13\n\t"
    "popq %r12\n\t"
    "popq %rbp\n\t"
    "popq %rbx\n\t"
    "ret\n\t"
  );
}

static __thread sexp sexp_current_fiber_ctx = NULL;

static void sexp_fiber_switch (sexp from_ctx, sexp to_ctx) {
  sexp_context_active_conts(from_ctx) = (void*)active_conts;
  active_conts = (struct sexp_native_cont*)sexp_context_active_conts(to_ctx);
  sexp_current_fiber_ctx = to_ctx;
  sexp_switch_fiber(&sexp_context_fiber_sp(from_ctx), sexp_context_fiber_sp(to_ctx));
  sexp_current_fiber_ctx = from_ctx;
}

sexp sexp_native_yield (sexp ctx) {
  sexp sched = sexp_global(ctx, SEXP_G_THREADS_SCHEDULER);
  if (!sexp_applicablep(sched) || sexp_truep(sexp_global(ctx, SEXP_G_ATOMIC_P))) {
    return SEXP_VOID;
  }
  sexp next_ctx = sexp_apply1(ctx, sched, ctx);
  if (sexp_exceptionp(next_ctx)) {
    return next_ctx;
  }
  if (next_ctx != ctx && sexp_contextp(next_ctx)) {
    sexp_fiber_switch(ctx, next_ctx);
  }
  return SEXP_VOID;
}

static void sexp_fiber_trampoline (void) {
  sexp ctx = sexp_current_fiber_ctx;
  if (!ctx) {
    __asm__ volatile ("movq %%r12, %0" : "=r"(ctx));
  }
  sexp thunk = sexp_context_proc(ctx);
  sexp res;

  res = sexp_apply(ctx, thunk, SEXP_NULL);
  if (sexp_exceptionp(res)) {
    sexp_context_errorp(ctx) = 1;
    sexp_context_result(ctx) = res;
  } else {
    sexp_context_result(ctx) = res;
  }
  sexp_context_refuel(ctx) = 0;
  sexp_native_yield(ctx);

  while (1) {
    sexp_native_yield(ctx);
  }
}

#define SEXP_FIBER_STACK_SIZE (512 * 1024)

void sexp_native_init_fiber (sexp ctx, sexp thread_ctx) {
  size_t size = SEXP_FIBER_STACK_SIZE;
  void *stack_base = mmap(NULL, size, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (stack_base == MAP_FAILED) {
    sexp_context_fiber_stack_base(thread_ctx) = NULL;
    sexp_context_fiber_stack_size(thread_ctx) = 0;
    sexp_context_fiber_sp(thread_ctx) = NULL;
    return;
  }
  mprotect(stack_base, 4096, PROT_NONE);

  sexp_context_fiber_stack_base(thread_ctx) = stack_base;
  sexp_context_fiber_stack_size(thread_ctx) = size;
  sexp_context_active_conts(thread_ctx) = NULL;
  sexp_context_trampoline(thread_ctx) = NULL;

  uintptr_t stack_top = (uintptr_t)stack_base + size;
  uint64_t *sp = (uint64_t*)stack_top;

  *(--sp) = 0;                                 /* stack_top - 8: 16-byte alignment sentinel */
  *(--sp) = (uint64_t)sexp_fiber_trampoline;   /* stack_top - 16: ret target */
  *(--sp) = 0;                                 /* r15 */
  *(--sp) = 0;                                 /* r14 */
  *(--sp) = 0;                                 /* r13 */
  *(--sp) = (uint64_t)thread_ctx;              /* r12 = thread_ctx */
  *(--sp) = 0;                                 /* rbp */
  *(--sp) = 0;                                 /* rbx */

  sexp_context_fiber_sp(thread_ctx) = (void*)sp;
}

void sexp_native_loop_yield (sexp ctx) {
  sexp_context_refuel(ctx) = SEXP_DEFAULT_QUANTUM;
  if (sexp_pairp(sexp_global(ctx, SEXP_G_THREADS_FRONT)) ||
      sexp_pairp(sexp_global(ctx, SEXP_G_THREADS_PAUSED))) {
    sexp_native_yield(ctx);
  }
}
#else
void sexp_native_init_fiber (sexp ctx, sexp thread_ctx) {}
sexp sexp_native_yield (sexp ctx) { return SEXP_VOID; }
void sexp_native_loop_yield (sexp ctx) {}
#endif

/************************* Opcode Dispatch ****************************/

static sexp sexp_native_num_eq (sexp ctx, sexp a, sexp b) {
  if (sexp_fixnump(a) && sexp_fixnump(b)) {
    return (a == b) ? SEXP_TRUE : SEXP_FALSE;
  }
#if SEXP_USE_COMPLEX
  if (sexp_complexp(a)) {
    if (sexp_flonump(sexp_complex_imag(a)) && sexp_flonum_value(sexp_complex_imag(a)) == 0.0) {
      a = sexp_complex_real(a);
    } else if (sexp_complexp(b)) {
      sexp r = sexp_native_num_eq(ctx, sexp_complex_real(a), sexp_complex_real(b));
      if (r != SEXP_TRUE) return r;
      return sexp_native_num_eq(ctx, sexp_complex_imag(a), sexp_complex_imag(b));
    } else if (sexp_numberp(b)) {
      return SEXP_FALSE;
    }
  }
  if (sexp_complexp(b)) {
    if (sexp_flonump(sexp_complex_imag(b)) && sexp_flonum_value(sexp_complex_imag(b)) == 0.0) {
      b = sexp_complex_real(b);
    } else if (sexp_numberp(a)) {
      return SEXP_FALSE;
    }
  }
#endif
  sexp cmp = sexp_compare(ctx, a, b);
  if (sexp_exceptionp(cmp)) {
    if (sexp_stringp(sexp_exception_message(cmp)) &&
        strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(cmp))) == 0)
      return SEXP_FALSE;
    return cmp;
  }
  return (cmp == SEXP_ZERO) ? SEXP_TRUE : SEXP_FALSE;
}

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
  return (d < (int)sexp_vector_length(v))
    && sexp_vector_ref(v, sexp_make_fixnum(d)) == b;
}

static sexp sexp_native_slotn_ref (sexp ctx, sexp self, sexp type, sexp instance, sexp slot) {
  sexp tmp1;
  sexp_sint_t i;
  if (!sexp_typep(type))
    return sexp_native_raise(ctx, self, "slotn-ref: not a record type", sexp_list1(ctx, type));
  else if (!sexp_check_type(ctx, instance, type))
    return sexp_native_raise(ctx, self, "slotn-ref: bad type", sexp_list1(ctx, instance));
  if (!sexp_fixnump(slot)) {
    for (i = 0, tmp1 = sexp_type_slots(type); sexp_pairp(tmp1); tmp1 = sexp_cdr(tmp1), ++i) {
      if (sexp_car(tmp1) == slot) { slot = sexp_make_fixnum(i); break; }
    }
  }
  if (!sexp_fixnump(slot))
    return sexp_native_raise(ctx, self, "slotn-ref: not an integer", sexp_list1(ctx, slot));
  if (sexp_vectorp(sexp_type_getters(type))) {
    if (sexp_unbox_fixnum(slot) < 0 || sexp_unbox_fixnum(slot) >= (sexp_sint_t)sexp_vector_length(sexp_type_getters(type)))
      return sexp_native_raise(ctx, self, "slotn-ref: slot out of bounds", sexp_list2(ctx, slot, sexp_make_fixnum(sexp_type_field_len_base(type))));
    tmp1 = sexp_vector_ref(sexp_type_getters(type), slot);
    if (sexp_opcodep(tmp1))
      return ((sexp_proc2)sexp_opcode_func(tmp1))(ctx, tmp1, 1, instance);
    else
      return sexp_native_raise(ctx, self, "slotn-ref: no getter defined", sexp_list1(ctx, slot));
  } else {
    if (sexp_unbox_fixnum(slot) < 0 || sexp_unbox_fixnum(slot) >= sexp_type_field_len_base(type))
      return sexp_native_raise(ctx, self, "slotn-ref: slot out of bounds", sexp_list2(ctx, slot, sexp_make_fixnum(sexp_type_field_len_base(type))));
    return sexp_slot_ref(instance, sexp_unbox_fixnum(slot));
  }
}

static sexp sexp_native_slotn_set (sexp ctx, sexp self, sexp type, sexp instance, sexp slot, sexp value) {
  sexp tmp1;
  sexp_sint_t i;
  if (!sexp_typep(type))
    return sexp_native_raise(ctx, self, "slotn-set!: not a record type", sexp_list1(ctx, type));
  else if (!sexp_check_type(ctx, instance, type))
    return sexp_native_raise(ctx, self, "slotn-set!: bad type", sexp_list1(ctx, instance));
  else if (sexp_immutablep(instance))
    return sexp_native_raise(ctx, self, "slotn-set!: immutable object", sexp_list1(ctx, instance));
  if (!sexp_fixnump(slot)) {
    for (i = 0, tmp1 = sexp_type_slots(type); sexp_pairp(tmp1); tmp1 = sexp_cdr(tmp1), ++i) {
      if (sexp_car(tmp1) == slot) { slot = sexp_make_fixnum(i); break; }
    }
  }
  if (!sexp_fixnump(slot))
    return sexp_native_raise(ctx, self, "slotn-set!: not an integer", sexp_list1(ctx, slot));
  if (sexp_vectorp(sexp_type_setters(type))) {
    if (sexp_unbox_fixnum(slot) < 0 || sexp_unbox_fixnum(slot) >= (sexp_sint_t)sexp_vector_length(sexp_type_setters(type)))
      return sexp_native_raise(ctx, self, "slotn-set!: slot out of bounds", sexp_list2(ctx, slot, sexp_make_fixnum(sexp_type_field_len_base(type))));
    tmp1 = sexp_vector_ref(sexp_type_setters(type), slot);
    if (sexp_opcodep(tmp1))
      return ((sexp_proc3)sexp_opcode_func(tmp1))(ctx, tmp1, 2, instance, value);
    else
      return sexp_native_raise(ctx, self, "slotn-set!: no setter defined", sexp_list1(ctx, slot));
  } else {
    if (sexp_unbox_fixnum(slot) < 0 || sexp_unbox_fixnum(slot) >= sexp_type_field_len_base(type))
      return sexp_native_raise(ctx, self, "slotn-set!: slot out of bounds", sexp_list2(ctx, slot, sexp_make_fixnum(sexp_type_field_len_base(type))));
    sexp_slot_set(instance, sexp_unbox_fixnum(slot), value);
    return SEXP_VOID;
  }
}

static sexp sexp_native_dispatch_op_inner (sexp ctx, sexp self, sexp op, sexp_sint_t nargs, sexp *args) {
  self = op;
  sexp_sint_t code = sexp_opcode_code(op);
  sexp_sint_t oclass = sexp_opcode_class(op);

  if (oclass == SEXP_OPC_FOREIGN) {
    sexp_proc1 f = sexp_opcode_func(op);
    switch (nargs) {
    case 0: return ((sexp_proc1)f)(ctx, self, 0);
    case 1: return ((sexp_proc2)f)(ctx, self, 1, args[0]);
    case 2: return ((sexp_proc3)f)(ctx, self, 2, args[0], args[1]);
    case 3: return ((sexp_proc4)f)(ctx, self, 3, args[0], args[1], args[2]);
    case 4: return ((sexp_proc5)f)(ctx, self, 4, args[0], args[1], args[2], args[3]);
    case 5: return ((sexp_proc6)f)(ctx, self, 5, args[0], args[1], args[2], args[3], args[4]);
    case 6: return ((sexp_proc7)f)(ctx, self, 6, args[0], args[1], args[2], args[3], args[4], args[5]);
    case 7: return ((sexp_proc8)f)(ctx, self, 7, args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
    case 8: return ((sexp_proc9)f)(ctx, self, 8, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
    default:
      return sexp_user_exception(ctx, self, "too many foreign arguments", op);
    }
  }

  switch (code) {
  case SEXP_OP_ADD:
    if (nargs == 2 && sexp_fixnump(args[0]) && sexp_fixnump(args[1])) {
      sexp_sint_t a = sexp_unbox_fixnum(args[0]), b = sexp_unbox_fixnum(args[1]), c = a + b;
      if (c >= SEXP_MIN_FIXNUM && c <= SEXP_MAX_FIXNUM)
        return sexp_make_fixnum(c);
      return sexp_add(ctx, args[0], args[1]);
    }
    if (nargs == 0) return SEXP_ZERO;
    if (nargs == 1) return args[0];
    {
      sexp res = args[0];
      for (sexp_sint_t i = 1; i < nargs; i++) {
        if (sexp_fixnump(res) && sexp_fixnump(args[i])) {
          sexp_sint_t a = sexp_unbox_fixnum(res), b = sexp_unbox_fixnum(args[i]), c = a + b;
          if (c >= SEXP_MIN_FIXNUM && c <= SEXP_MAX_FIXNUM) {
            res = sexp_make_fixnum(c);
            continue;
          }
        }
        res = sexp_add(ctx, res, args[i]);
      }
      return res;
    }
  case SEXP_OP_SUB:
    if (nargs == 2 && sexp_fixnump(args[0]) && sexp_fixnump(args[1])) {
      sexp_sint_t a = sexp_unbox_fixnum(args[0]), b = sexp_unbox_fixnum(args[1]), c = a - b;
      if (c >= SEXP_MIN_FIXNUM && c <= SEXP_MAX_FIXNUM)
        return sexp_make_fixnum(c);
      return sexp_sub(ctx, args[0], args[1]);
    }
    if (nargs == 0) return SEXP_ZERO;
    if (nargs == 1) return sexp_sub(ctx, SEXP_ZERO, args[0]);
    {
      sexp res = args[0];
      for (sexp_sint_t i = 1; i < nargs; i++) {
        if (sexp_fixnump(res) && sexp_fixnump(args[i])) {
          sexp_sint_t a = sexp_unbox_fixnum(res), b = sexp_unbox_fixnum(args[i]), c = a - b;
          if (c >= SEXP_MIN_FIXNUM && c <= SEXP_MAX_FIXNUM) {
            res = sexp_make_fixnum(c);
            continue;
          }
        }
        res = sexp_sub(ctx, res, args[i]);
      }
      return res;
    }
  case SEXP_OP_MUL:
    if (nargs == 0) return SEXP_ONE;
    if (nargs == 1) return args[0];
    {
      sexp res = args[0];
      for (sexp_sint_t i = 1; i < nargs; i++) {
        res = sexp_mul(ctx, res, args[i]);
      }
      return res;
    }
  case SEXP_OP_DIV:
    if (nargs == 1) {
      if (args[0] == SEXP_ZERO)
        return sexp_user_exception(ctx, self, "divide by zero", SEXP_NULL);
      return sexp_div(ctx, SEXP_ONE, args[0]);
    }
    {
      sexp res = args[0];
      for (sexp_sint_t i = 1; i < nargs; i++) {
        if (args[i] == SEXP_ZERO) {
#if SEXP_USE_FLONUMS
          if (sexp_flonump(res) && sexp_flonum_value(res) == 0.0)
            res = sexp_make_flonum(ctx, 0.0);
          else
#endif
            return sexp_user_exception(ctx, self, "divide by zero", SEXP_NULL);
        } else {
          res = sexp_div(ctx, res, args[i]);
        }
      }
      return res;
    }
  case SEXP_OP_QUOTIENT:
    if (args[1] == SEXP_ZERO)
      return sexp_user_exception(ctx, self, "divide by zero", SEXP_NULL);
    return sexp_quotient(ctx, args[0], args[1]);
  case SEXP_OP_REMAINDER:
    if (args[1] == SEXP_ZERO)
      return sexp_user_exception(ctx, self, "divide by zero", SEXP_NULL);
    return sexp_remainder(ctx, args[0], args[1]);
  case SEXP_OP_LT:
    if (sexp_opcode_inverse(op)) {
      if (nargs == 2 && sexp_fixnump(args[0]) && sexp_fixnump(args[1])) {
        return ((sexp_sint_t)args[0] > (sexp_sint_t)args[1]) ? SEXP_TRUE : SEXP_FALSE;
      }
      for (sexp_sint_t i = 0; i < nargs - 1; i++) {
        if (sexp_fixnump(args[i]) && sexp_fixnump(args[i+1])) {
          if (!((sexp_sint_t)args[i] > (sexp_sint_t)args[i+1])) return SEXP_FALSE;
        } else {
          sexp cmp = sexp_compare(ctx, args[i], args[i+1]);
          if (sexp_exceptionp(cmp)) {
            if (sexp_stringp(sexp_exception_message(cmp)) &&
                strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(cmp))) == 0)
              return SEXP_FALSE;
            return cmp;
          }
          if (sexp_unbox_fixnum(cmp) <= 0) return SEXP_FALSE;
        }
      }
      return SEXP_TRUE;
    } else {
      if (nargs == 2 && sexp_fixnump(args[0]) && sexp_fixnump(args[1])) {
        return ((sexp_sint_t)args[0] < (sexp_sint_t)args[1]) ? SEXP_TRUE : SEXP_FALSE;
      }
      for (sexp_sint_t i = 0; i < nargs - 1; i++) {
        if (sexp_fixnump(args[i]) && sexp_fixnump(args[i+1])) {
          if (!((sexp_sint_t)args[i] < (sexp_sint_t)args[i+1])) return SEXP_FALSE;
        } else {
          sexp cmp = sexp_compare(ctx, args[i], args[i+1]);
          if (sexp_exceptionp(cmp)) {
            if (sexp_stringp(sexp_exception_message(cmp)) &&
                strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(cmp))) == 0)
              return SEXP_FALSE;
            return cmp;
          }
          if (sexp_unbox_fixnum(cmp) >= 0) return SEXP_FALSE;
        }
      }
      return SEXP_TRUE;
    }
  case SEXP_OP_LE:
    if (sexp_opcode_inverse(op)) {
      if (nargs == 2 && sexp_fixnump(args[0]) && sexp_fixnump(args[1])) {
        return ((sexp_sint_t)args[0] >= (sexp_sint_t)args[1]) ? SEXP_TRUE : SEXP_FALSE;
      }
      for (sexp_sint_t i = 0; i < nargs - 1; i++) {
        if (sexp_fixnump(args[i]) && sexp_fixnump(args[i+1])) {
          if (!((sexp_sint_t)args[i] >= (sexp_sint_t)args[i+1])) return SEXP_FALSE;
        } else {
          sexp cmp = sexp_compare(ctx, args[i], args[i+1]);
          if (sexp_exceptionp(cmp)) {
            if (sexp_stringp(sexp_exception_message(cmp)) &&
                strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(cmp))) == 0)
              return SEXP_FALSE;
            return cmp;
          }
          if (sexp_unbox_fixnum(cmp) < 0) return SEXP_FALSE;
        }
      }
      return SEXP_TRUE;
    } else {
      if (nargs == 2 && sexp_fixnump(args[0]) && sexp_fixnump(args[1])) {
        return ((sexp_sint_t)args[0] <= (sexp_sint_t)args[1]) ? SEXP_TRUE : SEXP_FALSE;
      }
      for (sexp_sint_t i = 0; i < nargs - 1; i++) {
        if (sexp_fixnump(args[i]) && sexp_fixnump(args[i+1])) {
          if (!((sexp_sint_t)args[i] <= (sexp_sint_t)args[i+1])) return SEXP_FALSE;
        } else {
          sexp cmp = sexp_compare(ctx, args[i], args[i+1]);
          if (sexp_exceptionp(cmp)) {
            if (sexp_stringp(sexp_exception_message(cmp)) &&
                strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(cmp))) == 0)
              return SEXP_FALSE;
            return cmp;
          }
          if (sexp_unbox_fixnum(cmp) > 0) return SEXP_FALSE;
        }
      }
      return SEXP_TRUE;
    }
  case SEXP_OP_EQN:
    if (nargs == 2 && sexp_fixnump(args[0]) && sexp_fixnump(args[1])) {
      return (args[0] == args[1]) ? SEXP_TRUE : SEXP_FALSE;
    }
    for (sexp_sint_t i = 0; i < nargs - 1; i++) {
      sexp eq = sexp_native_num_eq(ctx, args[i], args[i+1]);
      if (eq != SEXP_TRUE) return eq;
    }
    return SEXP_TRUE;
  case SEXP_OP_EQ:
    return (args[0] == args[1]) ? SEXP_TRUE : SEXP_FALSE;
  case SEXP_OP_CONS:
    return sexp_cons(ctx, args[0], args[1]);
  case SEXP_OP_CAR:
    if (!sexp_pairp(args[0]))
      return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_PAIR, args[0]));
    return sexp_car(args[0]);
  case SEXP_OP_CDR:
    if (!sexp_pairp(args[0]))
      return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_PAIR, args[0]));
    return sexp_cdr(args[0]);
  case SEXP_OP_SET_CAR:
    if (!sexp_pairp(args[0]))
      return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_PAIR, args[0]));
    else if (sexp_immutablep(args[0]))
      return sexp_native_raise(ctx, op, "set-car!: immutable pair", sexp_list1(ctx, args[0]));
    sexp_car(args[0]) = args[1];
    return SEXP_VOID;
  case SEXP_OP_SET_CDR:
    if (!sexp_pairp(args[0]))
      return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_PAIR, args[0]));
    else if (sexp_immutablep(args[0]))
      return sexp_native_raise(ctx, op, "set-cdr!: immutable pair", sexp_list1(ctx, args[0]));
    sexp_cdr(args[0]) = args[1];
    return SEXP_VOID;
  case SEXP_OP_NULLP:
    return sexp_nullp(args[0]) ? SEXP_TRUE : SEXP_FALSE;
  case SEXP_OP_FIXNUMP:
    return sexp_fixnump(args[0]) ? SEXP_TRUE : SEXP_FALSE;
  case SEXP_OP_SYMBOLP:
    return sexp_symbolp(args[0]) ? SEXP_TRUE : SEXP_FALSE;
  case SEXP_OP_CHARP:
    return sexp_charp(args[0]) ? SEXP_TRUE : SEXP_FALSE;
  case SEXP_OP_EOFP:
    return (args[0] == SEXP_EOF) ? SEXP_TRUE : SEXP_FALSE;
  case SEXP_OP_TYPEP:
    return sexp_make_boolean(sexp_check_type(ctx, args[0], sexp_type_by_index(ctx, sexp_unbox_fixnum(sexp_opcode_data(op)))));
  case SEXP_OP_ISA:
    if (!sexp_typep(args[1])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_TYPE, args[1]));
    return sexp_make_boolean(sexp_check_type(ctx, args[0], args[1]));
  case SEXP_OP_VECTOR_REF:
    if (!sexp_vectorp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_VECTOR, args[0]));
    if (!sexp_fixnump(args[1])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_FIXNUM, args[1]));
    {
      sexp_sint_t idx = sexp_unbox_fixnum(args[1]);
      if (idx < 0 || idx >= (sexp_sint_t)sexp_vector_length(args[0]))
        return sexp_native_raise_exception(ctx, op, sexp_user_exception(ctx, self, "vector-ref: index out of range", args[1]));
      return sexp_vector_ref(args[0], args[1]);
    }
  case SEXP_OP_VECTOR_SET:
    if (!sexp_vectorp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_VECTOR, args[0]));
    else if (sexp_immutablep(args[0])) return sexp_native_raise(ctx, op, "vector-set!: immutable vector", sexp_list1(ctx, args[0]));
    if (!sexp_fixnump(args[1])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_FIXNUM, args[1]));
    {
      sexp_sint_t idx = sexp_unbox_fixnum(args[1]);
      if (idx < 0 || idx >= (sexp_sint_t)sexp_vector_length(args[0]))
        return sexp_native_raise_exception(ctx, op, sexp_user_exception(ctx, self, "vector-set!: index out of range", args[1]));
      sexp_vector_set(args[0], args[1], args[2]);
      return SEXP_VOID;
    }
  case SEXP_OP_VECTOR_LENGTH:
    if (!sexp_vectorp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_VECTOR, args[0]));
    return sexp_make_fixnum(sexp_vector_length(args[0]));
  case SEXP_OP_MAKE_VECTOR:
    return sexp_make_vector(ctx, args[0], nargs > 1 ? args[1] : SEXP_VOID);
  case SEXP_OP_BYTES_REF:
    if (!sexp_bytesp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_BYTES, args[0]));
    if (!sexp_fixnump(args[1])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_FIXNUM, args[1]));
    {
      sexp_sint_t idx = sexp_unbox_fixnum(args[1]);
      if (idx < 0 || idx >= (sexp_sint_t)sexp_bytes_length(args[0]))
        return sexp_native_raise_exception(ctx, op, sexp_user_exception(ctx, self, "bytevector-u8-ref: index out of range", args[1]));
      return sexp_bytes_ref(args[0], args[1]);
    }
  case SEXP_OP_BYTES_SET:
    if (!sexp_bytesp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_BYTES, args[0]));
    else if (sexp_immutablep(args[0])) return sexp_native_raise(ctx, op, "byte-vector-set!: immutable byte-vector", sexp_list1(ctx, args[0]));
    if (!sexp_fixnump(args[1])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_FIXNUM, args[1]));
    if (!sexp_fixnump(args[2])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_FIXNUM, args[2]));
    {
      sexp_sint_t idx = sexp_unbox_fixnum(args[1]);
      if (idx < 0 || idx >= (sexp_sint_t)sexp_bytes_length(args[0]))
        return sexp_native_raise_exception(ctx, op, sexp_user_exception(ctx, self, "bytevector-u8-set!: index out of range", args[1]));
      sexp_bytes_set(args[0], args[1], args[2]);
      return SEXP_VOID;
    }
  case SEXP_OP_BYTES_LENGTH:
    if (!sexp_bytesp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_BYTES, args[0]));
    return sexp_make_fixnum(sexp_bytes_length(args[0]));
  case SEXP_OP_STRING_REF:
    return sexp_string_cursor_ref(ctx, args[0], args[1]);
  case SEXP_OP_STRING_SET:
    if (!sexp_stringp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_STRING, args[0]));
    else if (sexp_immutablep(args[0])) return sexp_native_raise(ctx, op, "string-set!: immutable string", sexp_list1(ctx, args[0]));
    sexp_string_cursor_set(ctx, args[0], args[1], args[2]);
    return SEXP_VOID;
  case SEXP_OP_STRING_LENGTH:
    if (!sexp_stringp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_STRING, args[0]));
    return sexp_make_fixnum(sexp_string_length(args[0]));
#if SEXP_USE_UTF8_STRINGS
  case SEXP_OP_SCP:
    return sexp_make_boolean(sexp_string_cursorp(args[0]));
  case SEXP_OP_SC_LT:
    if (sexp_opcode_inverse(op)) {
      for (sexp_sint_t i = 0; i < nargs - 1; i++) {
        if (!((sexp_sint_t)args[i] > (sexp_sint_t)args[i+1])) return SEXP_FALSE;
      }
      return SEXP_TRUE;
    }
    for (sexp_sint_t i = 0; i < nargs - 1; i++) {
      if (!((sexp_sint_t)args[i] < (sexp_sint_t)args[i+1])) return SEXP_FALSE;
    }
    return SEXP_TRUE;
  case SEXP_OP_SC_LE:
    if (sexp_opcode_inverse(op)) {
      for (sexp_sint_t i = 0; i < nargs - 1; i++) {
        if (!((sexp_sint_t)args[i] >= (sexp_sint_t)args[i+1])) return SEXP_FALSE;
      }
      return SEXP_TRUE;
    }
    for (sexp_sint_t i = 0; i < nargs - 1; i++) {
      if (!((sexp_sint_t)args[i] <= (sexp_sint_t)args[i+1])) return SEXP_FALSE;
    }
    return SEXP_TRUE;
  case SEXP_OP_STRING_CURSOR_NEXT:
    if (!sexp_stringp(args[0])) return sexp_type_exception(ctx, self, SEXP_STRING, args[0]);
    if (!sexp_string_cursorp(args[1])) return sexp_user_exception(ctx, self, "string-cursor-next: not a string cursor", args[1]);
    return sexp_string_cursor_next(args[0], args[1]);
  case SEXP_OP_STRING_CURSOR_PREV:
    if (!sexp_stringp(args[0])) return sexp_type_exception(ctx, self, SEXP_STRING, args[0]);
    if (!sexp_string_cursorp(args[1])) return sexp_user_exception(ctx, self, "string-cursor-prev: not a string cursor", args[1]);
    return sexp_string_cursor_prev(args[0], args[1]);
  case SEXP_OP_STRING_CURSOR_END:
    if (!sexp_stringp(args[0])) return sexp_type_exception(ctx, self, SEXP_STRING, args[0]);
    return sexp_make_string_cursor(sexp_string_size(args[0]));
#endif
  case SEXP_OP_CHAR2INT:
    if (!sexp_charp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_CHAR, args[0]));
    return sexp_make_fixnum(sexp_unbox_character(args[0]));
  case SEXP_OP_INT2CHAR:
    if (!sexp_fixnump(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_FIXNUM, args[0]));
    return sexp_make_character(sexp_unbox_fixnum(args[0]));
  case SEXP_OP_CHAR_UPCASE:
    if (!sexp_charp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_CHAR, args[0]));
    return sexp_make_character(toupper(sexp_unbox_character(args[0])));
  case SEXP_OP_CHAR_DOWNCASE:
    if (!sexp_charp(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_CHAR, args[0]));
    return sexp_make_character(tolower(sexp_unbox_character(args[0])));
  case SEXP_OP_WRITE_CHAR:
    return sexp_write_char_op(ctx, self, nargs, args[0], nargs > 1 ? args[1] : sexp_current_output_port(ctx));
  case SEXP_OP_WRITE_STRING:
    {
      sexp str = args[0];
      sexp len_obj = args[1];
      sexp port = nargs > 2 ? args[2] : sexp_current_output_port(ctx);
      sexp_sint_t len;
      const char *data;
      if (sexp_stringp(str)) {
        data = sexp_string_data(str);
        len = (len_obj == SEXP_TRUE) ? sexp_string_size(str) : (sexp_fixnump(len_obj) ? sexp_unbox_fixnum(len_obj) : 0);
      } else if (sexp_bytesp(str)) {
        data = sexp_bytes_data(str);
        len = (len_obj == SEXP_TRUE) ? sexp_bytes_length(str) : (sexp_fixnump(len_obj) ? sexp_unbox_fixnum(len_obj) : 0);
      } else {
        return sexp_type_exception(ctx, self, SEXP_STRING, str);
      }
      sexp_write_string_n(ctx, data, len, port);
      return SEXP_VOID;
    }
  case SEXP_OP_READ_CHAR:
    return sexp_read_char_op(ctx, self, nargs, nargs > 0 ? args[0] : sexp_current_input_port(ctx));
  case SEXP_OP_PEEK_CHAR:
    return sexp_peek_char_op(ctx, self, nargs, nargs > 0 ? args[0] : sexp_current_input_port(ctx));
  case SEXP_OP_APPLY1:
    return sexp_apply(ctx, args[0], args[1]);
  case SEXP_OP_CALLCC:
    return sexp_native_callcc_helper(ctx, args[0]);
  case SEXP_OP_RAISE:
    return sexp_native_raise_exception(ctx, self, args[0]);
  case SEXP_OP_PARAMETER_REF:
    if (nargs == 0) return sexp_parameter_ref(ctx, op);
    return sexp_native_parameter_set(ctx, op, args[0]);
  case SEXP_OP_MAKE:
    return sexp_alloc_tagged(ctx, sexp_unbox_fixnum(sexp_opcode_data2(op)), sexp_unbox_fixnum(sexp_opcode_data(op)));
  case SEXP_OP_SLOT_REF: {
    sexp_uint_t tid = sexp_unbox_fixnum(sexp_opcode_data(op));
    sexp type = sexp_type_by_index(ctx, tid);
    if (!sexp_check_type(ctx, args[0], type))
      return sexp_native_raise(ctx, op, "slot-ref: bad type",
                               sexp_list2(ctx, sexp_type_name_by_index(ctx, tid), args[0]));
    return sexp_slot_ref(args[0], sexp_unbox_fixnum(sexp_opcode_data2(op)));
  }
  case SEXP_OP_SLOT_SET: {
    sexp_uint_t tid = sexp_unbox_fixnum(sexp_opcode_data(op));
    sexp type = sexp_type_by_index(ctx, tid);
    if (!sexp_check_type(ctx, args[0], type))
      return sexp_native_raise(ctx, op, "slot-set!: bad type",
                               sexp_list2(ctx, sexp_type_name_by_index(ctx, tid), args[0]));
    else if (sexp_immutablep(args[0]))
      return sexp_native_raise(ctx, op, "slot-set!: immutable object", sexp_list1(ctx, args[0]));
    sexp_slot_set(args[0], sexp_unbox_fixnum(sexp_opcode_data2(op)), args[1]);
    return SEXP_VOID;
  }
  case SEXP_OP_SLOTN_REF:
    return sexp_native_slotn_ref(ctx, self, args[0], args[1], args[2]);
  case SEXP_OP_SLOTN_SET:
    return sexp_native_slotn_set(ctx, self, args[0], args[1], args[2], args[3]);
  case SEXP_OP_MAKE_EXCEPTION:
    return sexp_make_exception(ctx, args[0], args[1], args[2], args[3], args[4]);
  case SEXP_OP_YIELD:
    return sexp_native_yield(ctx);
  default:
    return sexp_native_raise(ctx, op, "unknown opcode in native dispatch", op);
  }
}

static inline int sexp_opcode_can_fail (sexp_sint_t code, sexp_sint_t oclass) {
  if (oclass == SEXP_OPC_FOREIGN) return 1;
  switch (code) {
  case SEXP_OP_ADD:
  case SEXP_OP_SUB:
  case SEXP_OP_MUL:
  case SEXP_OP_DIV:
  case SEXP_OP_QUOTIENT:
  case SEXP_OP_REMAINDER:
  case SEXP_OP_LT:
  case SEXP_OP_LE:
  case SEXP_OP_EQN:
  case SEXP_OP_STRING_REF:
  case SEXP_OP_STRING_CURSOR_NEXT:
  case SEXP_OP_STRING_CURSOR_PREV:
  case SEXP_OP_READ_CHAR:
  case SEXP_OP_PEEK_CHAR:
  case SEXP_OP_WRITE_CHAR:
  case SEXP_OP_WRITE_STRING:
  case SEXP_OP_APPLY1:
  case SEXP_OP_PARAMETER_REF:
    return 1;
  default:
    return 0;
  }
}

sexp sexp_native_dispatch_op (sexp ctx, sexp self, sexp op, sexp_sint_t nargs, sexp *args) {
  sexp res = sexp_native_dispatch_op_inner(ctx, self, op, nargs, args);
  sexp_sint_t code = sexp_opcode_code(op);
  sexp_sint_t oclass = sexp_opcode_class(op);
  if (sexp_trampolinep(res)) {
    if (sexp_not(sexp_trampoline_procedure(res)) && sexp_pairp(sexp_trampoline_args(res))) {
      return sexp_car(sexp_trampoline_args(res));
    }
  }
  if (sexp_exceptionp(res) && sexp_opcode_can_fail(code, oclass)) {
    res = sexp_native_raise_exception(ctx, op, res);
  }
  return res;
}

/************************* Code Emission Primitives *******************/

static void emit_u8 (sexp ctx, uint8_t b) {
  sexp_emit(ctx, b);
}

static void emit_u32 (sexp ctx, uint32_t dw) {
  emit_u8(ctx, dw & 0xFF);
  emit_u8(ctx, (dw >> 8) & 0xFF);
  emit_u8(ctx, (dw >> 16) & 0xFF);
  emit_u8(ctx, (dw >> 24) & 0xFF);
}

static void emit_u64 (sexp ctx, uint64_t qw) {
  emit_u32(ctx, qw & 0xFFFFFFFF);
  emit_u32(ctx, (qw >> 32) & 0xFFFFFFFF);
}

static void patch_u32 (sexp ctx, sexp_sint_t pos, uint32_t val) {
  sexp bc = sexp_context_bc(ctx);
  unsigned char *data = sexp_bytecode_data(bc) + pos;
  memcpy(data, &val, 4);
}

static void bytecode_preserve (sexp ctx, sexp obj) {
  sexp ls = sexp_bytecode_literals(sexp_context_bc(ctx));
  if (sexp_pointerp(obj) && !sexp_symbolp(obj)
      && sexp_not(sexp_memq(ctx, obj, ls)))
    sexp_push(ctx, sexp_bytecode_literals(sexp_context_bc(ctx)), obj);
}

static int get_param_count (sexp lam) {
  if (!lam || !sexp_lambdap(lam)) return 0;
  sexp ls = sexp_lambda_params(lam);
  int count = 0;
  for (; sexp_pairp(ls); ls = sexp_cdr(ls))
    count++;
  if (!sexp_nullp(ls))
    count++;
  return count;
}

static int get_local_count (sexp lam) {
  if (!lam || !sexp_lambdap(lam)) return 0;
  sexp ls = sexp_lambda_locals(lam);
  int count = 0;
  for (; sexp_pairp(ls); ls = sexp_cdr(ls))
    count++;
  return count;
}

static int get_local_slot (sexp lam, int k) {
  if (k >= 0) return k;
  return get_param_count(lam) + (-(k + 5));
}

static int get_temp_slot (sexp ctx) {
  sexp lam = sexp_context_lambda(ctx);
  int p = get_param_count(lam);
  int l = get_local_count(lam);
  int d = sexp_unbox_fixnum(sexp_context_depth(ctx));
  return p + l + d;
}

static int32_t get_slot_disp (int slot_index) {
  if (slot_index >= SEXP_NATIVE_FRAME_SLOTS - 1) {
    fprintf(stderr, "WARNING: slot_index %d >= %d!\n", slot_index, SEXP_NATIVE_FRAME_SLOTS - 1);
  }
  return -4160 + 8 * slot_index;
}

static void emit_mov_slot_rax (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x85);
  emit_u32(ctx, (uint32_t)disp);
}

static void emit_mov_rax_slot (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x85);
  emit_u32(ctx, (uint32_t)disp);
}

static void emit_clear_slot (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xc7); emit_u8(ctx, 0x85);
  emit_u32(ctx, (uint32_t)disp);
  emit_u32(ctx, (uint32_t)(uintptr_t)SEXP_UNDEF);
}

static void emit_clear_slots (sexp ctx, int start_slot, int count) {
  for (int i = 0; i < count; i++) {
    emit_clear_slot(ctx, start_slot + i);
  }
}

static void emit_mov_imm64_rax (sexp ctx, uint64_t val) {
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xb8);
  emit_u64(ctx, val);
}

static void emit_mov_slot_rcx (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x8d);
  emit_u32(ctx, (uint32_t)disp);
}

static void emit_mov_imm64_rdx (sexp ctx, uint64_t val) {
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xba);
  emit_u64(ctx, val);
}

static sexp_sint_t emit_jcc (sexp ctx, uint8_t cc) {
  sexp_sint_t pos = sexp_unbox_fixnum(sexp_context_pos(ctx));
  emit_u8(ctx, 0x0f);
  emit_u8(ctx, cc);
  emit_u32(ctx, 0);
  return pos;
}

static void patch_jcc (sexp ctx, sexp_sint_t pos) {
  sexp_sint_t target = sexp_unbox_fixnum(sexp_context_pos(ctx));
  int32_t disp = (int32_t)(target - (pos + 6));
  patch_u32(ctx, pos + 2, (uint32_t)disp);
}

static sexp_sint_t emit_jmp (sexp ctx) {
  sexp_sint_t pos = sexp_unbox_fixnum(sexp_context_pos(ctx));
  emit_u8(ctx, 0xe9);
  emit_u32(ctx, 0);
  return pos;
}

static void patch_jmp_to (sexp ctx, sexp_sint_t pos, sexp_sint_t target) {
  int32_t disp = (int32_t)(target - (pos + 5));
  patch_u32(ctx, pos + 1, (uint32_t)disp);
}

static void patch_jmp (sexp ctx, sexp_sint_t pos) {
  patch_jmp_to(ctx, pos, sexp_unbox_fixnum(sexp_context_pos(ctx)));
}

static int is_fixnum_literal (sexp x, sexp_sint_t *val) {
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

static void emit_mov_mem_rax (sexp ctx, int32_t offset) {
  if (offset >= -128 && offset <= 127) {
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x40);
    emit_u8(ctx, (uint8_t)offset);
  } else {
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x80);
    emit_u32(ctx, (uint32_t)offset);
  }
}

static void emit_box_var (sexp ctx, sexp name, int slot_index) {
  bytecode_preserve(ctx, name);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7); // mov %r12, %rdi (ctx)
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xbe);                     // movabs $name, %rsi
  emit_u64(ctx, (uint64_t)name);
  int32_t disp = get_slot_disp(slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x95); // mov disp(%rbp), %rdx
  emit_u32(ctx, (uint32_t)disp);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);                     // movabs $native_cons, %r11
  emit_u64(ctx, (uint64_t)native_cons);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3); // call *%r11
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x85); // mov %rax, disp(%rbp)
  emit_u32(ctx, (uint32_t)disp);
}

static void emit_native_call (sexp ctx, int temp_start, int nargs) {
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc6);
  emit_u8(ctx, 0xba);
  emit_u32(ctx, (uint32_t)nargs);
  int32_t disp = get_slot_disp(temp_start);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x8d);
  emit_u32(ctx, (uint32_t)disp);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
  emit_u64(ctx, (uint64_t)sexp_native_call);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);
}

static void emit_native_tail_call (sexp ctx, int temp_start, int nargs) {
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc6);
  emit_u8(ctx, 0xba);
  emit_u32(ctx, (uint32_t)nargs);
  int32_t disp = get_slot_disp(temp_start);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x8d);
  emit_u32(ctx, (uint32_t)disp);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
  emit_u64(ctx, (uint64_t)sexp_native_make_trampoline);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);
}

static void emit_get_param_value (sexp ctx, sexp param_op, int slot) {
  bytecode_preserve(ctx, param_op);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xbe);
  emit_u64(ctx, (uint64_t)param_op);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
  emit_u64(ctx, (uint64_t)sexp_parameter_ref);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);
  if (slot >= 0)
    emit_mov_rax_slot(ctx, slot);
}

sexp sexp_native_parameter_set (sexp ctx, sexp param_op, sexp val) {
  if (sexp_opcodep(param_op) && sexp_pairp(sexp_opcode_data(param_op))) {
    sexp conv = sexp_opcode_data2(param_op);
    if (conv && sexp_applicablep(conv)) {
      sexp_gc_var2(val_save, conv_save);
      sexp_gc_preserve2(ctx, val_save, conv_save);
      val_save = val;
      conv_save = conv;
      val = sexp_apply1(ctx, conv_save, val_save);
      sexp_gc_release2(ctx);
      if (sexp_exceptionp(val))
        return sexp_native_raise_exception(ctx, param_op, val);
    }
    sexp_cdr(sexp_opcode_data(param_op)) = val;
  }
  return SEXP_VOID;
}

static void emit_set_param_value (sexp ctx, sexp param_op, int val_slot) {
  bytecode_preserve(ctx, param_op);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xbe);
  emit_u64(ctx, (uint64_t)param_op);
  emit_mov_slot_rax(ctx, val_slot);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc2);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
  emit_u64(ctx, (uint64_t)sexp_native_parameter_set);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);
}

static void emit_native_dispatch_op (sexp ctx, sexp op, int temp_start, int nargs) {
  bytecode_preserve(ctx, op);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xee);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xba);
  emit_u64(ctx, (uint64_t)op);
  emit_u8(ctx, 0xb9);
  emit_u32(ctx, (uint32_t)nargs);
  int32_t disp = get_slot_disp(temp_start);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x85);
  emit_u32(ctx, (uint32_t)disp);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
  emit_u64(ctx, (uint64_t)sexp_native_dispatch_op);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);
}

static void emit_native_make_closure (sexp ctx, sexp flags, sexp len, sexp bc, int num_fvs, int temp_start) {
  bytecode_preserve(ctx, bc);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xbe);
  emit_u64(ctx, (uint64_t)flags);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xba);
  emit_u64(ctx, (uint64_t)len);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xb9);
  emit_u64(ctx, (uint64_t)bc);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xb8);
  emit_u32(ctx, (uint32_t)num_fvs);
  int32_t disp = get_slot_disp(temp_start);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x8d);
  emit_u32(ctx, (uint32_t)disp);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
  emit_u64(ctx, (uint64_t)sexp_native_make_closure);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);
}

/************************* Leaf Procedure Analysis ********************/

static int is_leaf_expr (sexp ctx, sexp lam, sexp x, sexp self_name, sexp self_loc, int tailp) {
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
    return is_leaf_expr(ctx, lam, test, self_name, self_loc, 0) &&
           is_leaf_expr(ctx, lam, sexp_cnd_pass(x), self_name, self_loc, tailp) &&
           is_leaf_expr(ctx, lam, sexp_cnd_fail(x), self_name, self_loc, tailp);
  }
  case SEXP_SEQ: {
    sexp ls = sexp_seq_ls(x);
    for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
      int cur_tailp = sexp_nullp(sexp_cdr(ls)) ? tailp : 0;
      if (!is_leaf_expr(ctx, lam, sexp_car(ls), self_name, self_loc, cur_tailp))
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
      case SEXP_OP_VECTOR_LENGTH:
      case SEXP_OP_STRING_LENGTH:
      case SEXP_OP_BYTES_LENGTH:
      case SEXP_OP_CHAR2INT:
      case SEXP_OP_INT2CHAR:
        break;
      default:
        return 0;
      }
      sexp ls = sexp_cdr(x);
      for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
        if (!is_leaf_expr(ctx, lam, sexp_car(ls), self_name, self_loc, 0))
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

static int is_leaf_procedure (sexp ctx, sexp lam, sexp name, sexp loc) {
  if (!name || !sexp_symbolp(name)) return 0;
  if (!lam || !sexp_lambdap(lam)) return 0;
  if (!sexp_truep(sexp_listp(ctx, sexp_lambda_params(lam)))) return 0;
  if (!sexp_nullp(sexp_lambda_fv(lam))) return 0;
  if (sexp_pairp(sexp_lambda_sv(lam))) return 0;
  if (sexp_pairp(sexp_lambda_locals(lam))) return 0;
  return is_leaf_expr(ctx, lam, sexp_lambda_body(lam), name, loc, 1);
}

/************************* Prologue & Epilogue ************************/

void sexp_emit_enter (sexp ctx) {
  sexp lam = sexp_context_lambda(ctx);
  int num_params = (lam && sexp_lambdap(lam)) ? get_param_count(lam) : 0;
  int is_leaf = (lam && sexp_lambdap(lam) && sexp_lambda_flags(lam) == SEXP_TRUE);

  emit_u8(ctx, 0x55);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe5);
  emit_u8(ctx, 0x53);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x54);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x55);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x56);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x57);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x81); emit_u8(ctx, 0xec);
  emit_u32(ctx, SEXP_NATIVE_FRAME_ALLOC);

  emit_u8(ctx, 0x49); emit_u8(ctx, 0x89); emit_u8(ctx, 0xfc);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0x89); emit_u8(ctx, 0xf5);

  if (is_leaf) {
    for (int i = 0; i < num_params; i++) {
      int32_t offset = i * 8;
      if (offset == 0) {
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x01);
      } else if (offset <= 127) {
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x41); emit_u8(ctx, (uint8_t)offset);
      } else {
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x81); emit_u32(ctx, (uint32_t)offset);
      }
      emit_mov_rax_slot(ctx, i);
    }
  } else {
    emit_u8(ctx, 0x49); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc8);
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xd1);
    emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe6);
    emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xea);
    emit_u8(ctx, 0x41); emit_u8(ctx, 0xb9);
    emit_u32(ctx, (uint32_t)(((SEXP_NATIVE_FRAME_SLOTS - 1) << 16) | (num_params & 0xFFFF)));

    emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
    emit_u64(ctx, (uint64_t)sexp_native_enter);
    emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);
  }
}

void sexp_emit_return (sexp ctx) {
  sexp lam = sexp_context_lambda(ctx);
  int is_leaf = (lam && sexp_lambdap(lam) && sexp_lambda_flags(lam) == SEXP_TRUE);

  if (!is_leaf) {
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc3);

    emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe6);
    emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
    emit_u64(ctx, (uint64_t)sexp_native_exit);
    emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);

    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xd8);
  }

  emit_u8(ctx, 0x48); emit_u8(ctx, 0x81); emit_u8(ctx, 0xc4);
  emit_u32(ctx, SEXP_NATIVE_FRAME_ALLOC);

  emit_u8(ctx, 0x41); emit_u8(ctx, 0x5f);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x5e);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x5d);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x5c);
  emit_u8(ctx, 0x5b);
  emit_u8(ctx, 0x5d);
  emit_u8(ctx, 0xc3);
}

void sexp_bless_bytecode (sexp ctx, sexp bc) {
  unsigned char *code = (unsigned char*)sexp_bytecode_data(bc);
  sexp lam = sexp_context_lambda(ctx);
  int is_leaf = (lam && sexp_lambdap(lam) && sexp_lambda_flags(lam) == SEXP_TRUE);
  if (!is_leaf && sexp_bytecode_length(bc) >= 60 && code[0] == 0x55 && code[1] == 0x48
      && code[2] == 0x89 && code[3] == 0xe5 && code[41] == 0x41 && code[42] == 0xb9) {
    uint32_t cur = *(uint32_t*)(code + 43);
    uint32_t num_params = cur & 0xFFFF;
    int num_locals = (lam && sexp_lambdap(lam)) ? get_local_count(lam) : 0;
    int max_depth = sexp_bytecode_max_depth(bc);
    int num_slots = num_params + num_locals + max_depth + 4;
    if (num_slots < (int)num_params + 1) num_slots = num_params + 1;
    if (num_slots > SEXP_NATIVE_FRAME_SLOTS - 2) num_slots = SEXP_NATIVE_FRAME_SLOTS - 2;
    *(uint32_t*)(code + 43) = ((uint32_t)num_slots << 16) | (num_params & 0xFFFF);
  }

  long page_size = sysconf(_SC_PAGESIZE);
  uintptr_t start = (uintptr_t)sexp_bytecode_data(bc);
  uintptr_t end = start + sexp_bytecode_length(bc);
  uintptr_t page_start = start & ~(page_size - 1);
  size_t len = (end - page_start + page_size - 1) & ~(page_size - 1);
  mprotect((void*)page_start, len, PROT_READ | PROT_WRITE | PROT_EXEC);
}

/************************* AST Generation *****************************/

static void generate_lit (sexp ctx, sexp value) {
  emit_mov_imm64_rax(ctx, (uint64_t)value);
  bytecode_preserve(ctx, value);
}

static void generate_non_global_ref (sexp ctx, sexp name, sexp cell,
                                     sexp lambda, sexp fv, int unboxp) {
  sexp loc = sexp_cdr(cell);
  if (loc == lambda && sexp_lambdap(lambda)) {
    int k = sexp_param_index(ctx, lambda, name);
    int slot = get_local_slot(lambda, k);
    emit_mov_slot_rax(ctx, slot);
  } else {
    int i = 0;
    for (; sexp_pairp(fv); fv = sexp_cdr(fv), i++) {
      if ((name == sexp_ref_name(sexp_car(fv)))
          && (loc == sexp_ref_loc(sexp_car(fv))))
        break;
    }
    // Load self->vars (offset 16 of %r13)
    emit_u8(ctx, 0x49); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x45); emit_u8(ctx, 0x10);
    // Load vars->data[i] (offset 16 + 8*i of %rax)
    emit_mov_mem_rax(ctx, 16 + 8 * i);
  }
  if (unboxp && sexp_truep(sexp_memq(ctx, name, sexp_lambda_sv(loc)))) {
    emit_mov_mem_rax(ctx, 16); // cdr of box pair
  }
}

static void generate_ref (sexp ctx, sexp ref, int unboxp) {
  sexp loc = sexp_ref_loc(ref);
  if (!sexp_lambdap(loc)) {
    sexp cell = sexp_ref_cell(ref);
    bytecode_preserve(ctx, cell);
    emit_mov_imm64_rax(ctx, (uint64_t)cell);
    if (unboxp) {
      emit_mov_mem_rax(ctx, 16); // cell->cdr
      if (sexp_cdr(cell) == SEXP_UNDEF) {
        // cmpq $SEXP_UNDEF, %rax
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x3d);
        emit_u32(ctx, (uint32_t)(uint64_t)SEXP_UNDEF);
        // jne .Lknown (rel32)
        emit_u8(ctx, 0x0f); emit_u8(ctx, 0x85);
        sexp_sint_t jne_pos = sexp_unbox_fixnum(sexp_context_pos(ctx));
        emit_u32(ctx, 0);

        // Slow path: call sexp_native_delayed_ref(ctx, cell)
        // movq %r12, %rdi
        emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
        // movabsq $cell, %rsi
        emit_u8(ctx, 0x48); emit_u8(ctx, 0xbe);
        emit_u64(ctx, (uint64_t)cell);
        // movabsq $sexp_native_delayed_ref, %r11
        emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
        emit_u64(ctx, (uint64_t)sexp_native_delayed_ref);
        // callq *%r11
        emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);

        // .Lknown:
        sexp_sint_t known_pos = sexp_unbox_fixnum(sexp_context_pos(ctx));
        patch_u32(ctx, jne_pos, (uint32_t)(known_pos - (jne_pos + 4)));
      }
    }
  } else {
    sexp lam = sexp_context_lambda(ctx);
    if (!lam || !sexp_lambdap(lam)) {
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_VOID);
    } else {
      generate_non_global_ref(ctx, sexp_ref_name(ref), sexp_ref_cell(ref),
                              lam, sexp_lambda_fv(lam), unboxp);
    }
  }
}

static void generate_set (sexp ctx, sexp set) {
  sexp ref = sexp_set_var(set);
  int temp_slot = get_temp_slot(ctx);

  sexp_context_tailp(ctx) = 0;
  if (sexp_lambdap(sexp_set_value(set))) {
    sexp_lambda_name(sexp_set_value(set)) = sexp_ref_name(ref);
    generate(ctx, sexp_ref_name(ref), sexp_ref_loc(ref), sexp_set_value(set), sexp_set_value(set));
  } else {
    generate(ctx, 0, 0, 0, sexp_set_value(set));
  }

  // Save new value in temp slot
  emit_mov_rax_slot(ctx, temp_slot);

  if (!sexp_lambdap(sexp_ref_loc(ref))) {
    // Global set: cell->cdr = new value
    sexp cell = sexp_ref_cell(ref);
    bytecode_preserve(ctx, cell);
    emit_u8(ctx, 0x48); emit_u8(ctx, 0xb9); // movabsq $cell, %rcx
    emit_u64(ctx, (uint64_t)cell);
    emit_mov_slot_rax(ctx, temp_slot);
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x41); emit_u8(ctx, 0x10); // movq %rax, 16(%rcx)
  } else {
    sexp lambda = sexp_ref_loc(ref);
    if (sexp_truep(sexp_memq(ctx, sexp_ref_name(ref), sexp_lambda_sv(lambda)))) {
      // Boxed mutable var: fetch box, then box->cdr = new value
      generate_ref(ctx, ref, 0); // box is in %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc1); // mov %rax, %rcx
      emit_mov_slot_rax(ctx, temp_slot);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x41); emit_u8(ctx, 0x10); // movq %rax, 16(%rcx)
    } else {
      // Local unboxed var
      int k = sexp_param_index(ctx, lambda, sexp_ref_name(ref));
      int slot = get_local_slot(lambda, k);
      emit_mov_slot_rax(ctx, temp_slot);
      emit_mov_rax_slot(ctx, slot);
    }
  }

  emit_clear_slot(ctx, temp_slot);
  emit_mov_imm64_rax(ctx, (uint64_t)SEXP_VOID);
}

static int try_generate_fused_cnd (sexp ctx, sexp name, sexp loc, sexp lam,
                                    sexp test, sexp pass, sexp fail, sexp_sint_t tailp) {
  if (!sexp_pairp(test) || !sexp_opcodep(sexp_car(test)))
    return 0;

  sexp op = sexp_car(test);
  sexp_sint_t code = sexp_opcode_code(op);
  sexp_sint_t oclass = sexp_opcode_class(op);
  sexp_sint_t num_args = sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(test)));

  if (oclass == SEXP_OPC_PARAMETER)
    return 0;

  sexp_sint_t fail_jcc = -1;
  sexp_sint_t jz_slow = -1;
  int has_slow = 0;
  int const_arg1 = 0;
  int const_arg0 = 0;
  sexp_sint_t c = 0;
  int temp_start = get_temp_slot(ctx);

  sexp_context_tailp(ctx) = 0;

  if (num_args == 1) {
    sexp arg0 = sexp_cadr(test);
    if (code == SEXP_OP_NULLP) {
      generate(ctx, 0, 0, 0, arg0);
      // cmpq $SEXP_NULL, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x3d);
      emit_u32(ctx, (uint32_t)(uintptr_t)SEXP_NULL);
      // Fail if not equal: jne .Lfail
      fail_jcc = emit_jcc(ctx, 0x85);
    } else if (code == SEXP_OP_FIXNUMP) {
      generate(ctx, 0, 0, 0, arg0);
      // testb $1, %al
      emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
      // Fail if zero (not fixnum): jz .Lfail
      fail_jcc = emit_jcc(ctx, 0x84);
    } else {
      return 0;
    }
  } else if (num_args == 2) {
    sexp arg0 = sexp_cadr(test);
    sexp arg1 = sexp_caddr(test);

    if (code == SEXP_OP_EQ) {
      generate(ctx, 0, 0, 0, arg0);
      emit_mov_rax_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, 1);
      generate(ctx, 0, 0, 0, arg1);
      emit_mov_slot_rcx(ctx, temp_start);
      emit_clear_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, -1);
      // cmpq %rax, %rcx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x39); emit_u8(ctx, 0xc1);
      // Fail if not equal: jne .Lfail
      fail_jcc = emit_jcc(ctx, 0x85);
    } else if (code == SEXP_OP_LT || code == SEXP_OP_LE || code == SEXP_OP_EQN) {
      const_arg1 = is_fixnum_literal(arg1, &c) && (c >= -1073741824LL && c <= 1073741823LL);
      const_arg0 = !const_arg1 && is_fixnum_literal(arg0, &c) && (c >= -1073741824LL && c <= 1073741823LL);

      if (const_arg1) {
        generate(ctx, 0, 0, 0, arg0);
        emit_mov_rax_slot(ctx, temp_start);
        sexp_inc_context_depth(ctx, 2);
        // testb $1, %al
        emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
        jz_slow = emit_jcc(ctx, 0x84);

        sexp_sint_t tagged = (sexp_sint_t)sexp_make_fixnum(c);
        if (tagged >= -128 && tagged <= 127) {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xf8);
          emit_u8(ctx, (uint8_t)(int8_t)tagged);
        } else {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x3d);
          emit_u32(ctx, (uint32_t)(int32_t)tagged);
        }
        uint8_t fail_cc;
        if (code == SEXP_OP_LT) {
          fail_cc = sexp_opcode_inverse(op) ? 0x8e /* jle */ : 0x8d /* jge */;
        } else if (code == SEXP_OP_LE) {
          fail_cc = sexp_opcode_inverse(op) ? 0x8c /* jl */ : 0x8f /* jg */;
        } else {
          fail_cc = 0x85; /* jne */
        }
        fail_jcc = emit_jcc(ctx, fail_cc);
        sexp_inc_context_depth(ctx, -2);
        has_slow = 1;
      } else if (const_arg0) {
        generate(ctx, 0, 0, 0, arg1);
        emit_mov_rax_slot(ctx, temp_start + 1);
        sexp_inc_context_depth(ctx, 2);
        // testb $1, %al
        emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
        jz_slow = emit_jcc(ctx, 0x84);

        sexp_sint_t tagged = (sexp_sint_t)sexp_make_fixnum(c);
        if (tagged >= -128 && tagged <= 127) {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xf8);
          emit_u8(ctx, (uint8_t)(int8_t)tagged);
        } else {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x3d);
          emit_u32(ctx, (uint32_t)(int32_t)tagged);
        }
        // computes arg1 - c = arg1 - arg0
        uint8_t fail_cc;
        if (code == SEXP_OP_LT) {
          fail_cc = sexp_opcode_inverse(op) ? 0x8d /* jge */ : 0x8e /* jle */;
        } else if (code == SEXP_OP_LE) {
          fail_cc = sexp_opcode_inverse(op) ? 0x8f /* jg */ : 0x8c /* jl */;
        } else {
          fail_cc = 0x85; /* jne */
        }
        fail_jcc = emit_jcc(ctx, fail_cc);
        sexp_inc_context_depth(ctx, -2);
        has_slow = 1;
      } else {
        generate(ctx, 0, 0, 0, arg0);
        emit_mov_rax_slot(ctx, temp_start);
        sexp_inc_context_depth(ctx, 1);
        generate(ctx, 0, 0, 0, arg1);
        emit_mov_rax_slot(ctx, temp_start + 1);
        sexp_inc_context_depth(ctx, 1);
        emit_mov_slot_rcx(ctx, temp_start);

        // movq %rcx, %rdx
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xca);
        // andq %rax, %rdx
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x21); emit_u8(ctx, 0xc2);
        // testb $1, %dl
        emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc2); emit_u8(ctx, 0x01);
        jz_slow = emit_jcc(ctx, 0x84);

        // cmpq %rax, %rcx (computes arg0 - arg1)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x39); emit_u8(ctx, 0xc1);
        uint8_t fail_cc;
        if (code == SEXP_OP_LT) {
          fail_cc = sexp_opcode_inverse(op) ? 0x8e /* jle */ : 0x8d /* jge */;
        } else if (code == SEXP_OP_LE) {
          fail_cc = sexp_opcode_inverse(op) ? 0x8c /* jl */ : 0x8f /* jg */;
        } else {
          fail_cc = 0x85; /* jne */
        }
        fail_jcc = emit_jcc(ctx, fail_cc);
        sexp_inc_context_depth(ctx, -2);
        has_slow = 1;
      }
    } else {
      return 0;
    }
  } else {
    return 0;
  }

  // Branch layout:
  // Fast path falls through directly into pass!
  sexp_sint_t pass_start = sexp_unbox_fixnum(sexp_context_pos(ctx));
  sexp_context_tailp(ctx) = tailp;
  generate(ctx, name, loc, lam, pass);

  // jmp .Lend: e9 <disp32>
  sexp_sint_t jmp_end = emit_jmp(ctx);

  sexp_sint_t slow_fail_jcc = -1;
  if (has_slow) {
    patch_jcc(ctx, jz_slow);
    if (const_arg1) {
      generate_lit(ctx, sexp_make_fixnum(c));
      emit_mov_rax_slot(ctx, temp_start + 1);
    } else if (const_arg0) {
      generate_lit(ctx, sexp_make_fixnum(c));
      emit_mov_rax_slot(ctx, temp_start);
    }
    emit_native_dispatch_op(ctx, op, temp_start, 2);
    emit_clear_slots(ctx, temp_start, 2);
    // cmpq $SEXP_FALSE, %rax
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xf8);
    emit_u8(ctx, (uint8_t)(sexp_uint_t)SEXP_FALSE);
    slow_fail_jcc = emit_jcc(ctx, 0x84); // je .Lfail

    // jmp to pass_start
    sexp_sint_t jmp_pass = emit_jmp(ctx);
    patch_jmp_to(ctx, jmp_pass, pass_start);
  }

  // .Lfail:
  patch_jcc(ctx, fail_jcc);
  if (has_slow && slow_fail_jcc != -1) {
    patch_jcc(ctx, slow_fail_jcc);
  }

  sexp_context_tailp(ctx) = tailp;
  generate(ctx, name, loc, lam, fail);

  // .Lend:
  patch_jmp(ctx, jmp_end);

  return 1;
}

static void generate_cnd (sexp ctx, sexp name, sexp loc, sexp lam, sexp cnd) {
  sexp_sint_t tailp = sexp_context_tailp(ctx);
  sexp test = sexp_cnd_test(cnd);
  sexp pass = sexp_cnd_pass(cnd);
  sexp fail = sexp_cnd_fail(cnd);

  while (sexp_pairp(test) && sexp_refp(sexp_car(test)) &&
         sexp_ref_name(sexp_car(test)) == sexp_intern(ctx, "not", 3) &&
         !sexp_lambdap(sexp_ref_loc(sexp_car(test))) &&
         sexp_pairp(sexp_cdr(test)) && sexp_nullp(sexp_cddr(test))) {
    test = sexp_cadr(test);
    sexp tmp = pass; pass = fail; fail = tmp;
  }

  if (try_generate_fused_cnd(ctx, name, loc, lam, test, pass, fail, tailp)) {
    return;
  }

  sexp_context_tailp(ctx) = 0;
  generate(ctx, name, loc, lam, test);

  // cmpq $SEXP_FALSE, %rax
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xf8);
  emit_u8(ctx, (uint8_t)(sexp_uint_t)SEXP_FALSE);

  // je .Lfail: 0f 84 <disp32>
  sexp_sint_t je_pos = emit_jcc(ctx, 0x84);

  sexp_context_tailp(ctx) = tailp;
  generate(ctx, name, loc, lam, pass);

  // jmp .Lend: e9 <disp32>
  sexp_sint_t jmp_pos = emit_jmp(ctx);

  // .Lfail:
  patch_jcc(ctx, je_pos);

  sexp_context_tailp(ctx) = tailp;
  generate(ctx, name, loc, lam, fail);

  // .Lend:
  patch_jmp(ctx, jmp_pos);
}

static void generate_seq (sexp ctx, sexp name, sexp loc, sexp lam, sexp app) {
  sexp head = app, tail = sexp_cdr(app);
  sexp_uint_t tailp = sexp_context_tailp(ctx);
  sexp_context_tailp(ctx) = 0;
  for (; sexp_pairp(tail); head = tail, tail = sexp_cdr(tail)) {
    if (sexp_pointerp(sexp_car(head)) && !sexp_litp(sexp_car(head))) {
      generate(ctx, name, loc, lam, sexp_car(head));
    }
  }
  sexp_context_tailp(ctx) = (char)tailp;
  generate(ctx, name, loc, lam, sexp_car(head));
}

static int try_generate_inlined_opcode (sexp ctx, sexp app, sexp op, sexp_sint_t num_args, int temp_start) {
  sexp_sint_t code = sexp_opcode_code(op);

  // 0-argument cases
  if (num_args == 0) {
    if (code == SEXP_OP_ADD) {
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_ZERO);
      return 1;
    }
    return 0;
  }

  // 1-argument cases
  if (num_args == 1) {
    sexp arg0 = sexp_cadr(app);

    if (code == SEXP_OP_ADD) {
      // (+ a) => a
      generate(ctx, 0, 0, 0, arg0);
      return 1;
    }

    if (code == SEXP_OP_NULLP) {
      generate(ctx, 0, 0, 0, arg0);
      // cmpq $SEXP_NULL, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x3d);
      emit_u32(ctx, (uint32_t)(uintptr_t)SEXP_NULL);
      // movq $SEXP_FALSE, %rax
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_FALSE);
      // movq $SEXP_TRUE, %rdx
      emit_mov_imm64_rdx(ctx, (uint64_t)SEXP_TRUE);
      // cmoveq %rdx, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x44); emit_u8(ctx, 0xc2);
      return 1;
    }

    if (code == SEXP_OP_FIXNUMP) {
      generate(ctx, 0, 0, 0, arg0);
      // testb $1, %al
      emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
      // movq $SEXP_FALSE, %rax
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_FALSE);
      // movq $SEXP_TRUE, %rdx
      emit_mov_imm64_rdx(ctx, (uint64_t)SEXP_TRUE);
      // cmovneq %rdx, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x45); emit_u8(ctx, 0xc2);
      return 1;
    }

    if (code == SEXP_OP_SUB) {
      // Unary minus: (- a)
      generate(ctx, 0, 0, 0, arg0);
      emit_mov_rax_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, 1);

      // testb $1, %al
      emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
      sexp_sint_t jz_slow = emit_jcc(ctx, 0x84);

      // mov $2, %rdx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xc7); emit_u8(ctx, 0xc2); emit_u32(ctx, 2);
      // subq %rax, %rdx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x29); emit_u8(ctx, 0xc2);
      sexp_sint_t jo_slow = emit_jcc(ctx, 0x80);
      // movq %rdx, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xd0);

      sexp_sint_t done_jmp = emit_jmp(ctx);

      patch_jcc(ctx, jz_slow);
      patch_jcc(ctx, jo_slow);
      emit_native_dispatch_op(ctx, op, temp_start, 1);

      patch_jmp(ctx, done_jmp);
      emit_clear_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, -1);
      return 1;
    }

    if (code == SEXP_OP_CAR || code == SEXP_OP_CDR) {
      generate(ctx, 0, 0, 0, arg0);
      emit_mov_rax_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, 1);

      // testb $3, %al (must be pointer tag 00)
      emit_u8(ctx, 0xa8); emit_u8(ctx, 0x03);
      sexp_sint_t jnz_slow = emit_jcc(ctx, 0x85);

      // testq %rax, %rax (must not be NULL)
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x85); emit_u8(ctx, 0xc0);
      sexp_sint_t jz_slow = emit_jcc(ctx, 0x84);

      // cmpl $SEXP_PAIR, 0(%rax) (tag must be SEXP_PAIR)
      emit_u8(ctx, 0x83); emit_u8(ctx, 0x38); emit_u8(ctx, (uint8_t)SEXP_PAIR);
      sexp_sint_t jne_slow = emit_jcc(ctx, 0x85);

      // Fast load
      int32_t off = (code == SEXP_OP_CAR)
        ? (int32_t)offsetof(struct sexp_struct, value.pair.car)
        : (int32_t)offsetof(struct sexp_struct, value.pair.cdr);
      emit_mov_mem_rax(ctx, off);

      sexp_sint_t done_jmp = emit_jmp(ctx);

      patch_jcc(ctx, jnz_slow);
      patch_jcc(ctx, jz_slow);
      patch_jcc(ctx, jne_slow);
      emit_native_dispatch_op(ctx, op, temp_start, 1);

      patch_jmp(ctx, done_jmp);
      emit_clear_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, -1);
      return 1;
    }

    return 0;
  }

  // 2-argument cases
  if (num_args == 2) {
    sexp arg0 = sexp_cadr(app);
    sexp arg1 = sexp_caddr(app);

    // Check for constant fixnum optimization on ADD / SUB
    sexp_sint_t c;
    int const_arg1 = (code == SEXP_OP_ADD || code == SEXP_OP_SUB) &&
                     is_fixnum_literal(arg1, &c) && (c >= -1073741824LL && c <= 1073741823LL);
    int const_arg0 = !const_arg1 && (code == SEXP_OP_ADD) && is_fixnum_literal(arg0, &c) && (c >= -1073741824LL && c <= 1073741823LL);
    if (const_arg1 || const_arg0) {
      sexp var_arg = const_arg1 ? arg0 : arg1;
      int64_t diff = (int64_t)((uint64_t)c << 1);

      generate(ctx, 0, 0, 0, var_arg);
      emit_mov_rax_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, 2);

      // testb $1, %al
      emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
      sexp_sint_t jz_slow = emit_jcc(ctx, 0x84);

      if (code == SEXP_OP_SUB) {
        if (diff >= -128 && diff <= 127) {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xe8); emit_u8(ctx, (uint8_t)(int8_t)diff);
        } else {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x2d); emit_u32(ctx, (uint32_t)(int32_t)diff);
        }
      } else {
        if (diff >= -128 && diff <= 127) {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xc0); emit_u8(ctx, (uint8_t)(int8_t)diff);
        } else {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x05); emit_u32(ctx, (uint32_t)(int32_t)diff);
        }
      }
      sexp_sint_t jo_slow = emit_jcc(ctx, 0x80);

      sexp_sint_t done_jmp = emit_jmp(ctx);

      patch_jcc(ctx, jz_slow);
      patch_jcc(ctx, jo_slow);
      if (const_arg1) {
        generate_lit(ctx, sexp_make_fixnum(c));
        emit_mov_rax_slot(ctx, temp_start + 1);
      } else {
        emit_mov_slot_rax(ctx, temp_start);
        emit_mov_rax_slot(ctx, temp_start + 1);
        generate_lit(ctx, sexp_make_fixnum(c));
        emit_mov_rax_slot(ctx, temp_start);
      }
      emit_native_dispatch_op(ctx, op, temp_start, 2);

      patch_jmp(ctx, done_jmp);
      emit_clear_slots(ctx, temp_start, 2);
      sexp_inc_context_depth(ctx, -2);
      return 1;
    }

    if (code == SEXP_OP_EQ) {
      // (eq? a b)
      generate(ctx, 0, 0, 0, arg0);
      emit_mov_rax_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, 1);
      generate(ctx, 0, 0, 0, arg1);
      emit_mov_slot_rcx(ctx, temp_start);
      emit_clear_slot(ctx, temp_start);

      // cmpq %rax, %rcx (48 39 c1)
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x39); emit_u8(ctx, 0xc1);
      // movq $SEXP_FALSE, %rax
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_FALSE);
      // movq $SEXP_TRUE, %rdx
      emit_mov_imm64_rdx(ctx, (uint64_t)SEXP_TRUE);
      // cmoveq %rdx, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x44); emit_u8(ctx, 0xc2);

      sexp_inc_context_depth(ctx, -1);
      return 1;
    }

    if (code == SEXP_OP_ADD || code == SEXP_OP_SUB ||
        code == SEXP_OP_LT || code == SEXP_OP_LE || code == SEXP_OP_EQN) {
      generate(ctx, 0, 0, 0, arg0);
      emit_mov_rax_slot(ctx, temp_start);
      sexp_inc_context_depth(ctx, 1);

      generate(ctx, 0, 0, 0, arg1);
      emit_mov_rax_slot(ctx, temp_start + 1);
      sexp_inc_context_depth(ctx, 1);

      // Load arg0 from temp_start into %rcx
      emit_mov_slot_rcx(ctx, temp_start);

      // Check if both are fixnums:
      // movq %rcx, %rdx (48 89 ca)
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xca);
      // andq %rax, %rdx (48 21 c2)
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x21); emit_u8(ctx, 0xc2);
      // testb $1, %dl (f6 c2 01)
      emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc2); emit_u8(ctx, 0x01);
      sexp_sint_t jz_slow = emit_jcc(ctx, 0x84);

      sexp_sint_t jo_slow = -1;

      if (code == SEXP_OP_ADD) {
        // addq %rcx, %rax (48 01 c8)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x01); emit_u8(ctx, 0xc8);
        jo_slow = emit_jcc(ctx, 0x80);
        // decq %rax (48 ff c8)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0xff); emit_u8(ctx, 0xc8);
      } else if (code == SEXP_OP_SUB) {
        // subq %rax, %rcx (48 29 c1)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x29); emit_u8(ctx, 0xc1);
        jo_slow = emit_jcc(ctx, 0x80);
        // incq %rcx (48 ff c1)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0xff); emit_u8(ctx, 0xc1);
        // movq %rcx, %rax (48 89 c8)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc8);
      } else {
        // Comparisons: cmpq %rax, %rcx (48 39 c1)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x39); emit_u8(ctx, 0xc1);
        // movq $SEXP_FALSE, %rax
        emit_mov_imm64_rax(ctx, (uint64_t)SEXP_FALSE);
        // movq $SEXP_TRUE, %rdx
        emit_mov_imm64_rdx(ctx, (uint64_t)SEXP_TRUE);

        if (code == SEXP_OP_LT) {
          if (sexp_opcode_inverse(op)) {
            // cmovg %rdx, %rax (48 0f 4f c2)
            emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x4f); emit_u8(ctx, 0xc2);
          } else {
            // cmovl %rdx, %rax (48 0f 4c c2)
            emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x4c); emit_u8(ctx, 0xc2);
          }
        } else if (code == SEXP_OP_LE) {
          if (sexp_opcode_inverse(op)) {
            // cmovge %rdx, %rax (48 0f 4d c2)
            emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x4d); emit_u8(ctx, 0xc2);
          } else {
            // cmovle %rdx, %rax (48 0f 4e c2)
            emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x4e); emit_u8(ctx, 0xc2);
          }
        } else if (code == SEXP_OP_EQN) {
          // cmove %rdx, %rax (48 0f 44 c2)
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x44); emit_u8(ctx, 0xc2);
        }
      }

      sexp_sint_t done_jmp = emit_jmp(ctx);

      patch_jcc(ctx, jz_slow);
      if (jo_slow != -1) patch_jcc(ctx, jo_slow);
      emit_native_dispatch_op(ctx, op, temp_start, 2);

      patch_jmp(ctx, done_jmp);
      emit_clear_slots(ctx, temp_start, 2);
      sexp_inc_context_depth(ctx, -2);
      return 1;
    }
  }

  return 0;
}

static void generate_opcode_app (sexp ctx, sexp app) {
  sexp op = sexp_car(app);
  sexp_sint_t num_args = sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(app)));
  int inv_default = 0;
  int push_default = 0;
  int temp_start = get_temp_slot(ctx);
  sexp ls;

  sexp_context_tailp(ctx) = 0;

  if (sexp_opcode_class(op) == SEXP_OPC_PARAMETER) {
    if (num_args == 0) {
      emit_get_param_value(ctx, op, -1);
      return;
    } else {
      generate(ctx, 0, 0, 0, sexp_cadr(app));
      emit_mov_rax_slot(ctx, temp_start);
      emit_set_param_value(ctx, op, temp_start);
      emit_clear_slot(ctx, temp_start);
      return;
    }
  }

  // Check for optional argument default
  if ((sexp_opcode_class(op) != SEXP_OPC_PARAMETER)
      && (num_args == sexp_opcode_num_args(op))
      && sexp_opcode_variadic_p(op) && sexp_opcode_data(op)) {
    if (sexp_opcode_inverse(op)) {
      inv_default = 1;
    } else {
      push_default = 1;
    }
  }

  if (inv_default) {
    // Push default first, then the argument
    if (sexp_opcode_opt_param_p(op) && sexp_opcodep(sexp_opcode_data(op))) {
      emit_get_param_value(ctx, sexp_opcode_data(op), temp_start);
    } else {
      generate_lit(ctx, sexp_opcode_data(op));
      emit_mov_rax_slot(ctx, temp_start);
    }
    sexp_inc_context_depth(ctx, 1);
    generate(ctx, 0, 0, 0, sexp_cadr(app));
    emit_mov_rax_slot(ctx, temp_start + 1);
    sexp_inc_context_depth(ctx, 1);
    num_args++;
  } else {
    if (!push_default && try_generate_inlined_opcode(ctx, app, op, num_args, temp_start)) {
      return;
    }
    int i = 0;
    for (ls = sexp_cdr(app); sexp_pairp(ls); ls = sexp_cdr(ls), i++) {
      generate(ctx, 0, 0, 0, sexp_car(ls));
      emit_mov_rax_slot(ctx, temp_start + i);
      sexp_inc_context_depth(ctx, 1);
    }
    if (push_default) {
      if (sexp_opcode_opt_param_p(op) && sexp_opcodep(sexp_opcode_data(op))) {
        emit_get_param_value(ctx, sexp_opcode_data(op), temp_start + i);
      } else {
        generate_lit(ctx, sexp_opcode_data(op));
        emit_mov_rax_slot(ctx, temp_start + i);
      }
      sexp_inc_context_depth(ctx, 1);
      num_args++;
    }
  }

  emit_native_dispatch_op(ctx, op, temp_start, (int)num_args);
  emit_clear_slots(ctx, temp_start, (int)num_args);
  sexp_inc_context_depth(ctx, -(int)num_args);
}

static void generate_tail_jump (sexp ctx, sexp name, sexp loc, sexp lam, sexp app) {
  sexp ls1, ls2;
  int num_args = (int)sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(app)));
  int temp_start = get_temp_slot(ctx);
  int loop_start = (int)sexp_context_top(ctx);

  sexp_context_tailp(ctx) = 0;
  int i = 0;
  for (ls1 = sexp_cdr(app); sexp_pairp(ls1); ls1 = sexp_cdr(ls1), i++) {
    generate(ctx, 0, 0, 0, sexp_car(ls1));
    emit_mov_rax_slot(ctx, temp_start + i);
    sexp_inc_context_depth(ctx, 1);
  }

  // Copy temporaries to parameter slots
  for (i = 0, ls2 = sexp_lambda_params(lam); i < num_args && sexp_pairp(ls2); i++, ls2 = sexp_cdr(ls2)) {
    emit_mov_slot_rax(ctx, temp_start + i);
    if (sexp_truep(sexp_memq(ctx, sexp_car(ls2), sexp_lambda_sv(lam)))) {
      // Boxed mutable param: load box from slot i, update box->cdr
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc1); // mov %rax, %rcx (new value)
      emit_mov_slot_rax(ctx, i); // %rax = box pair
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x48); emit_u8(ctx, 0x10); // mov %rcx, 16(%rax)
    } else {
      emit_mov_rax_slot(ctx, i);
    }
  }

  emit_clear_slots(ctx, temp_start, num_args);

  sexp_inc_context_depth(ctx, -num_args);

#if SEXP_USE_GREEN_THREADS
  uint32_t refuel_off = (uint32_t)offsetof(struct sexp_struct, value.context.refuel);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xff); emit_u8(ctx, 0x8c); emit_u8(ctx, 0x24); // decq refuel_off(%r12)
  emit_u32(ctx, refuel_off);
  emit_u8(ctx, 0x7f); emit_u8(ctx, 0x10); // jg skip (16 bytes)
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7); // mov %r12, %rdi
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb); // movabs $sexp_native_loop_yield, %r11
  emit_u64(ctx, (uint64_t)sexp_native_loop_yield);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3); // call *%r11
#endif

  // Jump to loop_start
  sexp_sint_t cur_pos = sexp_unbox_fixnum(sexp_context_pos(ctx));
  int32_t jump_disp = (int32_t)(loop_start - (cur_pos + 5));
  emit_u8(ctx, 0xe9); // jmp rel32
  emit_u32(ctx, (uint32_t)jump_disp);

  sexp_context_tailp(ctx) = 1;
}

static void generate_general_app (sexp ctx, sexp app) {
  sexp_sint_t len = sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(app)));
  int temp_start = get_temp_slot(ctx);
  sexp ls;
  sexp_gc_var1(args);
  sexp_gc_preserve1(ctx, args);
  sexp_uint_t tailp = sexp_context_tailp(ctx);

  sexp_context_tailp(ctx) = 0;
  int i = 0;
  for (ls = sexp_cdr(app); sexp_pairp(ls); ls = sexp_cdr(ls), i++) {
    generate(ctx, 0, 0, 0, sexp_car(ls));
    emit_mov_rax_slot(ctx, temp_start + i);
    sexp_inc_context_depth(ctx, 1);
  }

  // Evaluate the operator
  generate(ctx, 0, 0, 0, sexp_car(app));

  if (tailp) {
    emit_native_tail_call(ctx, temp_start, (int)len);
  } else {
    emit_native_call(ctx, temp_start, (int)len);
    emit_clear_slots(ctx, temp_start, (int)len);
  }
  sexp_inc_context_depth(ctx, -(int)len);
  sexp_gc_release1(ctx);
}

static void generate_lambda (sexp ctx, sexp name, sexp loc, sexp lam, sexp lambda) {
  sexp ctx2, fv, ls, flags, len, ref, prev_lambda, prev_fv;
  sexp_gc_var2(tmp, bc);
  if (sexp_exceptionp(sexp_context_exception(ctx))) return;

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

  if (!name && sexp_symbolp(sexp_lambda_name(lambda))) {
    name = sexp_lambda_name(lambda);
  }
  if (name && !loc && sexp_symbolp(name)) {
    loc = sexp_env_cell(ctx, sexp_context_env(ctx), name, 0);
  }

  int is_leaf = is_leaf_procedure(ctx2, lambda, name, loc);
  if (is_leaf) {
    sexp_lambda_flags(lambda) = SEXP_TRUE;
  } else {
    sexp_lambda_flags(lambda) = SEXP_FALSE;
  }

  // Emit function entry
  sexp_emit_enter(ctx2);

  // Box mutable variables
  for (ls = sexp_lambda_sv(lambda); sexp_pairp(ls); ls = sexp_cdr(ls)) {
    int k = sexp_param_index(ctx, lambda, sexp_car(ls));
    int slot = get_local_slot(lambda, k);
    emit_box_var(ctx2, sexp_car(ls), slot);
  }

  // Record loop start for self-tail jumps
  sexp_context_top(ctx2) = sexp_unbox_fixnum(sexp_context_pos(ctx2));

  // Generate body
  sexp_context_tailp(ctx2) = 1;
  generate(ctx2, name, loc, lambda, sexp_lambda_body(lambda));

  flags = sexp_make_fixnum(sexp_not(sexp_listp(ctx, sexp_lambda_params(lambda)))
                           ? (SEXP_PROC_VARIADIC + (sexp_rest_unused_p(lambda) ? SEXP_PROC_UNUSED_REST : 0))
                           : SEXP_PROC_NONE);
  len = sexp_length(ctx2, sexp_lambda_params(lambda));
  bc = sexp_complete_bytecode(ctx2);

  if (sexp_exceptionp(bc)) {
    sexp_context_exception(ctx) = bc;
  } else {
    sexp_bytecode_name(bc) = sexp_lambda_name(lambda);
    if (sexp_nullp(fv)) {
      // No free variables: static procedure object
      tmp = sexp_make_vector(ctx2, SEXP_ZERO, SEXP_VOID);
      tmp = sexp_make_procedure(ctx2, flags, len, bc, tmp);
      bytecode_preserve(ctx, tmp);
      generate_lit(ctx, tmp);
    } else {
      // Dynamic closure: evaluate free variables and create procedure at runtime
      int num_fvs = (int)sexp_unbox_fixnum(sexp_length(ctx, fv));
      int temp_start = get_temp_slot(ctx);
      int k = 0;
      for (ls = fv; sexp_pairp(ls); ls = sexp_cdr(ls), k++) {
        ref = sexp_car(ls);
        generate_non_global_ref(ctx, sexp_ref_name(ref), sexp_ref_cell(ref),
                                prev_lambda, prev_fv, 0);
        emit_mov_rax_slot(ctx, temp_start + k);
        sexp_inc_context_depth(ctx, 1);
      }
      emit_native_make_closure(ctx, flags, len, bc, num_fvs, temp_start);
      emit_clear_slots(ctx, temp_start, num_fvs);
      sexp_inc_context_depth(ctx, -num_fvs);
    }
  }
  sexp_gc_release2(ctx);
}

static void generate_app (sexp ctx, sexp name, sexp loc, sexp lam, sexp app) {
  if (sexp_opcodep(sexp_car(app))) {
    generate_opcode_app(ctx, app);
  } else if (sexp_context_tailp(ctx) && sexp_refp(sexp_car(app))
             && name && (name == sexp_ref_name(sexp_car(app)))
             && (loc == sexp_ref_loc(sexp_car(app)))
             && lam
             && sexp_listp(ctx, sexp_lambda_params(lam))
             && (sexp_unbox_fixnum(sexp_length(ctx, sexp_cdr(app)))
                 == get_param_count(lam))) {
    generate_tail_jump(ctx, name, loc, lam, app);
  } else {
    generate_general_app(ctx, app);
  }
}

static void generate (sexp ctx, sexp name, sexp loc, sexp lam, sexp x) {
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

void sexp_generate (sexp ctx, sexp name, sexp loc, sexp lam, sexp x) {
  generate(ctx, name, loc, lam, x);
}

/************************* Virtual Machine Interface ******************/

static sexp make_param_list (sexp ctx, sexp_uint_t i) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_NULL;
  for (; i > 0; i--)
    res = sexp_cons(ctx, sexp_make_fixnum(i), res);
  sexp_gc_release1(ctx);
  return res;
}

static sexp make_opcode_procedure (sexp ctx, sexp op, sexp_uint_t i, sexp_sint_t flags) {
  int j = i + (flags & SEXP_PROC_VARIADIC);
  sexp ls, res, env;
  sexp_gc_var6(bc, params, ref, refs, lambda, ctx2);
  if (j == sexp_opcode_num_args(op)) {
    if (sexp_opcode_proc(op)) return sexp_opcode_proc(op);
  } else if (j < sexp_opcode_num_args(op)) {
    return sexp_compile_error(ctx, "not enough args for opcode", op);
  } else if (!sexp_opcode_variadic_p(op)) {
    return sexp_compile_error(ctx, "too many args for opcode", op);
  }
  sexp_gc_preserve6(ctx, bc, params, ref, refs, lambda, ctx2);
  params = make_param_list(ctx, j);
  lambda = sexp_make_lambda(ctx, params);
  ctx2 = sexp_make_child_context(ctx, lambda);
  env = sexp_extend_env(ctx2, sexp_context_env(ctx), params, lambda);
  if (sexp_exceptionp(env)) {
    res = env;
  } else {
    sexp_context_env(ctx2) = env;
    for (ls = params, refs = SEXP_NULL; sexp_pairp(ls); ls = sexp_cdr(ls)) {
      ref = sexp_make_ref(ctx2, sexp_car(ls), sexp_env_cell(ctx, env, sexp_car(ls), 0));
      if (!sexp_exceptionp(ref)) sexp_push(ctx2, refs, ref);
    }
    if (!sexp_exceptionp(refs))
      refs = sexp_reverse(ctx2, refs);
    refs = sexp_cons(ctx2, op, refs);
    if (sexp_exceptionp(refs)) {
      res = refs;
    } else {
      sexp_emit_enter(ctx2);
      generate_opcode_app(ctx2, refs);
      bc = sexp_complete_bytecode(ctx2);
      sexp_bytecode_name(bc) = sexp_opcode_name(op);
      res = sexp_make_procedure(ctx2, sexp_make_fixnum(flags), sexp_make_fixnum(i), bc, SEXP_VOID);
      if (j == sexp_opcode_num_args(op))
        sexp_opcode_proc(op) = res;
    }
  }
  sexp_gc_release6(ctx);
  return res;
}

sexp sexp_make_foreign_proc (sexp ctx, const char *name, int num_args, int flags,
                             const char *fname, sexp_proc1 f) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_make_foreign(ctx, name, num_args + ((flags & SEXP_PROC_VARIADIC) > 0), 0, fname, f, NULL);
  if (!sexp_exceptionp(res))
    res = make_opcode_procedure(ctx, res, num_args, flags);
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_define_foreign_proc_aux (sexp ctx, sexp env, const char *name, int num_args,
                                   int flags, const char *fname, sexp_proc1 f, sexp data) {
  sexp_gc_var2(sym, res);
  sexp_gc_preserve2(ctx, sym, res);
  res = sexp_make_foreign_proc(ctx, name, num_args, flags, fname, f);
  if (!sexp_exceptionp(res))
    sexp_env_define(ctx, env, sym = sexp_intern(ctx, name, -1), res);
  sexp_gc_release2(ctx);
  return res;
}

/************************* Apply Functions ****************************/

sexp sexp_apply (sexp ctx, sexp proc, sexp args) {
  sexp_sint_t len = 0;
  sexp ls = args;
  for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
    len++;
  }
  if (!sexp_nullp(ls)) {
    return sexp_user_exception(ctx, proc, "apply: improper args list", sexp_list1(ctx, args));
  }
  sexp *argv = (sexp*)alloca(sizeof(sexp) * (len > 0 ? len : 1));
  ls = args;
  for (sexp_sint_t i = 0; i < len; i++, ls = sexp_cdr(ls)) {
    argv[i] = sexp_car(ls);
  }
  return sexp_native_call(ctx, proc, len, argv);
}

sexp sexp_apply1 (sexp ctx, sexp f, sexp x) {
  if (sexp_opcodep(f) && sexp_opcode_func(f)) {
    return ((sexp_proc2)sexp_opcode_func(f))(ctx, f, 1, x);
  } else {
    sexp args[1] = { x };
    return sexp_native_call(ctx, f, 1, args);
  }
}

sexp sexp_apply2 (sexp ctx, sexp f, sexp x, sexp y) {
  if (sexp_opcodep(f) && sexp_opcode_func(f)) {
    return ((sexp_proc3)sexp_opcode_func(f))(ctx, f, 2, x, y);
  } else {
    sexp args[2] = { x, y };
    return sexp_native_call(ctx, f, 2, args);
  }
}

sexp sexp_apply3 (sexp ctx, sexp f, sexp x, sexp y, sexp z) {
  if (sexp_opcodep(f) && sexp_opcode_func(f)) {
    return ((sexp_proc4)sexp_opcode_func(f))(ctx, f, 3, x, y, z);
  } else {
    sexp args[3] = { x, y, z };
    return sexp_native_call(ctx, f, 3, args);
  }
}

sexp sexp_apply_no_err_handler (sexp ctx, sexp proc, sexp args) {
  sexp res, err_cell;
  sexp_gc_var1(handler);
  sexp_gc_preserve1(ctx, handler);
  err_cell = sexp_global(ctx, SEXP_G_ERR_HANDLER);
  err_cell = sexp_opcodep(err_cell) ? sexp_opcode_data(err_cell) : SEXP_FALSE;
  handler = sexp_pairp(err_cell) ? sexp_cdr(err_cell) : SEXP_FALSE;
  if (sexp_pairp(err_cell)) sexp_cdr(err_cell) = SEXP_FALSE;
  res = sexp_apply(ctx, proc, args);
  if (sexp_pairp(err_cell)) sexp_cdr(err_cell) = handler;
  sexp_gc_release1(ctx);
  return res;
}

/************************* Stub functions *****************************/

sexp sexp_get_stack_trace (sexp ctx) {
  return SEXP_NULL;
}

void sexp_print_extracted_stack_trace (sexp ctx, sexp trace, sexp out) {
}

void sexp_stack_trace (sexp ctx, sexp out) {
}

sexp sexp_stack_trace_op (sexp ctx, sexp self, sexp_sint_t n, sexp out) {
  return SEXP_VOID;
}

sexp sexp_print_exception_stack_trace_op (sexp ctx, sexp self, sexp_sint_t n, sexp exn, sexp out) {
  return SEXP_VOID;
}

int sexp_poll_port (sexp ctx, sexp port, int inputp) {
  return 0;
}

sexp sexp_fcall (sexp ctx, sexp self, sexp_sint_t n, sexp f) {
  return SEXP_VOID;
}

sexp sexp_write_char_op (sexp ctx, sexp self, sexp_sint_t n, sexp ch, sexp out) {
  if (n < 2 || !out) out = sexp_current_output_port(ctx);
  if (!sexp_charp(ch)) return sexp_type_exception(ctx, self, SEXP_CHAR, ch);
  if (!sexp_oportp(out)) return sexp_type_exception(ctx, self, SEXP_OPORT, out);
  if (!sexp_port_openp(out)) return sexp_user_exception(ctx, self, "write-char: port is closed", out);
#if SEXP_USE_UTF8_STRINGS
  if (sexp_unbox_character(ch) >= 0x80)
    sexp_write_utf8_char(ctx, sexp_unbox_character(ch), out);
  else
#endif
    sexp_write_char(ctx, sexp_unbox_character(ch), out);
  return SEXP_VOID;
}

sexp sexp_newline_op (sexp ctx, sexp self, sexp_sint_t n, sexp out) {
  if (n < 1 || !out) out = sexp_current_output_port(ctx);
  sexp_newline(ctx, out);
  return SEXP_VOID;
}

sexp sexp_read_char_op (sexp ctx, sexp self, sexp_sint_t n, sexp in) {
  if (n < 1 || !in) in = sexp_current_input_port(ctx);
  if (!sexp_iportp(in)) return sexp_type_exception(ctx, self, SEXP_IPORT, in);
  if (!sexp_port_openp(in)) return sexp_user_exception(ctx, self, "read-char: port is closed", in);
  int i = sexp_read_char(ctx, in);
  if (i == EOF) return SEXP_EOF;
#if SEXP_USE_UTF8_STRINGS
  if (i >= 0x80) return sexp_read_utf8_char(ctx, in, i);
#endif
  if (i == '\n') sexp_port_line(in)++;
  return sexp_make_character(i);
}

sexp sexp_peek_char_op (sexp ctx, sexp self, sexp_sint_t n, sexp in) {
  if (n < 1 || !in) in = sexp_current_input_port(ctx);
  if (!sexp_iportp(in)) return sexp_type_exception(ctx, self, SEXP_IPORT, in);
  if (!sexp_port_openp(in)) return sexp_user_exception(ctx, self, "peek-char: port is closed", in);
  int i = sexp_read_char(ctx, in);
  if (i == EOF) return SEXP_EOF;
#if SEXP_USE_UTF8_STRINGS
  if (i >= 0x80) {
    sexp tmp = sexp_read_utf8_char(ctx, in, i);
    sexp_push_utf8_char(ctx, sexp_unbox_character(tmp), in);
    return tmp;
  }
#endif
  sexp_push_char(ctx, i, in);
  return sexp_make_character(i);
}

sexp sexp_char_upcase (sexp ctx, sexp self, sexp_sint_t n, sexp ch) {
  return sexp_make_character(toupper(sexp_unbox_character(ch)));
}

sexp sexp_char_downcase (sexp ctx, sexp self, sexp_sint_t n, sexp ch) {
  return sexp_make_character(tolower(sexp_unbox_character(ch)));
}

sexp sexp_five (sexp ctx, sexp self, sexp_sint_t n, sexp a, sexp b, sexp c, sexp d, sexp e) {
  return sexp_make_fixnum(5);
}
