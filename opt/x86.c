/*  x86.c -- native amd64 backend                             */
/*  Copyright (c) 2009-2026 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#define _GNU_SOURCE
#include "chibi/eval.h"
#include <alloca.h>
#include <pthread.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>

struct sexp_native_frame {
  struct sexp_gc_var_t *old_saves;
  sexp old_proc;
  sexp self;
};

static inline uint32_t compute_frame_alloc (int num_slots) {
  uint32_t needed = 24 * (num_slots + 2);
  return ((needed + 7) & ~15) + 8;
}

static inline int32_t compute_slot_base (int num_slots) {
  uint32_t frame_alloc = compute_frame_alloc(num_slots);
  return -16 - (int32_t)frame_alloc + 16 * (num_slots + 1);
}

/* Forward declarations */
static void generate (sexp ctx, sexp name, sexp loc, sexp lam, sexp x);
static void generate_lambda (sexp ctx, sexp name, sexp loc, sexp lam, sexp lambda);
static void generate_lit (sexp ctx, sexp value);
static sexp make_opcode_procedure (sexp ctx, sexp op, sexp_uint_t i, sexp_sint_t flags);

static void sexp_inc_context_depth (sexp ctx, sexp_sint_t off) {
  sexp_context_depth(ctx) = sexp_fx_add(sexp_context_depth(ctx), sexp_make_fixnum(off));
  if (sexp_unbox_fixnum(sexp_context_depth(ctx))
      > sexp_unbox_fixnum(sexp_context_max_depth(ctx)))
    sexp_context_max_depth(ctx) = sexp_context_depth(ctx);
}

/************************* GC Preservation ****************************/

static void *sexp_native_get_context_stack_base (sexp ctx);

void sexp_native_enter (sexp ctx, struct sexp_native_frame *f, sexp self,
                        sexp_sint_t nargs, sexp *args, sexp_sint_t packed_params) {
  int i;
  sexp_sint_t num_params = packed_params & 0xFFFF;
  sexp_sint_t num_slots = (packed_params >> 16) & 0xFFFF;
  if (num_slots == 0) num_slots = num_params + 1;

  f->old_saves = sexp_context_saves(ctx);
  f->old_proc = sexp_context_proc(ctx);
  f->self = self;
  sexp_context_proc(ctx) = self;

  struct sexp_gc_var_t *saves = (struct sexp_gc_var_t*)((char*)f + sizeof(struct sexp_native_frame));
  sexp *slots = (sexp*)((char*)saves + sizeof(struct sexp_gc_var_t) * (num_slots + 1));

  for (i = 0; i < num_slots; i++) {
    slots[i] = (i < num_params && i < nargs && args) ? args[i] : SEXP_UNDEF;
    saves[i].var = &slots[i];
    saves[i].next = (i == 0) ? f->old_saves : &saves[i - 1];
  }
  slots[num_slots] = self;
  saves[num_slots].var = &slots[num_slots];
  saves[num_slots].next = (num_slots > 0) ? &saves[num_slots - 1] : f->old_saves;
  sexp_context_saves(ctx) = &saves[num_slots];
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

#define FIXED_ARGS_SIZE 16

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
  sexp *call_dyn_args;
  size_t call_dyn_capacity;
  /* An uncaught raise unwinds to the innermost call from C, which */
  /* returns the exception to its caller, as the VM does. */
  int entry;
  uint64_t abort_jb[8];
  sexp abort_val;
  struct sexp_gc_var_t *abort_saves;
  sexp abort_proc;
} sexp_native_trampoline_t;

static void sexp_native_free_trampoline (sexp_native_trampoline_t *tramp) {
  free(tramp->call_dyn_args);
  free(tramp->dyn_args);
  free(tramp->dyn_saves);
}

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

__attribute__((returns_twice)) static int native_setjmp (void *env);
__attribute__((noreturn)) static void native_longjmp (void *env, int val);

/* Abort to the innermost call from C, which returns exn to its */
/* caller.  Only returns if there's no such call to abort to. */
static sexp sexp_native_abort (sexp ctx, sexp exn) {
  sexp_native_trampoline_t *tramp = (sexp_native_trampoline_t*)sexp_context_trampoline(ctx);
  while (tramp && !tramp->entry)
    tramp = tramp->prev;
  if (!tramp) return exn;
  /* free the native calls being skipped */
  while ((sexp_native_trampoline_t*)sexp_context_trampoline(ctx) != tramp) {
    sexp_native_trampoline_t *t = (sexp_native_trampoline_t*)sexp_context_trampoline(ctx);
    sexp_native_free_trampoline(t);
    sexp_context_trampoline(ctx) = t->prev;
  }
  tramp->abort_val = exn;
  sexp_context_saves(ctx) = tramp->abort_saves;
  sexp_context_proc(ctx) = tramp->abort_proc;
  native_longjmp(tramp->abort_jb, 1);
}

static sexp sexp_native_call1 (sexp ctx, sexp f, sexp x);
static sexp sexp_native_apply_list (sexp ctx, sexp proc, sexp args, int entry);

typedef sexp (*sexp_proc8)(sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc9)(sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);

static sexp sexp_native_raise_exception (sexp ctx, sexp self, sexp exn) {
  sexp handler = sexp_parameter_ref(ctx, sexp_global(ctx, SEXP_G_ERR_HANDLER));
  if (sexp_procedurep(handler)) {
    return sexp_native_call1(ctx, handler, exn);
  } else {
#if SEXP_USE_GREEN_THREADS
    sexp_context_errorp(ctx) = 1;
#endif
    if (!sexp_exceptionp(exn))
      exn = sexp_make_exception(ctx, SEXP_UNCAUGHT, SEXP_FALSE, exn, self, SEXP_FALSE);
    return sexp_native_abort(ctx, exn);
  }
}

/* a param failed the type check made on entry for it */
static sexp sexp_native_param_type_error (sexp ctx, sexp self, sexp arg, sexp_sint_t type) {
  return sexp_native_raise_exception(ctx, self, sexp_type_exception(ctx, self, type, arg));
}

static sexp sexp_native_raise (sexp ctx, sexp self, const char *msg, sexp irritants) {
  sexp exn = sexp_user_exception(ctx, self, msg, irritants);
  return sexp_native_raise_exception(ctx, self, exn);
}

sexp sexp_native_delayed_ref (sexp ctx, sexp cell) {
  if (sexp_cdr(cell) == SEXP_UNDEF && sexp_synclop(sexp_car(cell))) {
    sexp tmp1 = sexp_env_cell(ctx, sexp_synclo_env(sexp_car(cell)),
                              sexp_synclo_expr(sexp_car(cell)), 0);
    if (tmp1 != NULL) cell = tmp1;
  }
  if (sexp_cdr(cell) == SEXP_UNDEF) {
    return sexp_native_raise(ctx, sexp_car(cell), "undefined variable",
                             sexp_list1(ctx, sexp_car(cell)));
  }
  return sexp_cdr(cell);
}

static sexp sexp_native_call_aux (sexp ctx, sexp callee, sexp_sint_t nargs, sexp *args, int entry) {
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
  tramp.call_dyn_args = NULL;
  tramp.call_dyn_capacity = 0;
  tramp.entry = entry;
  sexp_context_trampoline(ctx) = &tramp;

  sexp res = SEXP_VOID;
  sexp fixed_args[FIXED_ARGS_SIZE];
  sexp *call_args = args;

  if (entry) {
    tramp.abort_saves = sexp_context_saves(ctx);
    tramp.abort_proc = sexp_context_proc(ctx);
    if (native_setjmp(tramp.abort_jb)) {
      res = tramp.abort_val;
      goto done;
    }
  }

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
        if (nargs + 2 > tramp.call_dyn_capacity) {
          tramp.call_dyn_capacity = (nargs + 2) * 2;
          tramp.call_dyn_args = (sexp*)realloc(tramp.call_dyn_args, sizeof(sexp) * tramp.call_dyn_capacity);
        }
        call_args = tramp.call_dyn_args;
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
          struct sexp_gc_var_t *saves_arr = tramp.saves;
          if (nargs > FIXED_ARGS_SIZE) {
            if ((size_t)nargs > tramp.dyn_capacity) {
              tramp.dyn_capacity = (size_t)(nargs + 2) * 2;
              tramp.dyn_saves = (struct sexp_gc_var_t*)realloc(tramp.dyn_saves, sizeof(struct sexp_gc_var_t) * tramp.dyn_capacity);
            }
            saves_arr = tramp.dyn_saves;
          }
          for (sexp_sint_t k = 0; k < nargs; k++) {
            saves_arr[k].var = &call_args[k];
            saves_arr[k].next = (k == 0) ? saved_saves : &saves_arr[k - 1];
          }
          sexp_context_saves(ctx) = &saves_arr[nargs - 1];
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
        if (nargs + 2 > tramp.call_dyn_capacity) {
          tramp.call_dyn_capacity = (nargs + 2) * 2;
          tramp.call_dyn_args = (sexp*)realloc(tramp.call_dyn_args, sizeof(sexp) * tramp.call_dyn_capacity);
        }
        args = tramp.call_dyn_args;
      }
      sexp ls = next_args;
      for (sexp_sint_t k = 0; k < nargs; k++, ls = sexp_cdr(ls))
        args[k] = sexp_car(ls);
      continue;
    }
    break;
  }

 done:
  sexp_native_free_trampoline(&tramp);
  sexp_context_trampoline(ctx) = tramp.prev;
  sexp_gc_release1(ctx);
  return res;
}

/* calls from native code */
sexp sexp_native_call (sexp ctx, sexp callee, sexp_sint_t nargs, sexp *args) {
  return sexp_native_call_aux(ctx, callee, nargs, args, 0);
}

/* calls from C, which an uncaught raise returns to */
static sexp sexp_native_call_entry (sexp ctx, sexp callee, sexp_sint_t nargs, sexp *args) {
  return sexp_native_call_aux(ctx, callee, nargs, args, 1);
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

static void *sexp_native_get_thread_stack_top (void) {
  pthread_attr_t attr;
  void *addr = NULL;
  size_t sz = 0;
  if (pthread_getattr_np(pthread_self(), &attr) == 0) {
    pthread_attr_getstack(&attr, &addr, &sz);
    pthread_attr_destroy(&attr);
    return (char*)addr + sz;
  }
  return NULL;
}

static void *sexp_native_get_context_stack_base (sexp ctx) {
#if SEXP_USE_GREEN_THREADS
  if (sexp_context_fiber_stack_base(ctx)) {
    return (char*)sexp_context_fiber_stack_base(ctx) + sexp_context_fiber_stack_size(ctx);
  }
#endif
  if (sexp_context_stack_base(ctx)) {
    return sexp_context_stack_base(ctx);
  }
  void *top = sexp_native_get_thread_stack_top();
  if (top) {
    sexp_context_stack_base(ctx) = top;
    return top;
  }
  uintptr_t approx_stack = (uintptr_t)&ctx;
  __asm__ volatile ("" : "+r"(approx_stack));
  return (void*)(approx_stack + 65536);
}

static sexp_uint_t sexp_native_cont_type_tag = 0;

sexp sexp_native_cont_finalize (sexp ctx, sexp self, sexp_sint_t n, sexp cptr) {
  if (sexp_pointerp(cptr) && (sexp_cpointerp(cptr) || sexp_pointer_tag(cptr) >= SEXP_CPOINTER)) {
    struct sexp_native_cont *cont = (struct sexp_native_cont*)sexp_cpointer_value(cptr);
    if (cont) {
      if (cont->saved_stack) {
        free(cont->saved_stack);
        cont->saved_stack = NULL;
      }
      free(cont);
      sexp_cpointer_value(cptr) = NULL;
    }
  }
  return SEXP_VOID;
}

__attribute__((noinline)) static void sexp_restore_stack_and_jump_stage2 (struct sexp_native_cont *cont) {
  memcpy(cont->sp, cont->saved_stack, cont->stack_size);
  sexp_context_saves(cont->ctx) = cont->saves;
  sexp_context_trampoline(cont->ctx) = cont->trampoline;
  sexp_context_child(cont->ctx) = cont->child;
  sexp_context_proc(cont->ctx) = cont->proc;
  sexp_context_last_fp(cont->ctx) = cont->last_fp;
  native_longjmp(cont->jb, 1);
}

__attribute__((noinline)) static void sexp_restore_stack_and_jump (struct sexp_native_cont *cont) {
  volatile char dummy;
  void *current_sp = (void*)&dummy;
  ptrdiff_t diff = (char*)current_sp - (char*)cont->sp;
  if (diff >= -1024) {
    size_t alloc_size = (diff > 0 ? (size_t)diff : 0) + 2048;
    void *space = alloca(alloc_size);
    __asm__ volatile ("" : : "r"(space) : "memory");
    sexp_restore_stack_and_jump_stage2(cont);
    return;
  }
  sexp_restore_stack_and_jump_stage2(cont);
}

sexp sexp_native_cont_call (sexp ctx, sexp self, sexp_sint_t n, sexp val) {
  sexp data = sexp_opcode_data(self);
  struct sexp_native_cont *cont;
  if (!sexp_pointerp(data) || (!sexp_cpointerp(data) && sexp_pointer_tag(data) < SEXP_CPOINTER))
    return sexp_user_exception(ctx, self, "invalid continuation pointer", data);
  cont = (struct sexp_native_cont*)sexp_cpointer_value(data);
  if (!cont || !cont->saved_stack) {
    return sexp_user_exception(ctx, self, "continuation is not callable", val);
  }
  cont->val = val;
  /* Clean up any child contexts spawned below the target stack frame */
  for (sexp c = sexp_context_child(cont->ctx); c && sexp_contextp(c) && c != cont->child; ) {
    sexp next = sexp_context_child(c);
    sexp_context_saves(c) = NULL;
    sexp_context_child(c) = NULL;
    c = next;
  }
  for (sexp c = ctx; c && sexp_contextp(c) && c != cont->ctx && c != cont->child; ) {
    sexp parent = sexp_context_parent(c);
    sexp_context_saves(c) = NULL;
    sexp_context_child(c) = NULL;
    c = parent;
  }
  sexp_restore_stack_and_jump(cont);
  return SEXP_VOID;
}

__attribute__((noinline)) static sexp sexp_native_callcc_helper (sexp ctx, sexp proc) {
  volatile sexp vctx = ctx;
  struct sexp_native_cont * volatile vcont = (struct sexp_native_cont*)malloc(sizeof(struct sexp_native_cont));
  if (!vcont)
    return sexp_user_exception(ctx, proc, "out of memory allocating continuation", SEXP_FALSE);
  sexp_gc_var2(cptr, k);
  sexp_gc_preserve2(ctx, cptr, k);

  if (sexp_native_cont_type_tag == 0 || sexp_native_cont_type_tag >= sexp_context_num_types(ctx)) {
    sexp_uint_t i, num_types = sexp_context_num_types(ctx);
    sexp_native_cont_type_tag = 0;
    for (i = 0; i < num_types; i++) {
      sexp t = sexp_type_by_index(ctx, i);
      if (t && sexp_typep(t)) {
        sexp tname = sexp_type_name(t);
        if (tname && sexp_stringp(tname) && strcmp(sexp_string_data(tname), "native_cont") == 0) {
          sexp_native_cont_type_tag = i;
          break;
        }
      }
    }
    if (sexp_native_cont_type_tag == 0) {
      sexp name = sexp_c_string(ctx, "native_cont", -1);
      sexp type = sexp_register_c_type(ctx, name, sexp_native_cont_finalize);
      if (sexp_typep(type)) {
        sexp_native_cont_type_tag = sexp_type_tag(type);
      }
    }
  }

  vcont->ctx = ctx;
  vcont->saves = sexp_context_saves(ctx);
  vcont->trampoline = sexp_context_trampoline(ctx);
  vcont->child = sexp_context_child(ctx);
  vcont->proc = sexp_context_proc(ctx);
  vcont->last_fp = sexp_context_last_fp(ctx);
  vcont->val = SEXP_VOID;
  vcont->sp = NULL;
  vcont->stack_base = NULL;
  vcont->stack_size = 0;
  vcont->saved_stack = NULL;

  cptr = sexp_make_cpointer(ctx, sexp_native_cont_type_tag ? sexp_native_cont_type_tag : SEXP_CPOINTER,
                            (void*)vcont, SEXP_FALSE, 1);
  k = sexp_make_foreign(ctx, "continuation", 1, 0, NULL, (sexp_proc1)sexp_native_cont_call, cptr);
  sexp_opcode_data2(k) = sexp_c_string(ctx, "sexp_native_cont_call", -1);

  sexp res;
  if (native_setjmp(vcont->jb) == 0) {
    vcont->sp = (void*)vcont->jb[1];
    vcont->stack_base = sexp_native_get_context_stack_base(ctx);
    if ((uintptr_t)vcont->stack_base > (uintptr_t)vcont->sp) {
      vcont->stack_size = (uintptr_t)vcont->stack_base - (uintptr_t)vcont->sp;
      vcont->saved_stack = (char*)malloc(vcont->stack_size);
      if (vcont->saved_stack) {
        memcpy(vcont->saved_stack, vcont->sp, vcont->stack_size);
      }
    }
    res = sexp_native_call1(vctx, proc, k);
  } else {
    res = vcont->val;
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
  sexp_context_stack_base(thread_ctx) = (char*)stack_base + size;
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
    sexp res = sexp_slot_ref(instance, sexp_unbox_fixnum(slot));
    return res ? res : SEXP_VOID;
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

/* a - b for fixnums whose difference overflows: sexp_sub assumes */
/* its caller handles that, as the VM does */
static sexp sexp_native_fixnum_sub (sexp ctx, sexp a, sexp b) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = sexp_fixnum_to_bignum(ctx, a);
  if (!sexp_exceptionp(res))
    res = sexp_bignum_normalize(sexp_sub(ctx, res, b));
  sexp_gc_release1(ctx);
  return res;
}

/* chained < (strict) or <=, or > and >= when inverse */
static sexp sexp_native_num_cmp (sexp ctx, sexp_sint_t nargs, sexp *args,
                                 int inverse, int strict) {
  for (sexp_sint_t i = 0; i < nargs - 1; i++) {
    sexp_sint_t c;
    if (sexp_fixnump(args[i]) && sexp_fixnump(args[i+1])) {
      c = ((sexp_sint_t)args[i] > (sexp_sint_t)args[i+1])
        - ((sexp_sint_t)args[i] < (sexp_sint_t)args[i+1]);
    } else {
      sexp cmp = sexp_compare(ctx, args[i], args[i+1]);
      if (sexp_exceptionp(cmp)) {
        if (sexp_stringp(sexp_exception_message(cmp)) &&
            strcmp("can't compare NaN", sexp_string_data(sexp_exception_message(cmp))) == 0)
          return SEXP_FALSE;
        return cmp;
      }
      c = sexp_unbox_fixnum(cmp);
    }
    if (inverse) c = -c;
    if (strict ? c >= 0 : c > 0) return SEXP_FALSE;
  }
  return SEXP_TRUE;
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
      return sexp_native_fixnum_sub(ctx, args[0], args[1]);
    }
    if (nargs == 0) return SEXP_ZERO;
    if (nargs == 1)
      return sexp_fixnump(args[0]) ? sexp_native_fixnum_sub(ctx, SEXP_ZERO, args[0])
        : sexp_sub(ctx, SEXP_ZERO, args[0]);
    {
      sexp res = args[0];
      for (sexp_sint_t i = 1; i < nargs; i++) {
        if (sexp_fixnump(res) && sexp_fixnump(args[i])) {
          sexp_sint_t a = sexp_unbox_fixnum(res), b = sexp_unbox_fixnum(args[i]), c = a - b;
          if (c >= SEXP_MIN_FIXNUM && c <= SEXP_MAX_FIXNUM) {
            res = sexp_make_fixnum(c);
          } else {
            res = sexp_native_fixnum_sub(ctx, res, args[i]);
          }
          continue;
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
    return sexp_native_num_cmp(ctx, nargs, args, sexp_opcode_inverse(op), 1);
  case SEXP_OP_LE:
    return sexp_native_num_cmp(ctx, nargs, args, sexp_opcode_inverse(op), 0);
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
    if (!sexp_fixnump(args[0])) return sexp_native_raise_exception(ctx, op, sexp_type_exception(ctx, self, SEXP_FIXNUM, args[0]));
    if (sexp_unbox_fixnum(args[0]) < 0) return sexp_native_raise(ctx, op, "make-vector: length must be non-negative", sexp_list1(ctx, args[0]));
    {
      sexp res = sexp_make_vector(ctx, args[0], nargs > 1 ? args[1] : SEXP_VOID);
      if (sexp_exceptionp(res)) return sexp_native_raise_exception(ctx, op, res);
      return res;
    }
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
    return sexp_native_apply_list(ctx, args[0], args[1], 0);
  case SEXP_OP_CALLCC:
    return sexp_native_callcc_helper(ctx, args[0]);
  case SEXP_OP_RAISE:
    return sexp_native_raise_exception(ctx, self, args[0]);
  case SEXP_OP_PARAMETER_REF:
    if (nargs == 0) return sexp_parameter_ref(ctx, op);
    return sexp_native_parameter_set(ctx, op, args[0]);
  case SEXP_OP_MAKE: {
    sexp_sint_t size = sexp_unbox_fixnum(sexp_opcode_data2(op));
    sexp res = sexp_alloc_tagged(ctx, size, sexp_unbox_fixnum(sexp_opcode_data(op)));
    if (!sexp_exceptionp(res)) {
      for (sexp_sint_t i = (size - sexp_sizeof_header) / sizeof(sexp_uint_t) - 1; i >= 0; i--)
        sexp_slot_set(res, i, SEXP_VOID);
    }
    return res;
  }
  case SEXP_OP_SLOT_REF: {
    sexp_uint_t tid = sexp_unbox_fixnum(sexp_opcode_data(op));
    sexp type = sexp_type_by_index(ctx, tid);
    if (!sexp_check_type(ctx, args[0], type))
      return sexp_native_raise(ctx, op, "slot-ref: bad type",
                               sexp_list2(ctx, sexp_type_name_by_index(ctx, tid), args[0]));
    sexp res = sexp_slot_ref(args[0], sexp_unbox_fixnum(sexp_opcode_data2(op)));
    return res ? res : SEXP_VOID;
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
  if (!res) res = SEXP_VOID;
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

static int get_context_num_slots (sexp ctx) {
  sexp lam = sexp_context_lambda(ctx);
  int num_params = (lam && sexp_lambdap(lam)) ? get_param_count(lam) : 0;
  int num_locals = (lam && sexp_lambdap(lam)) ? get_local_count(lam) : 0;
  int max_depth = 0;
  if (sexp_vectorp(sexp_context_specific(ctx))) {
    sexp bc = sexp_context_bc(ctx);
    if (bc && sexp_bytecodep(bc)) {
      max_depth = (int)sexp_bytecode_max_depth(bc);
    }
    if (max_depth <= 0 && sexp_fixnump(sexp_context_max_depth(ctx))) {
      max_depth = (int)sexp_unbox_fixnum(sexp_context_max_depth(ctx));
    }
  }
  if (max_depth <= 0) {
    max_depth = 120;
    if (sexp_vectorp(sexp_context_specific(ctx))) {
      sexp bc = sexp_context_bc(ctx);
      if (bc && sexp_bytecodep(bc))
        sexp_bytecode_max_depth(bc) = max_depth;
      sexp_context_max_depth(ctx) = sexp_make_fixnum(max_depth);
    }
  }
  return num_params + num_locals + max_depth + 4;
}

static int32_t get_slot_disp (sexp ctx, int slot_index) {
  int num_slots = get_context_num_slots(ctx);
  if (slot_index >= num_slots) {
    fprintf(stderr, "WARNING: slot_index %d >= num_slots %d!\n", slot_index, num_slots);
  }
  return compute_slot_base(num_slots) + 8 * slot_index;
}

static void emit_mov_slot_rax (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(ctx, slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x85);
  emit_u32(ctx, (uint32_t)disp);
}

static void emit_mov_rax_slot (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(ctx, slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x85);
  emit_u32(ctx, (uint32_t)disp);
}

static void emit_clear_slot (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(ctx, slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xc7); emit_u8(ctx, 0x85);
  emit_u32(ctx, (uint32_t)disp);
  emit_u32(ctx, (uint32_t)(uintptr_t)SEXP_UNDEF);
}

static void emit_clear_slots (sexp ctx, int start_slot, int count) {
  for (int i = 0; i < count; i++) {
    emit_clear_slot(ctx, start_slot + i);
  }
}

static int g_non_allocating_bb = 0;
/* code offset of the current lambda's loop start for self tail calls; */
/* this must not live in sexp_context_top, which is the GC'd extent */
/* of the (possibly shared and live) context stack */
static sexp_sint_t g_loop_start = 0;

/* movabsq $fn, %r11; callq *%r11 */
static void emit_call_abs (sexp ctx, void *fn) {
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xbb);
  emit_u64(ctx, (uint64_t)fn);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0xff); emit_u8(ctx, 0xd3);
}

static void emit_mov_imm64_rax (sexp ctx, uint64_t val) {
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xb8);
  emit_u64(ctx, val);
}

static void emit_mov_slot_rcx (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(ctx, slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x8d);
  emit_u32(ctx, (uint32_t)disp);
}

static void emit_mov_slot_rdx (sexp ctx, int slot_index) {
  int32_t disp = get_slot_disp(ctx, slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x95);
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

static void emit_inlined_op_cleanup (sexp ctx, sexp_sint_t done_jmp, int temp_start, int count) {
  if (g_non_allocating_bb) {
    emit_clear_slots(ctx, temp_start, count);
  }
  patch_jmp(ctx, done_jmp);
  if (!g_non_allocating_bb) {
    emit_clear_slots(ctx, temp_start, count);
  }
  sexp_inc_context_depth(ctx, -count);
}

/* Finish an inlined opcode: jump over the slow path, where each failed */
/* check (the jcc positions after nargs, -1 for none) lands to call the */
/* generic opcode on the args in the temp slots.  Returns 1 (handled). */
#define emit_inlined_op_slow_path(ctx, op, temp_start, nargs, ...)        \
  emit_inlined_op_slow_path_aux(ctx, op, temp_start, nargs,              \
    (const sexp_sint_t[]){__VA_ARGS__},                                  \
    sizeof((const sexp_sint_t[]){__VA_ARGS__}) / sizeof(sexp_sint_t))

static void emit_native_dispatch_op (sexp ctx, sexp op, int temp_start, int nargs);

static int emit_inlined_op_slow_path_aux (sexp ctx, sexp op, int temp_start, int nargs,
                                          const sexp_sint_t *slow, int num_slow) {
  int i;
  for (i = 0; i < num_slow && slow[i] == -1; i++)
    ;
  if (i == num_slow) {
    /* every check was elided: no slow path, as on the fast path */
    if (!g_non_allocating_bb)
      emit_clear_slots(ctx, temp_start, nargs);
    sexp_inc_context_depth(ctx, -nargs);
    return 1;
  }
  sexp_sint_t done_jmp = emit_jmp(ctx);
  for (i = 0; i < num_slow; i++)
    if (slow[i] != -1) patch_jcc(ctx, slow[i]);
  emit_native_dispatch_op(ctx, op, temp_start, nargs);
  emit_inlined_op_cleanup(ctx, done_jmp, temp_start, nargs);
  return 1;
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
  int32_t disp = get_slot_disp(ctx, slot_index);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x95); // mov disp(%rbp), %rdx
  emit_u32(ctx, (uint32_t)disp);
  emit_call_abs(ctx, (void*)native_cons);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x85); // mov %rax, disp(%rbp)
  emit_u32(ctx, (uint32_t)disp);
}

static void emit_native_call (sexp ctx, int temp_start, int nargs) {
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc6);
  emit_u8(ctx, 0xba);
  emit_u32(ctx, (uint32_t)nargs);
  int32_t disp = get_slot_disp(ctx, temp_start);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x8d);
  emit_u32(ctx, (uint32_t)disp);
  emit_call_abs(ctx, (void*)sexp_native_call);
}

static void emit_native_tail_call (sexp ctx, int temp_start, int nargs) {
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc6);
  emit_u8(ctx, 0xba);
  emit_u32(ctx, (uint32_t)nargs);
  int32_t disp = get_slot_disp(ctx, temp_start);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x8d);
  emit_u32(ctx, (uint32_t)disp);
  emit_call_abs(ctx, (void*)sexp_native_make_trampoline);
}

static void emit_get_param_value (sexp ctx, sexp param_op, int slot) {
  bytecode_preserve(ctx, param_op);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xbe);
  emit_u64(ctx, (uint64_t)param_op);
  emit_call_abs(ctx, (void*)sexp_parameter_ref);
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
      val = sexp_native_call1(ctx, conv_save, val_save);
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
  emit_call_abs(ctx, (void*)sexp_native_parameter_set);
}

static void emit_native_dispatch_op (sexp ctx, sexp op, int temp_start, int nargs) {
  bytecode_preserve(ctx, op);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xee);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0xba);
  emit_u64(ctx, (uint64_t)op);
  emit_u8(ctx, 0xb9);
  emit_u32(ctx, (uint32_t)nargs);
  int32_t disp = get_slot_disp(ctx, temp_start);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x85);
  emit_u32(ctx, (uint32_t)disp);
  emit_call_abs(ctx, (void*)sexp_native_dispatch_op);
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
  int32_t disp = get_slot_disp(ctx, temp_start);
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x8d);
  emit_u32(ctx, (uint32_t)disp);
  emit_call_abs(ctx, (void*)sexp_native_make_closure);
}

enum { REG_RAX = 0, REG_RCX = 1 };

/* jump to slow paths (returned in *jnz and *jz) unless reg is a */
/* non-null pointer: testb $3, %al/%cl; jnz; testq reg, reg; jz */
static void emit_check_pointer (sexp ctx, int reg, sexp_sint_t *jnz, sexp_sint_t *jz) {
  if (reg == REG_RAX) {
    emit_u8(ctx, 0xa8); emit_u8(ctx, 0x03);
  } else {
    emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc0 | reg); emit_u8(ctx, 0x03);
  }
  *jnz = emit_jcc(ctx, 0x85);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x85); emit_u8(ctx, 0xc0 | (reg << 3) | reg);
  *jz = emit_jcc(ctx, 0x84);
}

/* cmpl $tag, (reg); returns the jne to the slow path */
static sexp_sint_t emit_check_tag (sexp ctx, int reg, int tag) {
  emit_u8(ctx, 0x83); emit_u8(ctx, 0x38 | reg); emit_u8(ctx, (uint8_t)tag);
  return emit_jcc(ctx, 0x85);
}

/* jump to slow paths unless reg holds a pointer to an object with the */
/* type tag, without any check when that's the inferred type of x */
static void emit_check_type (sexp ctx, sexp x, int reg, int tag, sexp_sint_t *jnz,
                             sexp_sint_t *jz, sexp_sint_t *jne) {
  if (sexp_inferred_type(ctx, x) == tag) {
    *jnz = *jz = *jne = -1;
    return;
  }
  emit_check_pointer(ctx, reg, jnz, jz);
  *jne = emit_check_tag(ctx, reg, tag);
}

/* cmpq $imm, %rax */
static void emit_cmp_rax_imm (sexp ctx, int32_t imm) {
  if (imm >= -128 && imm <= 127) {
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xf8);
    emit_u8(ctx, (uint8_t)(int8_t)imm);
  } else {
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x3d);
    emit_u32(ctx, (uint32_t)imm);
  }
}

/* jcc opcode taken when a fused fixnum comparison of a against b */
/* (flags from a - b) fails; inverse turns < and <= into > and >= */
static uint8_t cmp_fail_cc (sexp_sint_t code, int inverse) {
  if (code == SEXP_OP_LT)
    return inverse ? 0x8e /* jle */ : 0x8d /* jge */;
  if (code == SEXP_OP_LE)
    return inverse ? 0x8c /* jl */ : 0x8f /* jg */;
  return 0x85; /* jne */
}

/* Check the params inferred to have types demanded on entry, in the */
/* order the body would have checked them.  On failure raise the same */
/* error, then return, since the body relies on the types. */
static void emit_param_type_checks (sexp ctx, sexp lam, int num_params) {
  int k, order, o, tag;
  sexp_sint_t jnz, jz, jne, ok_jmp;
  for (o = 0; o < num_params; o++) {
    for (k = 0; k < num_params; k++)
      if ((tag = sexp_param_checked_type(lam, k, &order)) >= 0 && order == o)
        break;
    if (k == num_params) break;
    emit_mov_slot_rax(ctx, get_local_slot(lam, k));
    emit_check_pointer(ctx, REG_RAX, &jnz, &jz);
    jne = emit_check_tag(ctx, REG_RAX, tag);
    ok_jmp = emit_jmp(ctx);
    patch_jcc(ctx, jnz);
    patch_jcc(ctx, jz);
    patch_jcc(ctx, jne);
    emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7); // mov %r12, %rdi
    emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xee); // mov %r13, %rsi
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc2); // mov %rax, %rdx
    emit_u8(ctx, 0xb9); emit_u32(ctx, (uint32_t)tag);           // mov $tag, %ecx
    emit_call_abs(ctx, (void*)sexp_native_param_type_error);
    sexp_emit_return(ctx);
    patch_jmp(ctx, ok_jmp);
  }
}

/************************* Prologue & Epilogue ************************/

void sexp_emit_enter (sexp ctx) {
  sexp lam = sexp_context_lambda(ctx);
  int num_params = (lam && sexp_lambdap(lam)) ? get_param_count(lam) : 0;
  int is_leaf = (lam && sexp_lambdap(lam) && sexp_lambda_leaf_p(lam));
  int num_slots = get_context_num_slots(ctx);
  uint32_t frame_alloc = compute_frame_alloc(num_slots);

  emit_u8(ctx, 0x55);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe5);
  emit_u8(ctx, 0x53);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x54);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x55);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x56);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x57);
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x81); emit_u8(ctx, 0xec);
  emit_u32(ctx, frame_alloc);

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
    emit_u32(ctx, (uint32_t)(((num_slots) << 16) | (num_params & 0xFFFF)));

    emit_call_abs(ctx, (void*)sexp_native_enter);
  }
}

void sexp_emit_return (sexp ctx) {
  sexp lam = sexp_context_lambda(ctx);
  int is_leaf = (lam && sexp_lambdap(lam) && sexp_lambda_leaf_p(lam));

  if (!is_leaf) {
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc3);

    emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7);
    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe6);
    emit_call_abs(ctx, (void*)sexp_native_exit);

    emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xd8);
  }

  // Restore %rsp to just below saved registers: %rsp = %rbp - 40
  emit_u8(ctx, 0x48); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x65); emit_u8(ctx, 0xd8); // lea -40(%rbp), %rsp

  emit_u8(ctx, 0x41); emit_u8(ctx, 0x5f);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x5e);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x5d);
  emit_u8(ctx, 0x41); emit_u8(ctx, 0x5c);
  emit_u8(ctx, 0x5b);
  emit_u8(ctx, 0x5d);
  emit_u8(ctx, 0xc3);
}

void sexp_bless_bytecode (sexp ctx, sexp bc) {
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
        emit_call_abs(ctx, (void*)sexp_native_delayed_ref);

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
    generate_lambda(ctx, sexp_ref_name(ref), sexp_ref_loc(ref), sexp_set_value(set), sexp_set_value(set));
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

  if (!g_non_allocating_bb) {
    emit_clear_slot(ctx, temp_slot);
  }
  emit_mov_imm64_rax(ctx, (uint64_t)SEXP_VOID);
}

/* generate x into a temp slot, which the caller must clear */
static void generate_to_temp (sexp ctx, sexp x, int slot) {
  generate(ctx, 0, 0, 0, x);
  emit_mov_rax_slot(ctx, slot);
  sexp_inc_context_depth(ctx, 1);
}

/* generate a conditional branch, which can be marked non-allocating */
/* even when the enclosing code isn't */
static void generate_branch (sexp ctx, sexp name, sexp loc, sexp lam, sexp x) {
  int saved_non_alloc = g_non_allocating_bb;
  g_non_allocating_bb = saved_non_alloc || !sexp_expr_may_allocp(ctx, lam, x, name, loc);
  generate(ctx, name, loc, lam, x);
  g_non_allocating_bb = saved_non_alloc;
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
      generate_to_temp(ctx, arg0, temp_start);
      generate(ctx, 0, 0, 0, arg1);
      emit_mov_slot_rcx(ctx, temp_start);
      if (!g_non_allocating_bb) {
        emit_clear_slot(ctx, temp_start);
      }
      sexp_inc_context_depth(ctx, -1);
      // cmpq %rax, %rcx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x39); emit_u8(ctx, 0xc1);
      // Fail if not equal: jne .Lfail
      fail_jcc = emit_jcc(ctx, 0x85);
    } else if (code == SEXP_OP_LT || code == SEXP_OP_LE || code == SEXP_OP_EQN) {
      const_arg1 = sexp_fixnum_literalp(arg1, &c) && (c >= -1073741824LL && c <= 1073741823LL);
      const_arg0 = !const_arg1 && sexp_fixnum_literalp(arg0, &c) && (c >= -1073741824LL && c <= 1073741823LL);

      if (const_arg1 || const_arg0) {
        int k = const_arg1 ? 0 : 1;
        sexp var_arg = const_arg1 ? arg0 : arg1;
        has_slow = !sexp_proven_fixnump(ctx, lam, var_arg);
        generate(ctx, 0, 0, 0, var_arg);
        if (has_slow) {
          emit_mov_rax_slot(ctx, temp_start + k);
          // reserve both slots for the slow path's args
          sexp_inc_context_depth(ctx, 2);
          sexp_inc_context_depth(ctx, -2);
          // testb $1, %al
          emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
          jz_slow = emit_jcc(ctx, 0x84);
        }
        emit_cmp_rax_imm(ctx, (int32_t)(sexp_sint_t)sexp_make_fixnum(c));
        // with a constant arg0 this computes arg1 - arg0, swapping the sense
        fail_jcc = emit_jcc(ctx, cmp_fail_cc(code, !!sexp_opcode_inverse(op) ^ const_arg0));
      } else {
        generate_to_temp(ctx, arg0, temp_start);
        generate_to_temp(ctx, arg1, temp_start + 1);
        emit_mov_slot_rcx(ctx, temp_start);

        has_slow = !sexp_proven_fixnump(ctx, lam, arg0) || !sexp_proven_fixnump(ctx, lam, arg1);
        if (has_slow) {
          // movq %rcx, %rdx
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xca);
          // andq %rax, %rdx
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x21); emit_u8(ctx, 0xc2);
          // testb $1, %dl
          emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc2); emit_u8(ctx, 0x01);
          jz_slow = emit_jcc(ctx, 0x84);
        }

        // cmpq %rax, %rcx (computes arg0 - arg1)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x39); emit_u8(ctx, 0xc1);
        fail_jcc = emit_jcc(ctx, cmp_fail_cc(code, !!sexp_opcode_inverse(op)));
        sexp_inc_context_depth(ctx, -2);
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
  generate_branch(ctx, name, loc, lam, pass);

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
    emit_cmp_rax_imm(ctx, (int32_t)(sexp_sint_t)SEXP_FALSE);
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
  generate_branch(ctx, name, loc, lam, fail);

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
  generate_branch(ctx, name, loc, lam, test);

  // cmpq $SEXP_FALSE, %rax
  emit_cmp_rax_imm(ctx, (int32_t)(sexp_sint_t)SEXP_FALSE);

  // je .Lfail: 0f 84 <disp32>
  sexp_sint_t je_pos = emit_jcc(ctx, 0x84);

  sexp_context_tailp(ctx) = tailp;
  generate_branch(ctx, name, loc, lam, pass);

  // jmp .Lend: e9 <disp32>
  sexp_sint_t jmp_pos = emit_jmp(ctx);

  // .Lfail:
  patch_jcc(ctx, je_pos);

  sexp_context_tailp(ctx) = tailp;
  generate_branch(ctx, name, loc, lam, fail);

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
  sexp lam = sexp_context_lambda(ctx);

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
      generate_to_temp(ctx, arg0, temp_start);

      sexp_sint_t jz_slow = -1;
      if (!sexp_proven_fixnump(ctx, lam, arg0)) {
        // testb $1, %al
        emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
        jz_slow = emit_jcc(ctx, 0x84);
      }

      // mov $2, %rdx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xc7); emit_u8(ctx, 0xc2); emit_u32(ctx, 2);
      // subq %rax, %rdx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x29); emit_u8(ctx, 0xc2);
      sexp_sint_t jo_slow = sexp_proven_fixnump(ctx, lam, app) ? -1 : emit_jcc(ctx, 0x80);
      // movq %rdx, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xd0);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 1, jz_slow, jo_slow);
    }

    if (code == SEXP_OP_CAR || code == SEXP_OP_CDR) {
      generate_to_temp(ctx, arg0, temp_start);

      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RAX, SEXP_PAIR, &jnz_slow, &jz_slow, &jne_slow);

      // Fast load
      int32_t off = (code == SEXP_OP_CAR)
        ? (int32_t)offsetof(struct sexp_struct, value.pair.car)
        : (int32_t)offsetof(struct sexp_struct, value.pair.cdr);
      emit_mov_mem_rax(ctx, off);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 1, jnz_slow, jz_slow, jne_slow);
    }

    if (code == SEXP_OP_VECTOR_LENGTH) {
      generate_to_temp(ctx, arg0, temp_start);

      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RAX, SEXP_VECTOR, &jnz_slow, &jz_slow, &jne_slow);

      // Load length at value.vector.length (offset 8)
      emit_mov_mem_rax(ctx, (int32_t)offsetof(struct sexp_struct, value.vector.length));
      // Tag as fixnum: addq %rax, %rax; incq %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x01); emit_u8(ctx, 0xc0);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xff); emit_u8(ctx, 0xc0);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 1, jnz_slow, jz_slow, jne_slow);
    }

    if (code == SEXP_OP_BYTES_LENGTH) {
      generate_to_temp(ctx, arg0, temp_start);

      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RAX, SEXP_BYTES, &jnz_slow, &jz_slow, &jne_slow);

      // Load length at value.bytes.length (offset 8)
      emit_mov_mem_rax(ctx, (int32_t)offsetof(struct sexp_struct, value.bytes.length));
      // Tag as fixnum: addq %rax, %rax; incq %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x01); emit_u8(ctx, 0xc0);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xff); emit_u8(ctx, 0xc0);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 1, jnz_slow, jz_slow, jne_slow);
    }

#if SEXP_USE_UTF8_STRINGS
    if (code == SEXP_OP_STRING_CURSOR_END) {
      generate_to_temp(ctx, arg0, temp_start);

      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RAX, SEXP_STRING, &jnz_slow, &jz_slow, &jne_slow);

      // Load byte size at value.string.length (offset 24)
      emit_mov_mem_rax(ctx, (int32_t)offsetof(struct sexp_struct, value.string.length));
      // Make string cursor: shlq $3, %rax; addq $2, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xc1); emit_u8(ctx, 0xe0); emit_u8(ctx, 3);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xc0); emit_u8(ctx, 2);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 1, jnz_slow, jz_slow, jne_slow);
    }
#else
    if (code == SEXP_OP_STRING_LENGTH) {
      generate_to_temp(ctx, arg0, temp_start);

      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RAX, SEXP_STRING, &jnz_slow, &jz_slow, &jne_slow);

      emit_mov_mem_rax(ctx, (int32_t)offsetof(struct sexp_struct, value.string.length));
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x01); emit_u8(ctx, 0xc0);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xff); emit_u8(ctx, 0xc0);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 1, jnz_slow, jz_slow, jne_slow);
    }
#endif

    if (code == SEXP_OP_SLOT_REF) {
      sexp tid_sexp = sexp_opcode_data(op);
      sexp_sint_t tid = sexp_typep(tid_sexp) ? sexp_type_tag(tid_sexp) :
                        sexp_fixnump(tid_sexp) ? sexp_unbox_fixnum(tid_sexp) : -1;
      sexp idx_sexp = sexp_opcode_data2(op);
      sexp_sint_t idx = sexp_fixnump(idx_sexp) ? sexp_unbox_fixnum(idx_sexp) : -1;

      if (tid >= 0 && idx >= 0) {
        generate_to_temp(ctx, arg0, temp_start);

        sexp_sint_t jnz_slow, jz_slow;
        emit_check_pointer(ctx, REG_RAX, &jnz_slow, &jz_slow);

        // cmpl $tid, (%rax)
        emit_u8(ctx, 0x81); emit_u8(ctx, 0x38); emit_u32(ctx, (uint32_t)tid);
        sexp_sint_t jne_slow = emit_jcc(ctx, 0x85);

        // Load field at 8 + idx * 8
        int32_t off = (int32_t)(offsetof(struct sexp_struct, value) + idx * sizeof(sexp));
        emit_mov_mem_rax(ctx, off);

        // If field was 0/NULL, replace with SEXP_VOID
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x85); emit_u8(ctx, 0xc0);
        emit_mov_imm64_rdx(ctx, (uint64_t)SEXP_VOID);
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0x44); emit_u8(ctx, 0xc2);

        return emit_inlined_op_slow_path(ctx, op, temp_start, 1, jnz_slow, jz_slow, jne_slow);
      }
    }

    if (code == SEXP_OP_TYPEP) {
      sexp tid_sexp = sexp_opcode_data(op);
      sexp_sint_t tid = sexp_typep(tid_sexp) ? sexp_type_tag(tid_sexp) :
                        sexp_fixnump(tid_sexp) ? sexp_unbox_fixnum(tid_sexp) : -1;
      if (tid >= 0) {
        generate_to_temp(ctx, arg0, temp_start);

        sexp_sint_t jnz_false, jz_false;
        emit_check_pointer(ctx, REG_RAX, &jnz_false, &jz_false);

        // cmpl $tid, (%rax)
        emit_u8(ctx, 0x81); emit_u8(ctx, 0x38); emit_u32(ctx, (uint32_t)tid);
        sexp_sint_t je_true = emit_jcc(ctx, 0x84);

        // If tag didn't match, fall back to slow path (sexp_check_type)
        sexp_sint_t jmp_slow = emit_jmp(ctx);

        // False path:
        patch_jcc(ctx, jnz_false);
        patch_jcc(ctx, jz_false);
        emit_mov_imm64_rax(ctx, (uint64_t)SEXP_FALSE);
        sexp_sint_t done_jmp = emit_jmp(ctx);

        // True path:
        patch_jcc(ctx, je_true);
        emit_mov_imm64_rax(ctx, (uint64_t)SEXP_TRUE);
        sexp_sint_t done_jmp2 = emit_jmp(ctx);

        // Slow path:
        patch_jmp(ctx, jmp_slow);
        emit_native_dispatch_op(ctx, op, temp_start, 1);
        if (g_non_allocating_bb) {
          emit_clear_slots(ctx, temp_start, 1);
        }
        patch_jmp(ctx, done_jmp);
        patch_jmp(ctx, done_jmp2);
        if (!g_non_allocating_bb) {
          emit_clear_slots(ctx, temp_start, 1);
        }
        sexp_inc_context_depth(ctx, -1);
        return 1;
      }
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
                     sexp_fixnum_literalp(arg1, &c) && (c >= -1073741824LL && c <= 1073741823LL);
    int const_arg0 = !const_arg1 && (code == SEXP_OP_ADD) && sexp_fixnum_literalp(arg0, &c) && (c >= -1073741824LL && c <= 1073741823LL);
    if (const_arg1 || const_arg0) {
      sexp var_arg = const_arg1 ? arg0 : arg1;
      int64_t diff = (int64_t)((uint64_t)c << 1);
      /* a proven fixnum needs no tag check, a proven result no overflow check */
      int check_tag = !sexp_proven_fixnump(ctx, lam, var_arg);
      int check_ovf = !sexp_proven_fixnump(ctx, lam, app);

      generate(ctx, 0, 0, 0, var_arg);
      if (check_tag || check_ovf) {
        emit_mov_rax_slot(ctx, temp_start);
        sexp_inc_context_depth(ctx, 2);
      }

      sexp_sint_t jz_slow = -1;
      if (check_tag) {
        // testb $1, %al
        emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
        jz_slow = emit_jcc(ctx, 0x84);
      }

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
      if (!check_tag && !check_ovf)
        return 1;
      sexp_sint_t jo_slow = check_ovf ? emit_jcc(ctx, 0x80) : -1;

      sexp_sint_t done_jmp = emit_jmp(ctx);

      if (jz_slow != -1) patch_jcc(ctx, jz_slow);
      if (jo_slow != -1) patch_jcc(ctx, jo_slow);
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

      emit_inlined_op_cleanup(ctx, done_jmp, temp_start, 2);
      return 1;
    }

    if (code == SEXP_OP_EQ) {
      // (eq? a b)
      generate_to_temp(ctx, arg0, temp_start);
      generate(ctx, 0, 0, 0, arg1);
      emit_mov_slot_rcx(ctx, temp_start);
      if (!g_non_allocating_bb) {
        emit_clear_slot(ctx, temp_start);
      }

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
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);

      // Load arg0 from temp_start into %rcx
      emit_mov_slot_rcx(ctx, temp_start);

      // Check if both are fixnums:
      sexp_sint_t jz_slow = -1;
      /* comparisons can't overflow, and neither can a proven result */
      int check_ovf = (code == SEXP_OP_ADD || code == SEXP_OP_SUB)
        && !sexp_proven_fixnump(ctx, lam, app);
      if (!sexp_proven_fixnump(ctx, lam, arg0) || !sexp_proven_fixnump(ctx, lam, arg1)) {
        // movq %rcx, %rdx (48 89 ca)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xca);
        // andq %rax, %rdx (48 21 c2)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x21); emit_u8(ctx, 0xc2);
        // testb $1, %dl (f6 c2 01)
        emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc2); emit_u8(ctx, 0x01);
        jz_slow = emit_jcc(ctx, 0x84);
      }

      sexp_sint_t jo_slow = -1;

      if (code == SEXP_OP_ADD) {
        // untag arg0 first, so the add itself overflows exactly when
        // the sum does: (2a+1)+(2b+1) can fit when 2(a+b)+1 doesn't
        // decq %rcx (48 ff c9)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0xff); emit_u8(ctx, 0xc9);
        // addq %rcx, %rax (48 01 c8)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x01); emit_u8(ctx, 0xc8);
        if (check_ovf) jo_slow = emit_jcc(ctx, 0x80);
      } else if (code == SEXP_OP_SUB) {
        // subq %rax, %rcx (48 29 c1)
        emit_u8(ctx, 0x48); emit_u8(ctx, 0x29); emit_u8(ctx, 0xc1);
        if (check_ovf) jo_slow = emit_jcc(ctx, 0x80);
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

      return emit_inlined_op_slow_path(ctx, op, temp_start, 2, jz_slow, jo_slow);
    }

    if (code == SEXP_OP_SET_CAR || code == SEXP_OP_SET_CDR) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);

      // Load arg0 (pair) into %rcx
      emit_mov_slot_rcx(ctx, temp_start);

      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RCX, SEXP_PAIR, &jnz_slow, &jz_slow, &jne_slow);

      // testb $1, 5(%rcx) (immutable check)
      emit_u8(ctx, 0xf6); emit_u8(ctx, 0x41); emit_u8(ctx, 0x05); emit_u8(ctx, 0x01);
      sexp_sint_t jnz_imm_slow = emit_jcc(ctx, 0x85);

      // Store val (in %rax) to off(%rcx)
      int32_t off = (code == SEXP_OP_SET_CAR)
        ? (int32_t)offsetof(struct sexp_struct, value.pair.car)
        : (int32_t)offsetof(struct sexp_struct, value.pair.cdr);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x41); emit_u8(ctx, (uint8_t)off);

      // Return SEXP_VOID
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_VOID);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 2, jnz_slow, jz_slow, jne_slow, jnz_imm_slow);
    }

    if (code == SEXP_OP_VECTOR_REF) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);

      // Load arg0 (vector) from temp_start into %rcx
      emit_mov_slot_rcx(ctx, temp_start);

      // Check vector is pointer
      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RCX, SEXP_VECTOR, &jnz_slow, &jz_slow, &jne_slow);

      // Check index in %rax is fixnum: testb $1, %al
      sexp_sint_t jz_idx_slow = -1;
      if (!sexp_proven_fixnump(ctx, lam, arg1)) {
        emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
        jz_idx_slow = emit_jcc(ctx, 0x84);
      }

      // Unbox fixnum index: sarq $1, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xd1); emit_u8(ctx, 0xf8);

      // Bounds check: cmpq 8(%rcx), %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x3b); emit_u8(ctx, 0x41);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.vector.length));
      sexp_sint_t jae_slow = emit_jcc(ctx, 0x83);

      // Fast load element: movq 16(%rcx, %rax, 8), %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x44); emit_u8(ctx, 0xc1);
      emit_u8(ctx, (uint8_t)(offsetof(struct sexp_struct, value) + sizeof(struct { sexp_uint_t length; })));

      return emit_inlined_op_slow_path(ctx, op, temp_start, 2, jnz_slow, jz_slow, jne_slow, jz_idx_slow, jae_slow);
    }

    if (code == SEXP_OP_BYTES_REF) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);

      // Load arg0 into %rcx
      emit_mov_slot_rcx(ctx, temp_start);

      // Check bytevector pointer
      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RCX, SEXP_BYTES, &jnz_slow, &jz_slow, &jne_slow);

      // Check index in %rax is fixnum
      sexp_sint_t jz_idx_slow = -1;
      if (!sexp_proven_fixnump(ctx, lam, arg1)) {
        emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
        jz_idx_slow = emit_jcc(ctx, 0x84);
      }

      // Unbox fixnum index: sarq $1, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xd1); emit_u8(ctx, 0xf8);

      // Bounds check: cmpq 8(%rcx), %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x3b); emit_u8(ctx, 0x41);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.bytes.length));
      sexp_sint_t jae_slow = emit_jcc(ctx, 0x83);

      // Fast load byte zero-extended: movzbq 16(%rcx, %rax, 1), %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0xb6); emit_u8(ctx, 0x44); emit_u8(ctx, 0x01);
      emit_u8(ctx, (uint8_t)(offsetof(struct sexp_struct, value) + sizeof(struct { sexp_uint_t length; })));

      // Tag as fixnum: addq %rax, %rax; incq %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x01); emit_u8(ctx, 0xc0);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xff); emit_u8(ctx, 0xc0);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 2, jnz_slow, jz_slow, jne_slow, jz_idx_slow, jae_slow);
    }

    if (code == SEXP_OP_STRING_REF) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);

      // Load string into %rcx
      emit_mov_slot_rcx(ctx, temp_start);

      // Check string pointer
      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RCX, SEXP_STRING, &jnz_slow, &jz_slow, &jne_slow);

#if SEXP_USE_DISJOINT_STRING_CURSORS
      // Check string cursor tag: (rax & 7) == 2
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc2);
      emit_u8(ctx, 0x80); emit_u8(ctx, 0xe2); emit_u8(ctx, 0x07);
      emit_u8(ctx, 0x80); emit_u8(ctx, 0xfa); emit_u8(ctx, 0x02);
      sexp_sint_t jne_cur_slow = emit_jcc(ctx, 0x85);

      // Unbox string cursor: sarq $3, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xc1); emit_u8(ctx, 0xf8); emit_u8(ctx, 0x03);
#else
      emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
      sexp_sint_t jne_cur_slow = emit_jcc(ctx, 0x84);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xd1); emit_u8(ctx, 0xf8);
#endif

      // Bounds check against string size at value.string.length (%rcx+24)
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x3b); emit_u8(ctx, 0x41);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.length));
      sexp_sint_t jae_slow = emit_jcc(ctx, 0x83);

      // Compute byte pointer:
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x51);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.bytes));
      emit_u8(ctx, 0x4c); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x41);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.offset));
      emit_u8(ctx, 0x4a); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x54); emit_u8(ctx, 0x02);
      emit_u8(ctx, (uint8_t)(offsetof(struct sexp_struct, value) + sizeof(struct { sexp_uint_t length; })));

      // Load byte: movzbq (%rdx, %rax, 1), %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x0f); emit_u8(ctx, 0xb6); emit_u8(ctx, 0x04); emit_u8(ctx, 0x02);

      // Check if ASCII (< 0x80): testb $0x80, %al
      emit_u8(ctx, 0xa8); emit_u8(ctx, 0x80);
      sexp_sint_t jnz_utf8_slow = emit_jcc(ctx, 0x85);

      // Convert ASCII byte to Scheme char: (b << 8) | 30
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xc1); emit_u8(ctx, 0xe0); emit_u8(ctx, 0x08);
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xc8); emit_u8(ctx, 0x1e);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 2, jnz_slow, jz_slow, jne_slow, jne_cur_slow, jae_slow, jnz_utf8_slow);
    }

#if SEXP_USE_UTF8_STRINGS
    if (code == SEXP_OP_STRING_CURSOR_NEXT) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);

      emit_mov_slot_rcx(ctx, temp_start);

      // Check string pointer
      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RCX, SEXP_STRING, &jnz_slow, &jz_slow, &jne_slow);

      // Check string cursor tag
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc2);
      emit_u8(ctx, 0x80); emit_u8(ctx, 0xe2); emit_u8(ctx, 0x07);
      emit_u8(ctx, 0x80); emit_u8(ctx, 0xfa); emit_u8(ctx, 0x02);
      sexp_sint_t jne_cur_slow = emit_jcc(ctx, 0x85);

      // Unbox cursor to %r9: movq %rax, %r9; sarq $3, %r9
      emit_u8(ctx, 0x49); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc1);
      emit_u8(ctx, 0x49); emit_u8(ctx, 0xc1); emit_u8(ctx, 0xf9); emit_u8(ctx, 0x03);

      // Bounds check against string size at %rcx+24: cmpq 24(%rcx), %r9; jae slow
      emit_u8(ctx, 0x4c); emit_u8(ctx, 0x3b); emit_u8(ctx, 0x49);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.length));
      sexp_sint_t jae_slow = emit_jcc(ctx, 0x83);

      // Compute byte pointer:
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x51);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.bytes));
      emit_u8(ctx, 0x4c); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x41);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.offset));
      emit_u8(ctx, 0x4a); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x54); emit_u8(ctx, 0x02);
      emit_u8(ctx, (uint8_t)(offsetof(struct sexp_struct, value) + sizeof(struct { sexp_uint_t length; })));

      // Load byte at cursor: movzbq (%rdx, %r9, 1), %r8
      emit_u8(ctx, 0x4e); emit_u8(ctx, 0x0f); emit_u8(ctx, 0xb6); emit_u8(ctx, 0x04); emit_u8(ctx, 0x0a);

      // Check ASCII: testb $0x80, %r8b
      emit_u8(ctx, 0x41); emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc0); emit_u8(ctx, 0x80);
      sexp_sint_t jnz_utf8_slow = emit_jcc(ctx, 0x85);

      // Advance cursor: addq $8, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xc0); emit_u8(ctx, 0x08);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 2, jnz_slow, jz_slow, jne_slow, jne_cur_slow, jae_slow, jnz_utf8_slow);
    }

    if (code == SEXP_OP_STRING_CURSOR_PREV) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);

      emit_mov_slot_rcx(ctx, temp_start);

      // Check string pointer
      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RCX, SEXP_STRING, &jnz_slow, &jz_slow, &jne_slow);

      // Check string cursor tag
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc2);
      emit_u8(ctx, 0x80); emit_u8(ctx, 0xe2); emit_u8(ctx, 0x07);
      emit_u8(ctx, 0x80); emit_u8(ctx, 0xfa); emit_u8(ctx, 0x02);
      sexp_sint_t jne_cur_slow = emit_jcc(ctx, 0x85);

      // Unbox cursor to %r9: movq %rax, %r9; sarq $3, %r9
      emit_u8(ctx, 0x49); emit_u8(ctx, 0x89); emit_u8(ctx, 0xc1);
      emit_u8(ctx, 0x49); emit_u8(ctx, 0xc1); emit_u8(ctx, 0xf9); emit_u8(ctx, 0x03);

      // Check cursor > 0: testq %r9, %r9; jle slow
      emit_u8(ctx, 0x4d); emit_u8(ctx, 0x85); emit_u8(ctx, 0xc9);
      sexp_sint_t jle_slow = emit_jcc(ctx, 0x8e);

      // Check cursor <= string size: cmpq 24(%rcx), %r9; ja slow
      emit_u8(ctx, 0x4c); emit_u8(ctx, 0x3b); emit_u8(ctx, 0x49);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.length));
      sexp_sint_t ja_slow = emit_jcc(ctx, 0x87);

      // Compute byte pointer:
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x51);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.bytes));
      emit_u8(ctx, 0x4c); emit_u8(ctx, 0x8b); emit_u8(ctx, 0x41);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.string.offset));
      emit_u8(ctx, 0x4a); emit_u8(ctx, 0x8d); emit_u8(ctx, 0x54); emit_u8(ctx, 0x02);
      emit_u8(ctx, (uint8_t)(offsetof(struct sexp_struct, value) + sizeof(struct { sexp_uint_t length; })));

      // Load previous byte: movzbq -1(%rdx, %r9, 1), %r8
      emit_u8(ctx, 0x4e); emit_u8(ctx, 0x0f); emit_u8(ctx, 0xb6); emit_u8(ctx, 0x44); emit_u8(ctx, 0x0a); emit_u8(ctx, 0xff);

      // Check ASCII: testb $0x80, %r8b
      emit_u8(ctx, 0x41); emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc0); emit_u8(ctx, 0x80);
      sexp_sint_t jnz_utf8_slow = emit_jcc(ctx, 0x85);

      // Decrement cursor: subq $8, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x83); emit_u8(ctx, 0xe8); emit_u8(ctx, 0x08);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 2, jnz_slow, jz_slow, jne_slow, jne_cur_slow, jle_slow, ja_slow, jnz_utf8_slow);
    }
#endif

    if (code == SEXP_OP_SLOT_SET) {
      sexp tid_sexp = sexp_opcode_data(op);
      sexp_sint_t tid = sexp_typep(tid_sexp) ? sexp_type_tag(tid_sexp) :
                        sexp_fixnump(tid_sexp) ? sexp_unbox_fixnum(tid_sexp) : -1;
      sexp idx_sexp = sexp_opcode_data2(op);
      sexp_sint_t idx = sexp_fixnump(idx_sexp) ? sexp_unbox_fixnum(idx_sexp) : -1;

      if (tid >= 0 && idx >= 0) {
        generate_to_temp(ctx, arg0, temp_start);
        generate_to_temp(ctx, arg1, temp_start + 1);

        // Load arg0 (instance) into %rcx
        emit_mov_slot_rcx(ctx, temp_start);

        // Check instance pointer
        sexp_sint_t jnz_slow, jz_slow;
        emit_check_pointer(ctx, REG_RCX, &jnz_slow, &jz_slow);

        // Tag check: cmpl $tid, (%rcx)
        emit_u8(ctx, 0x81); emit_u8(ctx, 0x39); emit_u32(ctx, (uint32_t)tid);
        sexp_sint_t jne_slow = emit_jcc(ctx, 0x85);

        // Immutable check: testb $1, 5(%rcx)
        emit_u8(ctx, 0xf6); emit_u8(ctx, 0x41); emit_u8(ctx, 0x05); emit_u8(ctx, 0x01);
        sexp_sint_t jnz_imm_slow = emit_jcc(ctx, 0x85);

        // Store val (in %rax) to off(%rcx)
        int32_t off = (int32_t)(offsetof(struct sexp_struct, value) + idx * sizeof(sexp));
        if (off >= -128 && off <= 127) {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x41); emit_u8(ctx, (uint8_t)off);
        } else {
          emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x81); emit_u32(ctx, (uint32_t)off);
        }

        // Return SEXP_VOID
        emit_mov_imm64_rax(ctx, (uint64_t)SEXP_VOID);

        return emit_inlined_op_slow_path(ctx, op, temp_start, 2, jnz_slow, jz_slow, jne_slow, jnz_imm_slow);
      }
    }

    if (code == SEXP_OP_ISA) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);

      // Load arg0 (instance) into %rcx; %rax holds arg1 (type)
      emit_mov_slot_rcx(ctx, temp_start);

      // Check instance is pointer
      sexp_sint_t jnz_false, jz_false;
      emit_check_pointer(ctx, REG_RCX, &jnz_false, &jz_false);

      // Check arg1 is pointer and tag is SEXP_TYPE
      sexp_sint_t jnz_slow, jz_slow;
      emit_check_pointer(ctx, REG_RAX, &jnz_slow, &jz_slow);
      sexp_sint_t jne_slow = emit_check_tag(ctx, REG_RAX, SEXP_TYPE);

      // Compare: movl (%rcx), %edx; cmpl %edx, 80(%rax) (value.type.tag)
      emit_u8(ctx, 0x8b); emit_u8(ctx, 0x11);
      emit_u8(ctx, 0x39); emit_u8(ctx, 0x50);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.type.tag));
      sexp_sint_t je_true = emit_jcc(ctx, 0x84);

      // If not equal, fall back to slow path (sexp_check_type)
      sexp_sint_t jmp_slow = emit_jmp(ctx);

      // False path:
      patch_jcc(ctx, jnz_false);
      patch_jcc(ctx, jz_false);
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_FALSE);
      sexp_sint_t done_jmp = emit_jmp(ctx);

      // True path:
      patch_jcc(ctx, je_true);
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_TRUE);
      sexp_sint_t done_jmp2 = emit_jmp(ctx);

      // Slow path:
      patch_jcc(ctx, jnz_slow);
      patch_jcc(ctx, jz_slow);
      patch_jcc(ctx, jne_slow);
      patch_jmp(ctx, jmp_slow);
      emit_native_dispatch_op(ctx, op, temp_start, 2);
      if (g_non_allocating_bb) {
        emit_clear_slots(ctx, temp_start, 2);
      }
      patch_jmp(ctx, done_jmp);
      patch_jmp(ctx, done_jmp2);
      if (!g_non_allocating_bb) {
        emit_clear_slots(ctx, temp_start, 2);
      }
      sexp_inc_context_depth(ctx, -2);
      return 1;
    }
  }

  // 3-argument cases
  if (num_args == 3) {
    sexp arg0 = sexp_cadr(app);
    sexp arg1 = sexp_caddr(app);
    sexp arg2 = sexp_cadddr(app);

    if (code == SEXP_OP_VECTOR_SET) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);
      generate_to_temp(ctx, arg2, temp_start + 2);

      // Load arg0 (vector) into %rcx
      emit_mov_slot_rcx(ctx, temp_start);
      // Load arg1 (index) into %rdx
      emit_mov_slot_rdx(ctx, temp_start + 1);
      // %rax holds arg2 (val)

      // Check vector is pointer
      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RCX, SEXP_VECTOR, &jnz_slow, &jz_slow, &jne_slow);

      // Immutable check: testb $1, 5(%rcx)
      emit_u8(ctx, 0xf6); emit_u8(ctx, 0x41); emit_u8(ctx, 0x05); emit_u8(ctx, 0x01);
      sexp_sint_t jnz_imm_slow = emit_jcc(ctx, 0x85);

      // Check index in %rdx is fixnum: testb $1, %dl
      sexp_sint_t jz_idx_slow = -1;
      if (!sexp_proven_fixnump(ctx, lam, arg1)) {
        emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc2); emit_u8(ctx, 0x01);
        jz_idx_slow = emit_jcc(ctx, 0x84);
      }

      // Unbox fixnum index: sarq $1, %rdx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xd1); emit_u8(ctx, 0xfa);

      // Bounds check: cmpq 8(%rcx), %rdx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x3b); emit_u8(ctx, 0x51);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.vector.length));
      sexp_sint_t jae_slow = emit_jcc(ctx, 0x83);

      // Store val: movq %rax, 16(%rcx, %rdx, 8)
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x89); emit_u8(ctx, 0x44); emit_u8(ctx, 0xd1);
      emit_u8(ctx, (uint8_t)(offsetof(struct sexp_struct, value) + sizeof(struct { sexp_uint_t length; })));

      // Return SEXP_VOID
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_VOID);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 3, jnz_slow, jz_slow, jne_slow, jnz_imm_slow, jz_idx_slow, jae_slow);
    }

    if (code == SEXP_OP_BYTES_SET) {
      generate_to_temp(ctx, arg0, temp_start);
      generate_to_temp(ctx, arg1, temp_start + 1);
      generate_to_temp(ctx, arg2, temp_start + 2);

      // Load arg0 (bytevector) into %rcx
      emit_mov_slot_rcx(ctx, temp_start);
      // Load arg1 (index) into %rdx
      emit_mov_slot_rdx(ctx, temp_start + 1);
      // %rax holds arg2 (val)

      // Check bytevector pointer
      sexp_sint_t jnz_slow, jz_slow, jne_slow;
      emit_check_type(ctx, arg0, REG_RCX, SEXP_BYTES, &jnz_slow, &jz_slow, &jne_slow);

      // Immutable check: testb $1, 5(%rcx)
      emit_u8(ctx, 0xf6); emit_u8(ctx, 0x41); emit_u8(ctx, 0x05); emit_u8(ctx, 0x01);
      sexp_sint_t jnz_imm_slow = emit_jcc(ctx, 0x85);

      // Check index in %rdx is fixnum: testb $1, %dl
      sexp_sint_t jz_idx_slow = -1;
      if (!sexp_proven_fixnump(ctx, lam, arg1)) {
        emit_u8(ctx, 0xf6); emit_u8(ctx, 0xc2); emit_u8(ctx, 0x01);
        jz_idx_slow = emit_jcc(ctx, 0x84);
      }

      // Unbox fixnum index: sarq $1, %rdx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xd1); emit_u8(ctx, 0xfa);

      // Bounds check: cmpq 8(%rcx), %rdx
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x3b); emit_u8(ctx, 0x51);
      emit_u8(ctx, (uint8_t)offsetof(struct sexp_struct, value.bytes.length));
      sexp_sint_t jae_slow = emit_jcc(ctx, 0x83);

      // Check val in %rax is fixnum: testb $1, %al
      emit_u8(ctx, 0xa8); emit_u8(ctx, 0x01);
      sexp_sint_t jz_val_slow = emit_jcc(ctx, 0x84);

      // Unbox val: sarq $1, %rax
      emit_u8(ctx, 0x48); emit_u8(ctx, 0xd1); emit_u8(ctx, 0xf8);

      // Range check val [0, 255]: cmpq $255, %rax; ja slow
      emit_u8(ctx, 0x48); emit_u8(ctx, 0x3d); emit_u32(ctx, 255);
      sexp_sint_t ja_val_slow = emit_jcc(ctx, 0x87);

      // Store byte: movb %al, 16(%rcx, %rdx, 1)
      emit_u8(ctx, 0x88); emit_u8(ctx, 0x44); emit_u8(ctx, 0x11);
      emit_u8(ctx, (uint8_t)(offsetof(struct sexp_struct, value) + sizeof(struct { sexp_uint_t length; })));

      // Return SEXP_VOID
      emit_mov_imm64_rax(ctx, (uint64_t)SEXP_VOID);

      return emit_inlined_op_slow_path(ctx, op, temp_start, 3, jnz_slow, jz_slow, jne_slow, jnz_imm_slow, jz_idx_slow, jae_slow, jz_val_slow, ja_val_slow);
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
      if (!g_non_allocating_bb) {
        emit_clear_slot(ctx, temp_start);
      }
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
  int loop_start = (int)g_loop_start;

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

  if (!g_non_allocating_bb) {
    emit_clear_slots(ctx, temp_start, num_args);
  }

  sexp_inc_context_depth(ctx, -num_args);

#if SEXP_USE_GREEN_THREADS
  uint32_t refuel_off = (uint32_t)offsetof(struct sexp_struct, value.context.refuel);
  emit_u8(ctx, 0x49); emit_u8(ctx, 0xff); emit_u8(ctx, 0x8c); emit_u8(ctx, 0x24); // decq refuel_off(%r12)
  emit_u32(ctx, refuel_off);
  emit_u8(ctx, 0x7f); emit_u8(ctx, 0x10); // jg skip (16 bytes)
  emit_u8(ctx, 0x4c); emit_u8(ctx, 0x89); emit_u8(ctx, 0xe7); // mov %r12, %rdi
  emit_call_abs(ctx, (void*)sexp_native_loop_yield);
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

static int compute_ast_max_depth (sexp ctx, sexp x) {
  if (!x || !sexp_pointerp(x)) return 0;
  int tag = sexp_pointer_tag(x);
  switch (tag) {
    case SEXP_PAIR: {
      int max_d = 0;
      int i = 0;
      sexp ls = sexp_cdr(x);
      for (; sexp_pairp(ls); ls = sexp_cdr(ls), i++) {
        int d = i + compute_ast_max_depth(ctx, sexp_car(ls));
        if (d > max_d) max_d = d;
      }
      int d_op = i + compute_ast_max_depth(ctx, sexp_car(x));
      if (d_op > max_d) max_d = d_op;
      if (i > max_d) max_d = i;
      return max_d;
    }
    case SEXP_CND: {
      int d1 = compute_ast_max_depth(ctx, sexp_cnd_test(x));
      int d2 = compute_ast_max_depth(ctx, sexp_cnd_pass(x));
      int d3 = compute_ast_max_depth(ctx, sexp_cnd_fail(x));
      int max_d = d1;
      if (d2 > max_d) max_d = d2;
      if (d3 > max_d) max_d = d3;
      return max_d;
    }
    case SEXP_SET:
    case SEXP_SET_SYN:
      return compute_ast_max_depth(ctx, sexp_set_value(x));
    case SEXP_SEQ: {
      int max_d = 0;
      for (sexp ls = sexp_seq_ls(x); sexp_pairp(ls); ls = sexp_cdr(ls)) {
        int d = compute_ast_max_depth(ctx, sexp_car(ls));
        if (d > max_d) max_d = d;
      }
      return max_d;
    }
    case SEXP_LAMBDA: {
      int num_fvs = 0;
      for (sexp ls = sexp_lambda_fv(x); sexp_pairp(ls); ls = sexp_cdr(ls))
        num_fvs++;
      return num_fvs;
    }
    default:
      return 0;
  }
}

void sexp_native_prepare_ast (sexp ctx, sexp ast) {
  int est = compute_ast_max_depth(ctx, ast);
  int max_depth = est + 4;
  if (max_depth < 120) max_depth = 120;
  if (sexp_vectorp(sexp_context_specific(ctx))) {
    sexp bc = sexp_context_bc(ctx);
    if (bc && sexp_bytecodep(bc))
      sexp_bytecode_max_depth(bc) = max_depth;
    sexp_context_max_depth(ctx) = sexp_make_fixnum(max_depth);
  }
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
  sexp_sint_t saved_loop_start = g_loop_start;

  if (!name && sexp_symbolp(sexp_lambda_name(lambda))) {
    name = sexp_lambda_name(lambda);
  }
  if (name && !loc && sexp_symbolp(name)) {
    loc = sexp_env_cell(ctx, sexp_context_env(ctx), name, 0);
  }

  int num_params = get_param_count(lambda);
  int num_locals = get_local_count(lambda);
  int est = compute_ast_max_depth(ctx2, sexp_lambda_body(lambda));
  int max_depth = est + 4;
  sexp_bytecode_max_depth(sexp_context_bc(ctx2)) = max_depth;
  sexp_context_max_depth(ctx2) = sexp_make_fixnum(max_depth);

  // Emit function entry
  sexp_emit_enter(ctx2);

  // Box mutable variables
  for (ls = sexp_lambda_sv(lambda); sexp_pairp(ls); ls = sexp_cdr(ls)) {
    int k = sexp_param_index(ctx, lambda, sexp_car(ls));
    int slot = get_local_slot(lambda, k);
    emit_box_var(ctx2, sexp_car(ls), slot);
  }

  // Record loop start for self-tail jumps
  g_loop_start = sexp_unbox_fixnum(sexp_context_pos(ctx2));

  // Check the types the body relies on params having, on every entry
  emit_param_type_checks(ctx2, lambda, num_params);

  int saved_non_alloc = g_non_allocating_bb;

  g_non_allocating_bb = sexp_lambda_non_alloc_p(lambda);

  // Generate body
  sexp_context_tailp(ctx2) = 1;
  generate(ctx2, name, loc, lambda, sexp_lambda_body(lambda));
  g_loop_start = saved_loop_start;

  g_non_allocating_bb = saved_non_alloc;

  flags = sexp_make_fixnum(sexp_not(sexp_listp(ctx, sexp_lambda_params(lambda)))
                           ? (SEXP_PROC_VARIADIC + (sexp_rest_unused_p(lambda) ? SEXP_PROC_UNUSED_REST : 0))
                           : SEXP_PROC_NONE);
  len = sexp_length(ctx2, sexp_lambda_params(lambda));
  bc = sexp_complete_bytecode(ctx2);

  if (sexp_exceptionp(bc)) {
    sexp_context_exception(ctx) = bc;
  } else {
    int act = sexp_bytecode_max_depth(bc);
    int num_slots = get_context_num_slots(ctx2);
    if (num_params + num_locals + act >= num_slots) {
      fprintf(stderr, "WARNING: frame slot overflow: name=%s, slots=%d, needed=%d\n",
              (name && sexp_symbolp(name)) ? sexp_string_data(sexp_symbol_to_string(ctx, name)) : "anon",
              num_slots, num_params + num_locals + act + 1);
    }
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
    /* the self name and loc reach a lambda only from generate_set, */
    /* never from an enclosing lambda's tail position */
    case SEXP_LAMBDA: generate_lambda(ctx, 0, 0, lam, x); break;
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
  int saved_non_alloc = g_non_allocating_bb;
  g_non_allocating_bb = !sexp_expr_may_allocp(ctx, lam, x, name, loc);
  generate(ctx, name, loc, lam, x);
  g_non_allocating_bb = saved_non_alloc;
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
  int max_depth = j + 4;
  sexp_bytecode_max_depth(sexp_context_bc(ctx2)) = max_depth;
  sexp_context_max_depth(ctx2) = sexp_make_fixnum(max_depth);
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

static sexp sexp_native_call1 (sexp ctx, sexp f, sexp x) {
  sexp args[1] = { x };
  return sexp_native_call(ctx, f, 1, args);
}

static sexp sexp_native_apply_list (sexp ctx, sexp proc, sexp args, int entry) {
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
  return sexp_native_call_aux(ctx, proc, len, argv, entry);
}

sexp sexp_apply (sexp ctx, sexp proc, sexp args) {
  return sexp_native_apply_list(ctx, proc, args, 1);
}

sexp sexp_apply1 (sexp ctx, sexp f, sexp x) {
  if (sexp_opcodep(f) && sexp_opcode_func(f)) {
    return ((sexp_proc2)sexp_opcode_func(f))(ctx, f, 1, x);
  } else {
    sexp args[1] = { x };
    return sexp_native_call_entry(ctx, f, 1, args);
  }
}

sexp sexp_apply2 (sexp ctx, sexp f, sexp x, sexp y) {
  if (sexp_opcodep(f) && sexp_opcode_func(f)) {
    return ((sexp_proc3)sexp_opcode_func(f))(ctx, f, 2, x, y);
  } else {
    sexp args[2] = { x, y };
    return sexp_native_call_entry(ctx, f, 2, args);
  }
}

sexp sexp_apply3 (sexp ctx, sexp f, sexp x, sexp y, sexp z) {
  if (sexp_opcodep(f) && sexp_opcode_func(f)) {
    return ((sexp_proc4)sexp_opcode_func(f))(ctx, f, 3, x, y, z);
  } else {
    sexp args[3] = { x, y, z };
    return sexp_native_call_entry(ctx, f, 3, args);
  }
}

sexp sexp_apply_no_err_handler (sexp ctx, sexp proc, sexp args) {
  sexp res, err_cell;
  sexp_gc_var2(handler, params);
  sexp_gc_preserve2(ctx, handler, params);
#if SEXP_USE_GREEN_THREADS
  params = sexp_context_params(ctx);
  sexp_context_params(ctx) = SEXP_NULL;
  ++sexp_context_refuel(ctx);
#endif
  err_cell = sexp_global(ctx, SEXP_G_ERR_HANDLER);
  err_cell = sexp_opcodep(err_cell) ? sexp_opcode_data(err_cell) : SEXP_FALSE;
  handler = sexp_pairp(err_cell) ? sexp_cdr(err_cell) : SEXP_FALSE;
  if (sexp_pairp(err_cell)) sexp_cdr(err_cell) = SEXP_FALSE;
  res = sexp_apply(ctx, proc, args);
  if (sexp_pairp(err_cell)) sexp_cdr(err_cell) = handler;
#if SEXP_USE_GREEN_THREADS
  sexp_context_params(ctx) = params;
#endif
  sexp_gc_release2(ctx);
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
