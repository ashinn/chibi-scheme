/*  signal.c -- process signals interface                     */
/*  Copyright (c) 2009-2011 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#define SEXP_MAX_SIGNUM 32

static sexp sexp_signal_contexts[SEXP_MAX_SIGNUM];

static struct sigaction call_sigaction, call_sigdefault, call_sigignore;

static void sexp_call_sigaction (int signum, siginfo_t *info, void *uctx) {
  sexp ctx;
#if ! SEXP_USE_GREEN_THREADS
  sexp sigctx, handler;
  sexp_gc_var1(args);
#endif
  ctx = sexp_signal_contexts[signum];
  if (ctx) {
#if SEXP_USE_GREEN_THREADS
    sexp_global(ctx, SEXP_G_THREADS_SIGNALS) =
      sexp_make_fixnum((1UL<<signum) | sexp_unbox_fixnum(sexp_global(ctx, SEXP_G_THREADS_SIGNALS)));
#else
    handler = sexp_vector_ref(sexp_global(ctx, SEXP_G_SIGNAL_HANDLERS),
                              sexp_make_fixnum(signum));
    if (sexp_applicablep(handler)) {
      sigctx = sexp_make_child_context(ctx, NULL);
      sexp_gc_preserve1(sigctx, args);
      args = sexp_cons(sigctx, sexp_make_fixnum(signum), SEXP_NULL);
      sexp_apply(sigctx, handler, args);
      sexp_gc_release1(sigctx);
    }
#endif
  }
}

static sexp sexp_set_signal_action (sexp ctx, sexp self, sexp signum, sexp newaction) {
  int res;
  sexp oldaction;
  if (! (sexp_fixnump(signum) && sexp_unbox_fixnum(signum) > 0
         && sexp_unbox_fixnum(signum) < SEXP_MAX_SIGNUM))
    return sexp_xtype_exception(ctx, self, "not a valid signal number", signum);
  if (! (sexp_procedurep(newaction) || sexp_opcodep(newaction)
         || sexp_booleanp(newaction)))
    return sexp_type_exception(ctx, self, SEXP_PROCEDURE, newaction);
  if (! sexp_vectorp(sexp_global(ctx, SEXP_G_SIGNAL_HANDLERS)))
    sexp_global(ctx, SEXP_G_SIGNAL_HANDLERS)
      = sexp_make_vector(ctx, sexp_make_fixnum(SEXP_MAX_SIGNUM), SEXP_FALSE);
  oldaction = sexp_vector_ref(sexp_global(ctx, SEXP_G_SIGNAL_HANDLERS), signum);
  res = sigaction(sexp_unbox_fixnum(signum),
                  (sexp_booleanp(newaction) ?
                   (sexp_truep(newaction) ? &call_sigdefault : &call_sigignore)
                   : &call_sigaction),
                  NULL);
  if (res)
    return sexp_user_exception(ctx, self, "couldn't set signal", signum);
  sexp_vector_set(sexp_global(ctx, SEXP_G_SIGNAL_HANDLERS), signum, newaction);
  sexp_signal_contexts[sexp_unbox_fixnum(signum)] = ctx;
  return oldaction;
}

#if SEXP_BSD

#include <sys/time.h>
#ifndef __DragonFly__
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/proc.h>
#endif
#include <sys/sysctl.h>
#include <sys/user.h>

static sexp sexp_pid_cmdline (sexp ctx, int pid) {
  size_t reslen = sizeof(struct kinfo_proc);
  struct kinfo_proc res;
  int name[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID, pid};
  if (sysctl(name, 4, &res, &reslen, NULL, 0) >= 0) {
#if defined(__APPLE__) || defined(__NetBSD__)
    return sexp_c_string(ctx, res.kp_proc.p_comm, -1);
#elif __OpenBSD__
    return sexp_c_string(ctx, res.p_comm, -1);
#elif __DragonFly__
    return sexp_c_string(ctx, res.kp_comm, -1);
#else
    return sexp_c_string(ctx, res.ki_comm, -1);
#endif
  } else {
    return SEXP_FALSE;
  }
}

#else

/* #include <sys/syscall.h> */
/* #include <linux/sysctl.h> */

/* #define CMDLINE_LENGTH 512 */

/* static sexp sexp_pid_cmdline (sexp ctx, int pid) { */
/*   struct __sysctl_args args; */
/*   char cmdline[CMDLINE_LENGTH]; */
/*   size_t cmdline_length; */
/*   int name[] = { CTL_KERN, KERN_OSTYPE }; */

/*   memset(&args, 0, sizeof(struct __sysctl_args)); */
/*   args.name = name; */
/*   args.nlen = sizeof(name)/sizeof(name[0]); */
/*   args.oldval = cmdline; */
/*   args.oldlenp = &cmdline_length; */
/*   cmdline_length = sizeof(cmdline); */

/*   if (syscall(SYS__sysctl, &args) == -1) { */
/*     return SEXP_FALSE; */
/*   } else { */
/*     return sexp_c_string(ctx, cmdline, -1); */
/*   } */
/* } */

#endif

static void sexp_init_signals (sexp ctx, sexp env) {
  call_sigaction.sa_sigaction  = sexp_call_sigaction;
#if SEXP_USE_GREEN_THREADS
  call_sigaction.sa_flags      = SA_SIGINFO | SA_RESTART /* | SA_NODEFER */;
  sigfillset(&call_sigaction.sa_mask);
#else
  call_sigaction.sa_flags      = SA_SIGINFO | SA_RESTART | SA_NODEFER;
#endif
  call_sigdefault.sa_handler   = SIG_DFL;
  call_sigignore.sa_handler    = SIG_IGN;
  memset(sexp_signal_contexts, 0, sizeof(sexp_signal_contexts));
}
