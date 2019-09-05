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
#include <sys/proc.h>
#endif
#include <sys/sysctl.h>
#ifndef __NetBSD__
#include <sys/user.h>
#endif

static sexp sexp_pid_cmdline (sexp ctx, int pid) {
#ifdef __NetBSD__
  /*
   * Newer version with defined interface that doesn't expose kernel
   * guts and works with 64-bit kernel, 32-bit userland.
   */
  struct kinfo_proc2 res;
  int id = KERN_PROC2;
#else
  struct kinfo_proc res;
  int id = KERN_PROC;
#endif
  size_t reslen = sizeof(res);
#if defined(__NetBSD__) || defined(__OpenBSD__)
  int name[6] = {CTL_KERN, id, KERN_PROC_PID, pid, reslen, 1};
  unsigned namelen = 6;
#else
  int name[4] = {CTL_KERN, id, KERN_PROC_PID, pid};
  unsigned namelen = 4;
#endif
  if (sysctl(name, namelen, &res, &reslen, NULL, 0) >= 0 && reslen > 0) {
#if defined(__APPLE__)
    return sexp_c_string(ctx, res.kp_proc.p_comm, -1);
#elif defined(__NetBSD__) || defined(__OpenBSD__)
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

static pid_t sexp_fork_and_kill_threads (sexp ctx) {
  pid_t res = fork();
#if SEXP_USE_GREEN_THREADS
  if (res == 0) {               /* child */
    sexp_global(ctx, SEXP_G_THREADS_FRONT) = SEXP_NULL;
    sexp_global(ctx, SEXP_G_THREADS_BACK) = SEXP_NULL;
    sexp_global(ctx, SEXP_G_THREADS_PAUSED) = SEXP_NULL;
  }
#endif
  return res;
}

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
