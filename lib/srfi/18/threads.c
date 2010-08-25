/*  threads.c -- SRFI-18 thread primitives                    */
/*  Copyright (c) 2010 Alex Shinn.  All rights reserved.      */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#define sexp_mutex_name(x)       sexp_slot_ref(x, 0)
#define sexp_mutex_specific(x)   sexp_slot_ref(x, 1)
#define sexp_mutex_thread(x)     sexp_slot_ref(x, 2)
#define sexp_mutex_lockp(x)       sexp_slot_ref(x, 3)

#define sexp_condvar_name(x)     sexp_slot_ref(x, 0)
#define sexp_condvar_specific(x) sexp_slot_ref(x, 1)
#define sexp_condvar_threads(x)  sexp_slot_ref(x, 2)

#define timeval_le(a, b) (((a).tv_sec < (b).tv_sec) || (((a).tv_sec == (b).tv_sec) && ((a).tv_usec < (b).tv_usec)))
#define sexp_context_before(c, t) (((sexp_context_timeval(c).tv_sec != 0) || (sexp_context_timeval(c).tv_usec != 0)) && timeval_le(sexp_context_timeval(c), t))

/* static int mutex_id, condvar_id; */

/**************************** threads *************************************/

static void sexp_define_type_predicate (sexp ctx, sexp env, char *cname, sexp_uint_t type) {
  sexp_gc_var2(name, op);
  sexp_gc_preserve2(ctx, name, op);
  name = sexp_c_string(ctx, cname, -1);
  op = sexp_make_type_predicate(ctx, name, sexp_make_fixnum(type));
  sexp_env_define(ctx, env, name=sexp_intern(ctx, cname, -1), op);
  sexp_gc_release2(ctx);
}

sexp sexp_thread_timeoutp (sexp ctx sexp_api_params(self, n)) {
  return sexp_make_boolean(sexp_context_timeoutp(ctx));
}

sexp sexp_thread_name (sexp ctx sexp_api_params(self, n), sexp thread) {
  sexp_assert_type(ctx, sexp_contextp, SEXP_CONTEXT, thread);
  return sexp_context_name(thread);
}

sexp sexp_thread_specific (sexp ctx sexp_api_params(self, n), sexp thread) {
  sexp_assert_type(ctx, sexp_contextp, SEXP_CONTEXT, thread);
  return sexp_context_specific(thread);
}

sexp sexp_thread_specific_set (sexp ctx sexp_api_params(self, n), sexp thread, sexp val) {
  sexp_assert_type(ctx, sexp_contextp, SEXP_CONTEXT, thread);
  sexp_context_specific(thread) = val;
  return SEXP_VOID;
}

sexp sexp_current_thread (sexp ctx sexp_api_params(self, n)) {
  return ctx;
}

sexp sexp_make_thread (sexp ctx sexp_api_params(self, n), sexp thunk, sexp name) {
  sexp res, *stack;
  sexp_assert_type(ctx, sexp_procedurep, SEXP_PROCEDURE, thunk);
  res = sexp_make_eval_context(ctx, SEXP_FALSE, sexp_context_env(ctx), 0);
  sexp_context_proc(res) = thunk;
  sexp_context_ip(res) = sexp_bytecode_data(sexp_procedure_code(thunk));
  stack = sexp_stack_data(sexp_context_stack(res));
  stack[0] = stack[1] = stack[3] = SEXP_ZERO;
  stack[2] = sexp_global(ctx, SEXP_G_FINAL_RESUMER);
  sexp_context_top(res) = 4;
  sexp_context_last_fp(res) = 0;
  return res;
}

sexp sexp_thread_start (sexp ctx sexp_api_params(self, n), sexp thread) {
  sexp cell;
  sexp_assert_type(ctx, sexp_contextp, SEXP_CONTEXT, thread);
  cell = sexp_cons(ctx, thread, SEXP_NULL);
  if (sexp_pairp(sexp_global(ctx, SEXP_G_THREADS_BACK))) {
    sexp_cdr(sexp_global(ctx, SEXP_G_THREADS_BACK)) = cell;
    sexp_global(ctx, SEXP_G_THREADS_BACK) = cell;
  } else {			/* init queue */
    sexp_global(ctx, SEXP_G_THREADS_BACK) = sexp_global(ctx, SEXP_G_THREADS_FRONT) = cell;
  }
  return thread;
}

sexp sexp_thread_terminate (sexp ctx sexp_api_params(self, n), sexp thread) {
  sexp res = sexp_make_boolean(ctx == thread);
  for ( ; thread && sexp_contextp(thread); thread=sexp_context_child(thread))
    sexp_context_refuel(thread) = 0;
  /* return true if terminating self */
  return res;
}

static void sexp_insert_timed (sexp ctx, sexp thread, sexp timeout) {
#if SEXP_USE_FLONUMS
  double d;
#endif
  sexp ls1=SEXP_NULL, ls2=sexp_global(ctx, SEXP_G_THREADS_PAUSED);
  if (sexp_integerp(timeout) || sexp_flonump(timeout))
    gettimeofday(&sexp_context_timeval(ctx), NULL);
  if (sexp_integerp(timeout)) {
    sexp_context_timeval(ctx).tv_sec += sexp_unbox_fixnum(timeout);
#if SEXP_USE_FLONUMS
  } else if (sexp_flonump(timeout)) {
    d = sexp_flonum_value(timeout);
    sexp_context_timeval(ctx).tv_sec += trunc(d);
    sexp_context_timeval(ctx).tv_usec += (d-trunc(d))*1000000;
#endif
  } else {
    sexp_context_timeval(ctx).tv_sec = 0;
    sexp_context_timeval(ctx).tv_usec = 0;
  }
  if (sexp_numberp(timeout))
    while (sexp_pairp(ls2)
           && sexp_context_before(sexp_car(ls2), sexp_context_timeval(ctx)))
      ls1=ls2, ls2=sexp_cdr(ls2);
  else
    while (sexp_pairp(ls2) && sexp_context_timeval(sexp_car(ls2)).tv_sec)
      ls1=ls2, ls2=sexp_cdr(ls2);
  if (ls1 == SEXP_NULL)
    sexp_global(ctx, SEXP_G_THREADS_PAUSED) = sexp_cons(ctx, thread, ls2);
  else
    sexp_cdr(ls1) = sexp_cons(ctx, thread, ls2);
}

sexp sexp_thread_join (sexp ctx sexp_api_params(self, n), sexp thread, sexp timeout) {
  sexp_assert_type(ctx, sexp_contextp, SEXP_CONTEXT, thread);
  if (sexp_context_refuel(thread) <= 0) /* return true if already terminated */ {
    return SEXP_TRUE;
  }
  sexp_context_timeoutp(ctx) = 0;
  sexp_context_waitp(ctx) = 1;
  sexp_context_event(ctx) = thread;
  sexp_insert_timed(ctx, ctx, timeout);
  return SEXP_FALSE;
}

sexp sexp_thread_sleep (sexp ctx sexp_api_params(self, n), sexp timeout) {
  sexp_context_waitp(ctx) = 1;
  if (timeout != SEXP_TRUE) {
    sexp_assert_type(ctx, sexp_numberp, SEXP_NUMBER, timeout);
    sexp_insert_timed(ctx, ctx, timeout);
  }
  return SEXP_FALSE;
}

/**************************** mutexes *************************************/

sexp sexp_mutex_state (sexp ctx sexp_api_params(self, n), sexp mutex) {
  /* sexp_assert_type(ctx, sexp_mutexp, mutex_id, timeout); */
  if (sexp_truep(sexp_mutex_lockp(mutex))) {
    if (sexp_contextp(sexp_mutex_thread(mutex)))
      return sexp_mutex_thread(mutex);
    else
      return sexp_intern(ctx, "not-owned", -1);
  } else {
    return sexp_intern(ctx, (sexp_mutex_thread(mutex) ? "not-abandoned" : "abandoned"), -1);
  }
}

sexp sexp_mutex_lock (sexp ctx sexp_api_params(self, n), sexp mutex, sexp timeout, sexp thread) {
  if (thread == SEXP_TRUE)
    thread = ctx;
  if (sexp_not(sexp_mutex_lockp(mutex))) {
    sexp_mutex_lockp(mutex) = SEXP_TRUE;
    sexp_mutex_thread(mutex) = thread;
    return SEXP_TRUE;
  } else {
    sexp_context_waitp(ctx) = 1;
    sexp_context_event(ctx) = mutex;
    sexp_insert_timed(ctx, ctx, timeout);
    return SEXP_FALSE;
  }
}

sexp sexp_mutex_unlock (sexp ctx sexp_api_params(self, n), sexp mutex, sexp condvar, sexp timeout) {
  sexp ls1, ls2;
  if (sexp_not(condvar)) {
    /* normal unlock - always succeeds, just need to unblock threads */
    if (sexp_truep(sexp_mutex_lockp(mutex))) {
      sexp_mutex_lockp(mutex) = SEXP_FALSE;
      sexp_mutex_thread(mutex) = ctx;
      /* search for threads blocked on this mutex */
      for (ls1=SEXP_NULL, ls2=sexp_global(ctx, SEXP_G_THREADS_PAUSED);
           sexp_pairp(ls2); ls1=ls2, ls2=sexp_cdr(ls2))
        if (sexp_context_event(sexp_car(ls2)) == mutex) {
          if (ls1==SEXP_NULL)
            sexp_global(ctx, SEXP_G_THREADS_PAUSED) = sexp_cdr(ls2);
          else
            sexp_cdr(ls1) = sexp_cdr(ls2);
          sexp_cdr(ls2) = sexp_global(ctx, SEXP_G_THREADS_FRONT);
          sexp_global(ctx, SEXP_G_THREADS_FRONT) = ls2;
          if (! sexp_pairp(sexp_cdr(ls2)))
            sexp_global(ctx, SEXP_G_THREADS_BACK) = ls2;
          sexp_context_waitp(sexp_car(ls2))
            = sexp_context_timeoutp(sexp_car(ls2)) = 0;
          break;
        }
    }
    return SEXP_TRUE;
  } else {
    /* wait on condition var */
    sexp_context_waitp(ctx) = 1;
    sexp_context_event(ctx) = condvar;
    sexp_insert_timed(ctx, ctx, timeout);
    return SEXP_FALSE;
  }
}

/**************************** condition variables *************************/

sexp sexp_condition_variable_signal (sexp ctx sexp_api_params(self, n), sexp condvar) {
  sexp ls1=SEXP_NULL, ls2=sexp_global(ctx, SEXP_G_THREADS_PAUSED);
  for ( ; sexp_pairp(ls2); ls1=ls2, ls2=sexp_cdr(ls2))
    if (sexp_context_event(sexp_car(ls2)) == condvar) {
      if (ls1==SEXP_NULL)
	sexp_global(ctx, SEXP_G_THREADS_PAUSED) = sexp_cdr(ls2);
      else
	sexp_cdr(ls1) = sexp_cdr(ls2);
      sexp_cdr(ls2) = sexp_global(ctx, SEXP_G_THREADS_FRONT);
      sexp_global(ctx, SEXP_G_THREADS_FRONT) = ls2;
      if (! sexp_pairp(sexp_cdr(ls2)))
	sexp_global(ctx, SEXP_G_THREADS_BACK) = ls2;
      sexp_context_waitp(sexp_car(ls2)) = sexp_context_timeoutp(sexp_car(ls2)) = 0;
      return SEXP_TRUE;
    }
  return SEXP_FALSE;
}

sexp sexp_condition_variable_broadcast (sexp ctx sexp_api_params(self, n), sexp condvar) {
  sexp res = SEXP_FALSE;
  while (sexp_truep(sexp_condition_variable_signal(ctx, self, n, condvar)))
    res = SEXP_TRUE;
  return res;
}

/**************************** the scheduler *******************************/

void sexp_wait_on_single_thread (sexp ctx) {
  struct timeval tval;
  useconds_t usecs = 0;
  gettimeofday(&tval, NULL);
  if (tval.tv_sec < sexp_context_timeval(ctx).tv_sec)
    usecs = (sexp_context_timeval(ctx).tv_sec - tval.tv_sec) * 1000000;
  if (tval.tv_usec < sexp_context_timeval(ctx).tv_usec)
    usecs += sexp_context_timeval(ctx).tv_usec - tval.tv_usec;
  usleep(usecs);
}

static const sexp_uint_t sexp_log2_lookup[32] = {
  0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8, 
  31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
};

/* only works on powers of two */
static sexp_uint_t sexp_log2_of_pow2 (sexp_uint_t n) {
  return sexp_log2_lookup[(n * 0x077CB531U) >> 27];
}

static sexp sexp_pop_signal (sexp ctx sexp_api_params(self, n)) {
  int allsigs, restsigs, signum;
  if (sexp_global(ctx, SEXP_G_THREADS_SIGNALS) == SEXP_ZERO) {
    return SEXP_FALSE;
  } else {
    allsigs = sexp_unbox_fixnum(sexp_global(ctx, SEXP_G_THREADS_SIGNALS));
    restsigs = allsigs & (allsigs-1);
    sexp_global(ctx, SEXP_G_THREADS_SIGNALS) = sexp_make_fixnum(restsigs);
    signum = sexp_log2_of_pow2(allsigs-restsigs);
    return sexp_make_fixnum(signum);
  }
}

static sexp sexp_get_signal_handler (sexp ctx sexp_api_params(self, n), sexp signum) {
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, signum);
  return sexp_vector_ref(sexp_global(ctx, SEXP_G_SIGNAL_HANDLERS), signum);
}

static sexp sexp_blocker (sexp ctx sexp_api_params(self, n), sexp port) {
  return SEXP_VOID;
}

sexp sexp_scheduler (sexp ctx sexp_api_params(self, n), sexp root_thread) {
  struct timeval tval;
  sexp res, ls1, ls2, runner, paused, front;
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(ctx, tmp);

  front  = sexp_global(ctx, SEXP_G_THREADS_FRONT);
  paused = sexp_global(ctx, SEXP_G_THREADS_PAUSED);

  /* check signals */
  if (sexp_global(ctx, SEXP_G_THREADS_SIGNALS) != SEXP_ZERO) {
    runner = sexp_global(ctx, SEXP_G_THREADS_SIGNAL_RUNNER);
    if (! sexp_contextp(runner)) { /* ensure the runner exists */
      if (sexp_envp(runner)) {
        tmp = sexp_env_cell(runner, (tmp=sexp_intern(ctx, "signal-runner", -1)));
        if (sexp_pairp(tmp) && sexp_procedurep(sexp_cdr(tmp))) {
          runner = sexp_make_thread(ctx, self, 2, sexp_cdr(tmp), SEXP_FALSE);
          sexp_global(ctx, SEXP_G_THREADS_SIGNAL_RUNNER) = runner;
          sexp_thread_start(ctx, self, 1, runner);
        }
      }
    } else if (sexp_context_waitp(runner)) { /* wake it if it's sleeping */
      sexp_context_waitp(runner) = 0;
      sexp_thread_start(ctx, self, 1, runner);
    }
  }

  /* check blocked fds */
  /* if () { */
  /* } */

  /* if we've terminated, check threads joining us */
  if (sexp_context_refuel(ctx) <= 0) {
    for (ls1=SEXP_NULL, ls2=paused; sexp_pairp(ls2); ) {
      if (sexp_context_event(sexp_car(ls2)) == ctx) {
        sexp_context_waitp(sexp_car(ls2)) = 0;
        sexp_context_timeoutp(sexp_car(ls2)) = 0;
        if (ls1==SEXP_NULL)
          sexp_global(ctx, SEXP_G_THREADS_PAUSED) = paused = sexp_cdr(ls2);
        else
          sexp_cdr(ls1) = sexp_cdr(ls2);
        tmp = sexp_cdr(ls2);
        sexp_cdr(ls2) = SEXP_NULL;
	if (! sexp_pairp(sexp_global(ctx, SEXP_G_THREADS_BACK))) {
	  sexp_global(ctx, SEXP_G_THREADS_FRONT) = front = ls2;
	} else {
	  sexp_cdr(sexp_global(ctx, SEXP_G_THREADS_BACK)) = ls2;
	}
	sexp_global(ctx, SEXP_G_THREADS_BACK) = ls2;
        ls2 = tmp;
      } else {
        ls1 = ls2;
        ls2 = sexp_cdr(ls2);
      }
    }
  }

  /* check timeouts */
  if (sexp_pairp(paused)) {
    if (gettimeofday(&tval, NULL) == 0) {
      ls1 = SEXP_NULL;
      ls2 = paused;
      while (sexp_pairp(ls2) && sexp_context_before(sexp_car(ls2), tval)) {
        sexp_context_timeoutp(sexp_car(ls2)) = 1;
        sexp_context_waitp(ctx) = 0;
        ls1 = ls2;
        ls2 = sexp_cdr(ls2);
      }
      if (sexp_pairp(ls1)) {
        sexp_cdr(ls1) = SEXP_NULL;
	if (! sexp_pairp(sexp_global(ctx, SEXP_G_THREADS_BACK))) {
	  sexp_global(ctx, SEXP_G_THREADS_FRONT) = front = paused;
	} else {
	  sexp_cdr(sexp_global(ctx, SEXP_G_THREADS_BACK)) = paused;
	}
	sexp_global(ctx, SEXP_G_THREADS_BACK) = ls1;
        sexp_global(ctx, SEXP_G_THREADS_PAUSED) = paused = ls2;
      }
    }
  }

  /* dequeue next thread */
  if (sexp_pairp(front)) {
    res = sexp_car(front);
    if ((sexp_context_refuel(ctx) <= 0) || sexp_context_waitp(ctx)) {
      /* either terminated or paused */
      sexp_global(ctx, SEXP_G_THREADS_FRONT) = sexp_cdr(front);
      if (! sexp_pairp(sexp_cdr(front)))
        sexp_global(ctx, SEXP_G_THREADS_BACK) = SEXP_NULL;
    } else {
      /* swap with front of queue */
      sexp_car(sexp_global(ctx, SEXP_G_THREADS_FRONT)) = ctx;
      /* rotate front of queue to back */
      sexp_cdr(sexp_global(ctx, SEXP_G_THREADS_BACK))
        = sexp_global(ctx, SEXP_G_THREADS_FRONT);
      sexp_global(ctx, SEXP_G_THREADS_FRONT)
        = sexp_cdr(sexp_global(ctx, SEXP_G_THREADS_FRONT));
      sexp_global(ctx, SEXP_G_THREADS_BACK)
        = sexp_cdr(sexp_global(ctx, SEXP_G_THREADS_BACK));
      sexp_cdr(sexp_global(ctx, SEXP_G_THREADS_BACK)) = SEXP_NULL;
    }
  } else {
    res = ctx;
  }

  if (sexp_context_waitp(res)) {
    /* the only thread available was waiting */
    sexp_wait_on_single_thread(res);
    sexp_context_timeoutp(res) = 1;
    sexp_context_waitp(res) = 0;
  }

  sexp_gc_release1(ctx);
  return res;
}

/**************************************************************************/

sexp sexp_init_library (sexp ctx sexp_api_params(self, n), sexp env) {

  sexp_define_type_predicate(ctx, env, "thread?", SEXP_CONTEXT);
  sexp_define_foreign(ctx, env, "thread-timeout?", 0, sexp_thread_timeoutp);
  sexp_define_foreign(ctx, env, "current-thread", 0, sexp_current_thread);
  sexp_define_foreign_opt(ctx, env, "make-thread", 2, sexp_make_thread, SEXP_FALSE);
  sexp_define_foreign(ctx, env, "thread-start!", 1, sexp_thread_start);
  sexp_define_foreign(ctx, env, "%thread-terminate!", 1, sexp_thread_terminate);
  sexp_define_foreign(ctx, env, "%thread-join!", 2, sexp_thread_join);
  sexp_define_foreign(ctx, env, "%thread-sleep!", 1, sexp_thread_sleep);
  sexp_define_foreign(ctx, env, "thread-name", 1, sexp_thread_name);
  sexp_define_foreign(ctx, env, "thread-specific", 1, sexp_thread_specific);
  sexp_define_foreign(ctx, env, "thread-specific-set!", 2, sexp_thread_specific_set);
  sexp_define_foreign(ctx, env, "mutex-state", 1, sexp_mutex_state);
  sexp_define_foreign(ctx, env, "%mutex-lock!", 3, sexp_mutex_lock);
  sexp_define_foreign(ctx, env, "%mutex-unlock!", 3, sexp_mutex_unlock);
  sexp_define_foreign(ctx, env, "condition-variable-signal!", 1, sexp_condition_variable_signal);
  sexp_define_foreign(ctx, env, "condition-variable-broadcast!", 1, sexp_condition_variable_broadcast);
  sexp_define_foreign(ctx, env, "pop-signal!", 0, sexp_pop_signal);
  sexp_define_foreign(ctx, env, "get-signal-handler", 1, sexp_get_signal_handler);

  sexp_global(ctx, SEXP_G_THREADS_SCHEDULER)
    = sexp_make_foreign(ctx, "scheduler", 1, 0, (sexp_proc1)sexp_scheduler, SEXP_FALSE);
  sexp_global(ctx, SEXP_G_THREADS_BLOCKER)
    = sexp_make_foreign(ctx, "blocker", 1, 0, (sexp_proc1)sexp_blocker, SEXP_FALSE);

  /* remember the env to lookup the runner later */
  sexp_global(ctx, SEXP_G_THREADS_SIGNAL_RUNNER) = env;

  return SEXP_VOID;
}

