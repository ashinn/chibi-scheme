/* time.c -- R7RS time routines                              */
/* Copyright (c) 2011-2012 Alex Shinn.  All rights reserved. */
/* Copyright (c) 2012 Alan Watson. All rights reserved.      */
/* BSD-style license: http://synthcode.com/license.txt       */

#include <chibi/eval.h>

#ifndef PLAN9
#include <sys/time.h>
#else
typedef long time_t;
#endif

#if SEXP_USE_NTP_GETTIME
#include <sys/timex.h>

/* We can determine the clock resolution by calling ntp_adjtime() and */
/* seeing if the STA_NANO bit of the status word is set. If it is, we */
/* have nanosecond resolution, otherwise we have microsecond resolution. */

/* The time member of the ntptimeval struct may be either a struct */
/* timeval (with the fraction in microseconds) or a struct timespec */
/* (with the fraction in nanoseconds). */

/* However, there are systems (e.g., Ubuntu 10.4 on X86_64) that use */
/* nanosecond resolution but still declare the time member of struct */
/* ntptimeval to be a struct timeval. Therefore, we explicitly use casts */
/* to access this member either as a struct timeval or struct timespec */
/* depending on the resolution. */

static double ntp_resolution = 0.0;

static void determine_ntp_resolution (void) {
  struct timex tx;
  tx.modes = 0;
  if (ntp_adjtime(&tx) < 0) {
    ntp_resolution = 0;
  } else if (tx.status & STA_NANO) {
    ntp_resolution = 1e-9;
  } else {
    ntp_resolution = 1e-6;
  }
}

static void current_ntp_clock_values (double *second, int *leap_second_indicator) {
  struct ntptimeval ntv;
  int status = ntp_gettime(&ntv);
  if (ntp_resolution != 0 && (
        status == TIME_OK  || 
        status == TIME_INS || 
        status == TIME_DEL || 
        status == TIME_OOP || 
        status == TIME_WAIT)) {
    if (ntp_resolution == 1e-6) {
      struct timeval *tv = (struct timeval *) &ntv.time;
      *second = tv->tv_sec + ntp_resolution * tv->tv_usec;
    } else {
      struct timespec *ts = (struct timespec *) &ntv.time;
      *second = ts->tv_sec + ntp_resolution * ts->tv_nsec;
    }
    *leap_second_indicator = (status == TIME_OOP);
  } else {
    *second = current_clock_second();
    *leap_second_indicator = 0;
  }
}

static sexp sexp_current_ntp_clock_values (sexp ctx, sexp self, sexp_sint_t n) {
  double second;
  int leap_second_indicator;
  sexp_gc_var3(res, car, cdr);
  current_ntp_clock_values (&second, &leap_second_indicator);
  sexp_gc_preserve3(ctx, res, car, cdr);
  cdr = sexp_make_boolean(leap_second_indicator);
  car = sexp_make_flonum(ctx, second);
  res = sexp_cons(ctx, car, cdr);
  sexp_gc_release3(ctx);
  return res;
}

#endif  /* def SEXP_USE_NTP_GETTIME */

static sexp sexp_current_clock_second (sexp ctx, sexp self, sexp_sint_t n) {
#ifndef PLAN9
  struct timeval tv;
  struct timezone tz;
  if (gettimeofday(&tv, &tz))
    return sexp_user_exception(ctx, self, "couldn't gettimeofday", SEXP_FALSE);
  return sexp_make_flonum(ctx, tv.tv_sec + tv.tv_usec / 1000000.0);
#else
  time_t res = time(NULL);
  return sexp_make_flonum(ctx, res);
#endif
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return sexp_global(ctx, SEXP_G_ABI_ERROR);
  sexp_define_foreign(ctx, env, "current-clock-second", 0, sexp_current_clock_second);
#if SEXP_USE_NTP_GETTIME
  determine_ntp_resolution();
  sexp_define_foreign(ctx, env, "current-ntp-clock-values", 0, sexp_current_ntp_clock_values);
#endif
  return SEXP_VOID;
}
