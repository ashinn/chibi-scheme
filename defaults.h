/*  defaults.h -- defaults for unspecified configs       */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#if HAVE_ERR_H
#include <err.h>
#else
/* requires msg be a string literal, and at least one argument */
#define errx(code, msg, ...) (fprintf(stderr,msg"\n",__VA_ARGS__), exit(code))
#endif

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__) || defined(__OpenBSD__)
#define SEXP_BSD 1
#else
#define SEXP_BSD 0
#define _GNU_SOURCE
#endif

#ifndef USE_BOEHM
#define USE_BOEHM 0
#endif

#ifndef USE_MALLOC
#define USE_MALLOC 0
#endif

#ifndef USE_DEBUG_GC
#define USE_DEBUG_GC 0
#endif

#ifndef USE_FLONUMS
#define USE_FLONUMS 1
#endif

#ifndef USE_MATH
#define USE_MATH 1
#endif

#ifndef USE_WARN_UNDEFS
#define USE_WARN_UNDEFS 1
#endif

#ifndef USE_HUFF_SYMS
#define USE_HUFF_SYMS 1
#endif

#ifndef USE_HASH_SYMS
#define USE_HASH_SYMS 1
#endif

#ifndef USE_DEBUG
#define USE_DEBUG 1
#endif

#ifndef USE_STRING_STREAMS
#define USE_STRING_STREAMS 1
#endif

#ifndef USE_FAST_LET
#define USE_FAST_LET 1
#endif

#ifndef USE_CHECK_STACK
#define USE_CHECK_STACK 0
#endif

