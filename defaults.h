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
#endif

#ifndef USE_BOEHM
#define USE_BOEHM 1
#endif

#ifndef USE_FLONUMS
#define USE_FLONUMS 1
#endif

#ifndef USE_HUFF_SYMS
#define USE_HUFF_SYMS 1
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

#if USE_BOEHM
#include "gc/include/gc.h"
#define SEXP_ALLOC        GC_malloc
#define SEXP_ALLOC_ATOMIC GC_malloc_atomic
#define SEXP_REALLOC      GC_realloc
#define SEXP_FREE         GC_free
#else
#define SEXP_ALLOC        malloc
#define SEXP_ALLOC_ATOMIC SEXP_ALLOC
#define SEXP_REALLOC      realloc
#define SEXP_FREE         free
#endif

