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
#define USE_BOEHM 1
#endif

#ifndef USE_MALLOC
#define USE_MALLOC 0
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

#if USE_BOEHM
#include "gc/include/gc.h"
#define sexp_alloc(ctx, size)        GC_malloc(size)
#define sexp_alloc_atomic(ctx, size) GC_malloc_atomic(size)
#define sexp_realloc(ctx, x, size)   GC_realloc(x, size)
#define sexp_free(ctx, x)
#define sexp_deep_free(ctx, x)
#elif USE_MALLOC
#define sexp_alloc(ctx, size)        malloc(size)
#define sexp_alloc_atomic(ctx, size) malloc(size)
#define sexp_realloc(ctx, x, size)   realloc(x, size)
#define sexp_free(ctx, x)            free(x)
void sexp_deep_free(sexp ctx, sexp obj);
#else  /* native gc */
void *sexp_alloc(sexp ctx, size_t size);
#define sexp_alloc_atomic            sexp_alloc
void *sexp_realloc(sexp ctx, sexp x, size_t size);
#define sexp_free(ctx, x)
#define sexp_deep_free(ctx, x)
#endif

