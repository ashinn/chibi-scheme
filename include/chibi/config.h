/*  config.h -- general configuration                    */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

/* uncomment this to disable the module system */
/*   Currently this just loads the config.scm from main and */
/*   sets up an (import (module name)) macro. */
/* #define USE_MODULES 0 */

/* uncomment this to disable dynamic loading */
/*   If enabled, you can LOAD .so files with a */
/*   sexp_init_library(ctx, env) function provided. */
/* #define USE_DL 0 */

/* uncomment this to disable a simplifying optimization pass */
/* #define USE_SIMPLIFY 0 */

/* uncomment this to disable dynamic type definitions */
/*   This enables register-simple-type and related */
/*   opcodes for defining types, needed by the default */
/*   implementation of (srfi 9). */
/* #define USE_TYPE_DEFS 0 */

/* uncomment this to use the Boehm conservative GC */
/*   Conservative GCs make it easier to write extensions, */
/*   since you don't have to keep track of intermediate */
/*   variables, but can leak memory.  Boehm is also a */
/*   very large library to link in.  You may want to */
/*   enable this when debugging your own extensions, or */
/*   if you suspect a bug in the native GC. */
/* #define USE_BOEHM 1 */

/* uncomment this to just malloc manually instead of any GC */
/*   Mostly for debugging purposes, this is the no GC option. */
/*   You can use just the read/write API and */
/*   explicitly free sexps, though. */
/* #define USE_MALLOC 1 */

/* uncomment this to add conservative checks to the native GC */
/*   Please mail the author if enabling this makes a bug */
/*   go away and you're not working on your own C extension. */
/* #define USE_DEBUG_GC 1 */

/* uncomment this to make the heap common to all contexts */
/*   By default separate contexts can have separate heaps, */
/*   and are thus thread-safe and independant. */
/* #define USE_GLOBAL_HEAP 1 */

/* uncomment this to make the symbol table common to all contexts */
/*   Will still be restricted to all contexts sharing the same */
/*   heap, of course. */
/* #define USE_GLOBAL_SYMBOLS 1 */

/* uncomment this if you don't need flonum support */
/*   This is only for EVAL - you'll still be able to read */
/*   and write flonums directly through the sexp API. */
/* #define USE_FLONUMS 0 */

/* uncomment this to disable reading/writing IEEE infinities */
/*   By default you can read/write +inf.0, -inf.0 and +nan.0 */
/* #define USE_INFINITIES 0 */

/* uncomment this if you want immediate flonums */
/*   This is experimental, enable at your own risk. */
/* #define USE_IMMEDIATE_FLONUMS 1 */

/* uncomment this if you don't want bignum support */
/*   Bignums are implemented with a small, custom library  */
/*   in opt/bignum.c. */
/* #define USE_BIGNUMS 0 */

/* uncomment this if you don't need extended math operations */
/*   This includes the trigonometric and expt functions. */
/*   Automatically disabled if you've disabled flonums. */
/* #define USE_MATH 0 */

/* uncomment this to disable warning about references to undefined variables */
/*   This is something of a hack, but can be quite useful. */
/*   It's very fast and doesn't involve any separate analysis */
/*   passes. */
/* #define USE_WARN_UNDEFS 0 */

/* uncomment this to disable huffman-coded immediate symbols */
/*   By default (this may change) small symbols are represented */
/*   as immediates using a simple huffman encoding.  This keeps */
/*   the symbol table small, and minimizes hashing when doing a */
/*   lot of reading. */
/* #define USE_HUFF_SYMS 0 */

/* uncomment this to just use a single list for hash tables */
/*   You can trade off some space in exchange for longer read */
/*   times by disabling hashing and just putting all */
/*   non-immediate symbols in a single list. */
/* #define USE_HASH_SYMS 0 */

/* uncomment this to disable string ports */
/*   If disabled some basic functionality such as number->string */
/*   will not be available by default. */
/* #define USE_STRING_STREAMS 0 */

/* uncomment this to disable automatic closing of ports */
/*   If enabled, the underlying FILE* for file ports will be */
/*   automatically closed when they're garbage collected.  Doesn't */
/*   apply to stdin/stdout/stderr. */
/* #define USE_AUTOCLOSE_PORTS 0 */

/* uncomment this to use the normal 1970 unix epoch */
/*   By default chibi uses an datetime epoch starting at */
/*   2010/01/01 00:00:00 in order to be able to represent */
/*   more common times as fixnums. */
/* #define USE_2010_EPOCH 0 */

/* uncomment this to disable stack overflow checks */
/*   By default stacks are fairly small, so it's good to leave */
/*   this enabled. */
/* #define USE_CHECK_STACK 0 */

/* uncomment this to disable debugging utilities */
/*   By default there's a `disasm' procedure you can use to */
/*   view the compiled VM instructions of a procedure.  You can */
/*   disable this if you don't need it. */
/* #define USE_DEBUG 0 */

/* #define USE_DEBUG_VM 0 */
/*   Experts only. */
/*   For *very* verbose output on every VM operation. */

/************************************************************************/
/*         DEFAULTS - DO NOT MODIFY ANYTHING BELOW THIS LINE            */
/************************************************************************/

#ifndef SEXP_64_BIT
#if defined(__amd64) || defined(__x86_64)
#define SEXP_64_BIT 1
#else
#define SEXP_64_BIT 0
#endif
#endif

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__) || defined(__OpenBSD__)
#define SEXP_BSD 1
#else
#define SEXP_BSD 0
#define _GNU_SOURCE
#endif

#ifndef USE_MODULES
#define USE_MODULES 1
#endif

#ifndef USE_TYPE_DEFS
#define USE_TYPE_DEFS 1
#endif

#ifndef SEXP_MAXIMUM_TYPES
#define SEXP_MAXIMUM_TYPES ((sexp_tag_t)-1)
#endif

#ifndef USE_DL
#ifdef PLAN9
#define USE_DL 0
#else
#define USE_DL 1
#endif
#endif

#ifndef USE_SIMPLIFY
#define USE_SIMPLIFY 1
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

#ifndef USE_GLOBAL_HEAP
#if USE_BOEHM || USE_MALLOC
#define USE_GLOBAL_HEAP 1
#else
#define USE_GLOBAL_HEAP 0
#endif
#endif

#ifndef USE_GLOBAL_SYMBOLS
#if USE_BOEHM || USE_MALLOC
#define USE_GLOBAL_SYMBOLS 1
#else
#define USE_GLOBAL_SYMBOLS 0
#endif
#endif

#ifndef USE_FLONUMS
#define USE_FLONUMS 1
#endif

#ifndef USE_INFINITIES
#if defined(PLAN9) || ! USE_FLONUMS
#define USE_INFINITIES 0
#else
#define USE_INFINITIES 1
#endif
#endif

#ifndef USE_IMMEDIATE_FLONUMS
#define USE_IMMEDIATE_FLONUMS 0
#endif

#ifndef USE_BIGNUMS
#define USE_BIGNUMS 1
#endif

#ifndef USE_MATH
#define USE_MATH USE_FLONUMS
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

#ifndef USE_DEBUG_VM
#define USE_DEBUG_VM 0
#endif

#ifndef USE_STRING_STREAMS
#define USE_STRING_STREAMS 1
#endif

#ifndef USE_AUTOCLOSE_PORTS
#define USE_AUTOCLOSE_PORTS 1
#endif

#ifndef USE_2010_EPOCH
#define USE_2010_EPOCH 1
#endif

#ifndef SEXP_EPOCH_OFFSET
#if USE_2010_EPOCH
#define SEXP_EPOCH_OFFSET 1262271600
#else
#define SEXP_EPOCH_OFFSET 0
#endif
#endif

#ifndef USE_CHECK_STACK
#define USE_CHECK_STACK 1
#endif

#ifdef PLAN9

#define errx(code, msg, ...) exits(msg)
#define exit_normally() exits(NULL)
#define strcasecmp cistrcmp
#define strncasecmp cistrncmp
#define round(x) floor((x)+0.5)
#define trunc(x) floor((x)+0.5*(((x)<0)?1:0))

#else

#define exit_normally() exit(0)
#if HAVE_ERR_H
#include <err.h>
#else
/* requires msg be a string literal, and at least one argument */
#define errx(code, msg, ...) (fprintf(stderr,msg"\n",__VA_ARGS__), exit(code))
#endif

#endif

#ifdef __MINGW32__
#ifdef BUILDING_DLL
#define SEXP_API    __declspec(dllexport)
#else
#define SEXP_API    __declspec(dllimport)
#endif
#else
#define SEXP_API
#endif
