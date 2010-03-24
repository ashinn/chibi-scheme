/*  features.h -- general feature configuration               */
/*  Copyright (c) 2009-2010 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

/* uncomment this to disable most features */
/*   Most features are enabled by default, but setting this */
/*   option will disable any not explicitly enabled. */
/* #define SEXP_USE_NO_FEATURES 1 */

/* uncomment this to disable the module system */
/*   Currently this just loads the config.scm from main and */
/*   sets up an (import (module name)) macro. */
/* #define SEXP_USE_MODULES 0 */

/* uncomment this to disable dynamic loading */
/*   If enabled, you can LOAD .so files with a */
/*   sexp_init_library(ctx, env) function provided. */
/* #define SEXP_USE_DL 0 */

/* uncomment this to statically compile all C libs */
/*   If set, this will statically include the clibs.c file */
/*   into the standard environment, so that you can have */
/*   access to a predefined set of C libraries without */
/*   needing dynamic loading.  The clibs.c file is generated */
/*   automatically by searching the lib directory for */
/*   modules with include-shared, but can be hand-tailored */
/*   to your needs. */
/* #define SEXP_USE_STATIC_LIBS 1 */

/* uncomment this to disable a simplifying optimization pass */
/*   This performs some simple optimizations such as dead-code */
/*   elimination, constant-folding, and directly propagating */
/*   non-mutated let values bound to constants or non-mutated */
/*   references.  More than performance, this is aimed at reducing the */
/*   size of the compiled code, especially as the result of macro */
/*   expansions, so it's a good idea to leave it enabled. */
/* #define SEXP_USE_SIMPLIFY 0 */

/* uncomment this to disable dynamic type definitions */
/*   This enables register-simple-type and related */
/*   opcodes for defining types, needed by the default */
/*   implementation of (srfi 9). */
/* #define SEXP_USE_TYPE_DEFS 0 */

/* uncomment this to use the Boehm conservative GC */
/*   Conservative GCs make it easier to write extensions, */
/*   since you don't have to keep track of intermediate */
/*   variables, but can leak memory.  Boehm is also a */
/*   very large library to link in.  You may want to */
/*   enable this when debugging your own extensions, or */
/*   if you suspect a bug in the native GC. */
/* #define SEXP_USE_BOEHM 1 */

/* uncomment this to just malloc manually instead of any GC */
/*   Mostly for debugging purposes, this is the no GC option. */
/*   You can use just the read/write API and */
/*   explicitly free sexps, though. */
/* #define SEXP_USE_MALLOC 1 */

/* uncomment this to allocate heaps with mmap instead of malloc */
/* #define SEXP_USE_MMAP_GC 1 */

/* uncomment this to add conservative checks to the native GC */
/*   Please mail the author if enabling this makes a bug */
/*   go away and you're not working on your own C extension. */
/* #define SEXP_USE_DEBUG_GC 1 */

/* uncomment this to make the heap common to all contexts */
/*   By default separate contexts can have separate heaps, */
/*   and are thus thread-safe and independant. */
/* #define SEXP_USE_GLOBAL_HEAP 1 */

/* uncomment this to make type definitions common to all contexts */
/*   By default types are only global if you don't allow user type */
/*   definitions, so new types will be local to a given set of */
/*   contexts sharing thei heap. */
/* #define SEXP_USE_GLOBAL_TYPES 1 */

/* uncomment this to make the symbol table common to all contexts */
/*   Will still be restricted to all contexts sharing the same */
/*   heap, of course. */
/* #define SEXP_USE_GLOBAL_SYMBOLS 1 */

/* uncomment this if you don't need flonum support */
/*   This is only for EVAL - you'll still be able to read */
/*   and write flonums directly through the sexp API. */
/* #define SEXP_USE_FLONUMS 0 */

/* uncomment this to disable reading/writing IEEE infinities */
/*   By default you can read/write +inf.0, -inf.0 and +nan.0 */
/* #define SEXP_USE_INFINITIES 0 */

/* uncomment this if you want immediate flonums */
/*   This is experimental, enable at your own risk. */
/* #define SEXP_USE_IMMEDIATE_FLONUMS 1 */

/* uncomment this if you don't want bignum support */
/*   Bignums are implemented with a small, custom library  */
/*   in opt/bignum.c. */
/* #define SEXP_USE_BIGNUMS 0 */

/* uncomment this if you don't need extended math operations */
/*   This includes the trigonometric and expt functions. */
/*   Automatically disabled if you've disabled flonums. */
/* #define SEXP_USE_MATH 0 */

/* uncomment this to disable the self and n parameters to primitives */
/*   This is the old style API. */
/* #define SEXP_USE_SELF_PARAMETER 0 */

/* uncomment this to disable warning about references to undefined variables */
/*   This is something of a hack, but can be quite useful. */
/*   It's very fast and doesn't involve any separate analysis */
/*   passes. */
/* #define SEXP_USE_WARN_UNDEFS 0 */

/* uncomment this to disable huffman-coded immediate symbols */
/*   By default (this may change) small symbols are represented */
/*   as immediates using a simple huffman encoding.  This keeps */
/*   the symbol table small, and minimizes hashing when doing a */
/*   lot of reading. */
/* #define SEXP_USE_HUFF_SYMS 0 */

/* uncomment this to just use a single list for hash tables */
/*   You can trade off some space in exchange for longer read */
/*   times by disabling hashing and just putting all */
/*   non-immediate symbols in a single list. */
/* #define SEXP_USE_HASH_SYMS 0 */

/* uncomment this to disable string ports */
/*   If disabled some basic functionality such as number->string */
/*   will not be available by default. */
/* #define SEXP_USE_STRING_STREAMS 0 */

/* uncomment this to disable automatic closing of ports */
/*   If enabled, the underlying FILE* for file ports will be */
/*   automatically closed when they're garbage collected.  Doesn't */
/*   apply to stdin/stdout/stderr. */
/* #define SEXP_USE_AUTOCLOSE_PORTS 0 */

/* uncomment this to use the normal 1970 unix epoch */
/*   By default chibi uses an datetime epoch starting at */
/*   2010/01/01 00:00:00 in order to be able to represent */
/*   more common times as fixnums. */
/* #define SEXP_USE_2010_EPOCH 0 */

/* uncomment this to disable stack overflow checks */
/*   By default stacks are fairly small, so it's good to leave */
/*   this enabled. */
/* #define SEXP_USE_CHECK_STACK 0 */

/* #define SEXP_USE_DEBUG_VM 0 */
/*   Experts only. */
/*   For *very* verbose output on every VM operation. */

/* uncomment this to make the VM adhere to alignment rules */
/*   This is required on some platforms, e.g. ARM */
/* #define SEXP_USE_ALIGNED_BYTECODE */

/************************************************************************/
/* These settings are configurable but only recommended for */
/* experienced users, and only apply when using the native GC.  */
/************************************************************************/

/* the initial heap size in bytes */
#ifndef SEXP_INITIAL_HEAP_SIZE
#define SEXP_INITIAL_HEAP_SIZE (2*1024*1024)
#endif

/* the maximum heap size in bytes - if 0 there is no limit */
#ifndef SEXP_MAXIMUM_HEAP_SIZE
#define SEXP_MAXIMUM_HEAP_SIZE 0
#endif
#ifndef SEXP_MINIMUM_HEAP_SIZE
#define SEXP_MINIMUM_HEAP_SIZE 512*1024
#endif

/* if after GC more than this percentage of memory is still in use, */
/* and we've not exceeded the maximum size, grow the heap */
#ifndef SEXP_GROW_HEAP_RATIO
#define SEXP_GROW_HEAP_RATIO 0.75
#endif

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

#ifndef SEXP_USE_NO_FEATURES
#define SEXP_USE_NO_FEATURES 0
#endif

#ifndef SEXP_USE_MODULES
#define SEXP_USE_MODULES ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_TYPE_DEFS
#define SEXP_USE_TYPE_DEFS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_MAXIMUM_TYPES
#define SEXP_MAXIMUM_TYPES ((sexp_tag_t)-1)
#endif

#ifndef SEXP_USE_DL
#ifdef PLAN9
#define SEXP_USE_DL 0
#else
#define SEXP_USE_DL ! SEXP_USE_NO_FEATURES
#endif
#endif

#ifndef SEXP_USE_STATIC_LIBS
#define SEXP_USE_STATIC_LIBS 0
#endif

#ifndef SEXP_USE_SIMPLIFY
#define SEXP_USE_SIMPLIFY ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_BOEHM
#define SEXP_USE_BOEHM 0
#endif

#ifndef SEXP_USE_MALLOC
#define SEXP_USE_MALLOC 0
#endif

#ifndef SEXP_USE_MMAP_GC
#define SEXP_USE_MMAP_GC 0
#endif

#ifndef SEXP_USE_DEBUG_GC
#define SEXP_USE_DEBUG_GC 0
#endif

#ifndef SEXP_USE_GLOBAL_HEAP
#if SEXP_USE_BOEHM || SEXP_USE_MALLOC
#define SEXP_USE_GLOBAL_HEAP 1
#else
#define SEXP_USE_GLOBAL_HEAP 0
#endif
#endif

#ifndef SEXP_USE_GLOBAL_TYPES
#define SEXP_USE_GLOBAL_TYPES (! SEXP_USE_TYPE_DEFS)
#endif

#ifndef SEXP_USE_GLOBAL_SYMBOLS
#if SEXP_USE_BOEHM || SEXP_USE_MALLOC
#define SEXP_USE_GLOBAL_SYMBOLS 1
#else
#define SEXP_USE_GLOBAL_SYMBOLS 0
#endif
#endif

#ifndef SEXP_USE_FLONUMS
#define SEXP_USE_FLONUMS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_INFINITIES
#if defined(PLAN9) || ! SEXP_USE_FLONUMS
#define SEXP_USE_INFINITIES 0
#else
#define SEXP_USE_INFINITIES ! SEXP_USE_NO_FEATURES
#endif
#endif

#ifndef SEXP_USE_IMMEDIATE_FLONUMS
#define SEXP_USE_IMMEDIATE_FLONUMS 0
#endif

#ifndef SEXP_USE_BIGNUMS
#define SEXP_USE_BIGNUMS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_MATH
#define SEXP_USE_MATH SEXP_USE_FLONUMS && ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_SELF_PARAMETER
#define SEXP_USE_SELF_PARAMETER 1
#endif

#ifndef SEXP_USE_WARN_UNDEFS
#define SEXP_USE_WARN_UNDEFS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_HUFF_SYMS
#define SEXP_USE_HUFF_SYMS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_HASH_SYMS
#define SEXP_USE_HASH_SYMS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_DEBUG_VM
#define SEXP_USE_DEBUG_VM 0
#endif

#ifndef SEXP_USE_STRING_STREAMS
#define SEXP_USE_STRING_STREAMS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_AUTOCLOSE_PORTS
#define SEXP_USE_AUTOCLOSE_PORTS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_2010_EPOCH
#define SEXP_USE_2010_EPOCH ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_EPOCH_OFFSET
#if SEXP_USE_2010_EPOCH
#define SEXP_EPOCH_OFFSET 1262271600
#else
#define SEXP_EPOCH_OFFSET 0
#endif
#endif

#ifndef SEXP_USE_CHECK_STACK
#define SEXP_USE_CHECK_STACK ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_ALIGNED_BYTECODE
#if defined(__arm__)
#define SEXP_USE_ALIGNED_BYTECODE 1
#else
#define SEXP_USE_ALIGNED_BYTECODE 0
#endif
#endif

#ifdef PLAN9
#define strcasecmp cistrcmp
#define strncasecmp cistrncmp
#define round(x) floor((x)+0.5)
#define trunc(x) floor((x)+0.5*(((x)<0)?1:0))
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
