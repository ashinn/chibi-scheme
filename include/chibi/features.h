/*  features.h -- general feature configuration               */
/*  Copyright (c) 2009-2011 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

/* uncomment this to disable most features */
/*   Most features are enabled by default, but setting this */
/*   option will disable any not explicitly enabled. */
/* #define SEXP_USE_NO_FEATURES 1 */

/* uncomment this to disable interpreter-based threads */
/* #define SEXP_USE_GREEN_THREADS 0 */

/* uncomment this to enable the experimental native x86 backend */
/* #define SEXP_USE_NATIVE_X86 1 */

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

/* uncomment this to disable weak references */
/* #define SEXP_USE_WEAK_REFERENCES 0 */

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
/* #define SEXP_USE_CONSERVATIVE_GC 1 */

/* uncomment this to add additional native checks to only mark objects in the heap */
/* #define SEXP_USE_SAFE_GC_MARK 1 */

/* uncomment this to track what C source line each object is allocated from */
/* #define SEXP_USE_TRACK_ALLOC_SOURCE 1 */

/* uncomment this to add additional native gc checks to verify a magic header */
/* #define SEXP_USE_HEADER_MAGIC 1 */

/* uncomment this to add very verbose debugging stats to the native GC */
/* #define SEXP_USE_DEBUG_GC 1 */

/* uncomment this to make the heap common to all contexts */
/*   By default separate contexts can have separate heaps, */
/*   and are thus thread-safe and independant. */
/* #define SEXP_USE_GLOBAL_HEAP 1 */

/* uncomment this to make the symbol table common to all contexts */
/*   Will still be restricted to all contexts sharing the same */
/*   heap, of course. */
/* #define SEXP_USE_GLOBAL_SYMBOLS 1 */

/* uncomment this to disable foreign function bindings with > 6 args */
/* #define SEXP_USE_EXTENDED_FCALL 0 */

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

/* uncomment this if you don't want exact ratio support */
/*   Ratios are part of the bignum library and imply bignums. */
/* #define SEXP_USE_RATIOS 0 */

/* uncomment this if you don't want imaginary number support */
/* #define SEXP_USE_COMPLEX 0 */

/* uncomment this if you don't want 1## style approximate digits */
/* #define SEXP_USE_PLACEHOLDER_DIGITS 0 */

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

/* uncomment this to disable UTF-8 string support */
/*   The default settings store strings in memory as UTF-8, */
/*   and assumes strings passed to/from the C FFI are UTF-8.  */
/* #define SEXP_USE_UTF8_STRINGS 0 */

/* uncomment this to disable the string-set! opcode */
/*   By default (non-literal) strings are mutable. */
/*   Making them immutable allows for packed UTF-8 strings. */
/* #define SEXP_USE_MUTABLE_STRINGS 0 */

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
#define SEXP_MINIMUM_HEAP_SIZE 8*1024
#endif

/* if after GC more than this percentage of memory is still in use, */
/* and we've not exceeded the maximum size, grow the heap */
#ifndef SEXP_GROW_HEAP_RATIO
#define SEXP_GROW_HEAP_RATIO 0.75
#endif

/* the default number of opcodes to run each thread for */
#ifndef SEXP_DEFAULT_QUANTUM
#define SEXP_DEFAULT_QUANTUM 500
#endif

/************************************************************************/
/*         DEFAULTS - DO NOT MODIFY ANYTHING BELOW THIS LINE            */
/************************************************************************/

#ifndef SEXP_64_BIT
#if defined(__amd64) || defined(__x86_64) || defined(_WIN64) || defined(_Wp64)
#define SEXP_64_BIT 1
#else
#define SEXP_64_BIT 0
#endif
#endif

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__) || defined(__OpenBSD__)
#define SEXP_BSD 1
#else
#define SEXP_BSD 0
#if ! defined(_GNU_SOURCE) && ! defined(_WIN32) && ! defined(PLAN9)
#define _GNU_SOURCE
#endif
#endif

#ifndef SEXP_USE_NO_FEATURES
#define SEXP_USE_NO_FEATURES 0
#endif

#ifndef SEXP_USE_GREEN_THREADS
#define SEXP_USE_GREEN_THREADS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_AUTO_FORCE
#define SEXP_USE_AUTO_FORCE 0
#endif

#ifndef SEXP_USE_NATIVE_X86
#define SEXP_USE_NATIVE_X86 0
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
#if defined(PLAN9) || defined(_WIN32)
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

#ifndef SEXP_USE_WEAK_REFERENCES
#define SEXP_USE_WEAK_REFERENCES ! SEXP_USE_NO_FEATURES
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

#ifndef SEXP_USE_SAFE_GC_MARK
#define SEXP_USE_SAFE_GC_MARK SEXP_USE_DEBUG_GC > 1
#endif

#ifndef SEXP_USE_CONSERVATIVE_GC
#define SEXP_USE_CONSERVATIVE_GC 0
#endif

#ifndef SEXP_USE_TRACK_ALLOC_SOURCE
#define SEXP_USE_TRACK_ALLOC_SOURCE SEXP_USE_DEBUG_GC > 2
#endif

#ifndef SEXP_USE_HEADER_MAGIC
#define SEXP_USE_HEADER_MAGIC 0
#endif

#ifndef SEXP_USE_SAFE_ACCESSORS
#define SEXP_USE_SAFE_ACCESSORS 0
#endif

#ifndef SEXP_USE_GLOBAL_HEAP
#if SEXP_USE_BOEHM || SEXP_USE_MALLOC
#define SEXP_USE_GLOBAL_HEAP 1
#else
#define SEXP_USE_GLOBAL_HEAP 0
#endif
#endif

#ifndef SEXP_USE_GLOBAL_SYMBOLS
#if SEXP_USE_BOEHM || SEXP_USE_MALLOC
#define SEXP_USE_GLOBAL_SYMBOLS 1
#else
#define SEXP_USE_GLOBAL_SYMBOLS 0
#endif
#endif

#ifndef SEXP_USE_STRICT_TOPLEVEL_BINDINGS
#define SEXP_USE_STRICT_TOPLEVEL_BINDINGS 0
#endif

#if SEXP_USE_STRICT_TOPLEVEL_BINDINGS
#define SEXP_USE_RENAME_BINDINGS 1
#else
#ifndef SEXP_USE_RENAME_BINDINGS
#define SEXP_USE_RENAME_BINDINGS 0
#endif
#endif

#ifndef SEXP_USE_EXTENDED_FCALL
#define SEXP_USE_EXTENDED_FCALL ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_RATIOS
#define SEXP_USE_RATIOS ! SEXP_USE_NO_FEATURES
#endif

/* #ifndef SEXP_USE_COMPLEX */
/* #define SEXP_USE_COMPLEX ! SEXP_USE_NO_FEATURES */
/* #endif */
#define SEXP_USE_COMPLEX 0

#ifndef SEXP_USE_BIGNUMS
#define SEXP_USE_BIGNUMS (SEXP_USE_RATIOS || SEXP_USE_COMPLEX)
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

#ifndef SEXP_USE_PLACEHOLDER_DIGITS
#define SEXP_USE_PLACEHOLDER_DIGITS SEXP_USE_FLONUMS
#endif

#ifndef SEXP_PLACEHOLDER_DIGIT
#define SEXP_PLACEHOLDER_DIGIT '#'
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

#ifndef SEXP_USE_FOLD_CASE_SYMS
#define SEXP_USE_FOLD_CASE_SYMS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_DEFAULT_FOLD_CASE_SYMS
#define SEXP_DEFAULT_FOLD_CASE_SYMS 0
#endif

#ifndef SEXP_USE_TAIL_JUMPS
#define SEXP_USE_TAIL_JUMPS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_RESERVE_OPCODE
#define SEXP_USE_RESERVE_OPCODE SEXP_USE_TAIL_JUMPS
#endif

#ifndef SEXP_USE_UNBOXED_LOCALS
#define SEXP_USE_UNBOXED_LOCALS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_DEBUG_VM
#define SEXP_USE_DEBUG_VM 0
#endif

#ifndef SEXP_USE_PROFILE_VM
#define SEXP_USE_PROFILE_VM 0
#endif

#ifndef SEXP_USE_UTF8_STRINGS
#define SEXP_USE_UTF8_STRINGS ! SEXP_USE_NO_FEATURES
#endif

#ifndef SEXP_USE_MUTABLE_STRINGS
#define SEXP_USE_MUTABLE_STRINGS 1
#endif

#if (SEXP_USE_UTF8_STRINGS && SEXP_USE_MUTABLE_STRINGS)
#define SEXP_USE_PACKED_STRINGS 0
#endif
#ifndef SEXP_USE_PACKED_STRINGS
#define SEXP_USE_PACKED_STRINGS 1
#endif

#ifndef SEXP_USE_STRING_STREAMS
#ifdef _WIN32
#define SEXP_USE_STRING_STREAMS 0
#else
#define SEXP_USE_STRING_STREAMS ! SEXP_USE_NO_FEATURES
#endif
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

#ifndef SEXP_INIT_BCODE_SIZE
#define SEXP_INIT_BCODE_SIZE 128
#endif
#ifndef SEXP_INIT_STACK_SIZE
#if SEXP_USE_CHECK_STACK
#define SEXP_INIT_STACK_SIZE 1024
#else
#define SEXP_INIT_STACK_SIZE 8192
#endif
#endif
#ifndef SEXP_MAX_STACK_SIZE
#define SEXP_MAX_STACK_SIZE SEXP_INIT_STACK_SIZE*1000
#endif

#if SEXP_USE_NATIVE_X86
#undef SEXP_USE_BOEHM
#define SEXP_USE_BOEHM 1
#undef SEXP_USE_FLONUMS
#define SEXP_USE_FLONUMS 0
#undef SEXP_USE_BIGNUMS
#define SEXP_USE_BIGNUMS 0
#undef SEXP_USE_SIMPLIFY
#define SEXP_USE_SIMPLIFY 0
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
#elif defined(_WIN32)
#define snprintf(buf, len, fmt, val) sprintf(buf, fmt, val)
#define strcasecmp lstrcmpi
#define strncasecmp(s1, s2, n) lstrcmpi(s1, s2)
#define round(x) floor((x)+0.5)
#define trunc(x) floor((x)+0.5*(((x)<0)?1:0))
#define isnan(x) (x!=x)
#define isinf(x) (x > DBL_MAX || x < -DBL_MAX)
#endif

#ifdef _WIN32
#define sexp_pos_infinity (DBL_MAX*DBL_MAX)
#define sexp_neg_infinity -sexp_pos_infinity
#define sexp_nan log(-2)
#else
#define sexp_pos_infinity (1.0/0.0)
#define sexp_neg_infinity -sexp_pos_infinity
#define sexp_nan (0.0/0.0)
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
