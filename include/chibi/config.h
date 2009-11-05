/*  config.h -- general configuration                    */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

/* uncomment this to disable the module system */
/* #define USE_MODULES 0 */

/* uncomment this to disable dynamic loading */
/* #define USE_DL 0 */

/* uncomment this to use the Boehm conservative GC */
/* #define USE_BOEHM 1 */

/* uncomment this to just malloc manually instead of any GC */
/* #define USE_MALLOC 1 */

/* uncomment this to add conservative checks to the native GC */
/* #define USE_DEBUG_GC 1 */

/* uncomment this if you only want fixnum support */
/* #define USE_FLONUMS 0 */

/* uncomment this if you want immediate flonums */
/* #define USE_IMMEDIATE_FLONUMS 1 */

/* uncomment this if you don't want bignum support */
/* #define USE_BIGNUMS 0 */

/* uncomment this if you don't need extended math operations */
/* #define USE_MATH 0 */

/* uncomment this to disable warning about references to undefined variables */
/* #define USE_WARN_UNDEFS 0 */

/* uncomment this to disable huffman-coded immediate symbols */
/* #define USE_HUFF_SYMS 0 */

/* uncomment this to just use a single list for hash tables */
/* #define USE_HASH_SYMS 0 */

/* uncomment this to disable string ports */
/* #define USE_STRING_STREAMS 0 */

/* uncomment this to enable stack overflow checks */
/* #define USE_CHECK_STACK 1 */

/* uncomment this to disable debugging utilities */
/* #define USE_DEBUG 0 */

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

#ifndef USE_DL
#ifdef PLAN9
#define USE_DL 0
#else
#define USE_DL 1
#endif
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

#ifndef USE_IMMEDIATE_FLONUMS
#define USE_IMMEDIATE_FLONUMS 0
#endif

#ifndef USE_BIGNUMS
#define USE_BIGNUMS 1
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
