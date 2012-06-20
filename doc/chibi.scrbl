@; #lang scribble/manual

@title{Chibi-Scheme}
@author{Alex Shinn}

@centered{@smaller{Minimal Scheme Implementation for use as an Extension Language}}
@centered{@url{http://synthcode.com/wiki/chibi-scheme/}}

@section{Introduction}

Chibi-Scheme is a very small library intended for use as an extension
and scripting language in C programs.  In addition to support for
lightweight VM-based threads, each VM itself runs in an isolated heap
allowing multiple VMs to run simultaneously in different OS threads.

The default language is an extended subset of the current draft R7RS
Scheme, with support for all libraries.  Support for additional
languages such as JavaScript, Go, Lua and Bash are planned for future
releases.  Scheme is chosen as a substrate because its first class
continuations and guaranteed tail-call optimization makes implementing
other languages easy.

The system is designed in optional layers, beginning with a VM based
on a small set of opcodes, a set of primitives implemented in C, a
default language, a module system implementation, and a set of
standard modules.  You can choose whichever layer suits your needs
best and customize the rest.  Adding your own primitives or wrappers
around existing C libraries is easy with the C FFI.

Chibi is known to build and run on 32 and 64-bit Linux, FreeBSD,
DragonFly, OS X, iOS, Windows (under Cygwin) and Plan9.

@section{Installation}

To build, just run "make".  This will provide a shared library
"libchibi-scheme", as well as a sample "chibi-scheme" command-line
repl.  The "chibi-scheme-static" make target builds an equivalent
static executable.  If your make doesn't support GNU make
conditionals, then you'll need to edit the top of the Makefile to
choose the appropriate settings.  On Plan9 just run "mk".  You can
test the build with "make test".

To install run "make install".  If you want to try the executable out
without installing, you will probably need to set LD_LIBRARY_PATH,
depending on your platform.  If you have an old version installed,
run "make uninstall" first, or manually delete the directory.

You can edit the file chibi/features.h for a number of settings,
mostly disabling features to make the executable smaller.  You can
specify standard options directly as arguments to make, for example

@command{make CFLAGS=-Os CPPFLAGS=-DSEXP_USE_NO_FEATURES=1}

to optimize for size, or

@command{make LDFLAGS=-L/usr/local/lib CPPFLAGS=-I/usr/local/include}

to compile against a library installed in /usr/local.

By default Chibi uses a custom, precise, non-moving GC (non-moving is
important so you can maintain references from C code).  You can link
against the Boehm conservative GC by editing the features.h file, or
directly from make with:

@command{make SEXP_USE_BOEHM=1}

To compile a static executable, use

@command{make chibi-scheme-static SEXP_USE_DL=0}

To compile a static executable with all C libraries statically
included, first you need to create a clibs.c file, which can be done
with:

@command{make clibs.c}

or edited manually.  Be sure to run this with a non-static
chibi-scheme.  Then you can make the static executable with:

@command{
make -B chibi-scheme-static SEXP_USE_DL=0 CPPFLAGS=-DSEXP_USE_STATIC_LIBS
}

@subsection{Compile-Time Options}

The include file @ccode{"chibi/features.h"} describes a number of
C preprocessor values which can be enabled or disabled by setting to
1 or 0 respectively.  For example, the above commands used the
features @ccode{SEXP_USE_BOEHM}, @ccode{SEXP_USE_DL} and
@ccode{SEXP_USE_STATIC_LIBS}.  Many features are still experimental
and may be removed from future releases, but the important features
are listed below.

@itemlist[
@item{@ccode{SEXP_USE_BOEHM} - link with the Boehm GC instead of the native Chibi GC}
@item{@ccode{SEXP_USE_DL} - allow dynamic linking (enabled by default)}
@item{@ccode{SEXP_USE_STATIC_LIBS} - compile the standard C libs statically}
@item{@ccode{SEXP_USE_MODULES} - use the module system}
@item{@ccode{SEXP_USE_GREEN_THREADS} - use lightweight threads (enabled by default)}
@item{@ccode{SEXP_USE_SIMPLIFY} - use a simplification optimizer pass (enabled by default)}
@item{@ccode{SEXP_USE_BIGNUMS} - use bignums (enabled by default)}
@item{@ccode{SEXP_USE_FLONUMS} - use flonums (enabled by default)}
@item{@ccode{SEXP_USE_RATIOS} - use exact ratios (enabled by default)}
@item{@ccode{SEXP_USE_COMPLEX} - use complex numbers (enabled by default)}
@item{@ccode{SEXP_USE_UTF8_STRINGS} - Unicode support (enabled by default)}
@item{@ccode{SEXP_USE_NO_FEATURES} - disable almost all features}
]

@subsection{Installed Programs}

The command-line programs @ccode{chibi-scheme}, @ccode{chibi-doc} and
@ccode{chibi-ffi} are installed by default, along with manpages.
@ccode{chibi-scheme} provides a REPL and way to run scripts.  In the
interest of size it has no --help option - see the man page for usage.
@ccode{chibi-doc} is the command-line interface to the literate
documentation system described in
@hyperlink["lib/chibi/scribble.html"]{(chibi scribble)}, and used to
build this manual.  @ccode{chibi-ffi} is a tool to build wrappers for
C libraries, described in the FFI section below.

@section{Default Language}

@subsection{Scheme Standard}

The default language is based on the latest draft of
@hyperlink["http://scheme-reports.org/"]{R7RS}, which is mostly a
superset of
@hyperlink["http://www.schemers.org/Documents/Standards/R5RS/HTML/"]{R5RS}.
Some of the more expensive bindings are not included in the interest
of size and quick startup, and some extra low-level utilities are
included for convenience and bootstrapping.  Note the builtin
@scheme{equal?}  does not support cyclic structures (you need the R7RS
@scheme{(scheme base)} or @scheme{(chibi equiv)}), nor do the default
reader and writer (you need @scheme{(srfi 38)} or the R7RS
@scheme{(scheme read)} and @scheme{(scheme write)}).

To get the exact R7RS language, you can @scheme{(import (scheme base))},
and likewise for the other R7RS libraries.

The reader defaults to case-sensitive, like R6RS and R7RS but unlike
R5RS.  The default configuration includes the full numeric tower:
fixnums, flonums, bignums, exact rationals and complex numbers.

Full continuations are supported, but currently continuations don't
take C code into account.  This means that you can call from Scheme to
C and then from C to Scheme again, but continuations passing through
this chain may not do what you expect.  The only higher-order C
functions (thus potentially running afoul of this) in the standard
environment are @scheme{load} and @scheme{eval}.  The result of
invoking a continuation created by a different thread is also
currently unspecified.

In R7RS (and R6RS) semantics it is impossible to use two macros from
different modules which both use the same auxiliary keywords (like
@scheme{else} in @scheme{cond} forms) without renaming one of the
keywords.  By default Chibi considers all top-level bindings
effectively unbound when matching auxiliary keywords, so this case
will "just work".  This decision was made because the chance of
different modules using the same keywords seems more likely than user
code unintentionally matching a top-level keyword with a different
binding, however if you want to use R7RS semantics you can compile
with @ccode{SEXP_USE_STRICT_TOPLEVEL_BINDINGS=1}.

@scheme{load} is extended to accept an optional environment argument, like
@scheme{eval}.  You can also @scheme{load} shared libraries in addition to
Scheme source files - in this case the function @cfun{sexp_init_library} is
automatically called with the following signature:

@ccode{
  sexp_init_library(sexp context, sexp self, sexp_sint_t n, sexp environment,
                    const char* version, sexp_abi_identifier_t abi);
}

The following additional procedures are available in the default
environment:

@itemlist[
@item{@scheme{(print-exception exn out)} - prints a human-readable description of @var{exn} to the output port @var{out}}
@item{@scheme{(port-fold-case? port)} - returns @scheme{#t} iff the given input port folds case on @scheme{read}}
@item{@scheme{(set-port-fold-case! port bool)} - set the case-folding behavior of @var{port}}
@item{@scheme{(string-concatenate list-of-strings [sep])} - append the strings joined by @var{sep}}
]

@subsection{Module System}

Chibi uses the R7RS module system natively, which is a simple static
module system in the style of the
@hyperlink["http://s48.org/"]{Scheme48} module system.  As with most
features this is optional, and can be ignored or completely disabled
at compile time.

Modules names are hierarchical lists of symbols or numbers.  A module
definition uses the following form:

@schemeblock{
  (define-library (foo bar baz)
    <library-declarations> ...)
}

where @var{<library-declarations>} can be any of

@schemeblock{
  (export <id> ...)                    ;; specify an export list
  (import <import-spec> ...)           ;; specify one or more imports
  (begin <expr> ...)                   ;; inline Scheme code
  (include <file> ...)                 ;; load one or more files
  (include-shared <file> ...)          ;; dynamic load a library
}

@var{<import-spec>} can either be a module name or any of

@schemeblock{
  (only <import-spec> <id> ...)
  (except <import-spec> <id> ...)
  (rename <import-spec> (<from-id> <to-id>) ...)
  (prefix <prefix-id> <import-spec>)
}

These forms perform basic selection and renaming of individual
identifiers from the given module. They may be composed to perform
combined selection and renaming.

Some modules can be statically included in the initial configuration,
and even more may be included in image files, however in general
modules are searched for in a module load path.  The definition of the
module @scheme{(foo bar baz)} is searched for in the file
@scheme{"foo/bar/baz.sld"}.  The default module path includes the
installed directories, @scheme{"."} and @scheme{"./lib"}.  Additional
directories can be specified with the command-line options @ccode{-I}
and @ccode{-A} (see the command-line options below) or with the
@scheme{add-modue-directory} procedure at runtime.  You can search for
a module file with @scheme{(find-module-file <file>)}, or load it with
@scheme{(load-module-file <file> <env>)}.

Within the module definition, files are loaded relative to the .sld
file, and are written with their extension (so you can use whatever
suffix you prefer - .scm, .ss, .sls, etc.).

Shared modules, on the other hand, should be specified @emph{without} the
extension - the correct suffix will be added portably (e.g. .so for Unix and
.dylib for OS X).

You may also use @scheme{cond-expand} and arbitrary macro expansions in a
module definition to generate @var{<module-declarations>}.

@subsection{Macro System}

@scheme{syntax-rules} macros are provided by default, with the extensions from
@hyperlink["http://srfi.schemers.org/srfi-46/srfi-46.html"]{SRFI-46}.
In addition, low-level hygienic macros are provided with a
syntactic-closures interface, including @scheme{sc-macro-transformer},
@scheme{rsc-macro-transformer}, and @scheme{er-macro-transformer}.  A good
introduction to syntactic-closures can be found at
@url{http://community.schemewiki.org/?syntactic-closures}.

@scheme{identifier?}, @scheme{identifier->symbol}, @scheme{identifier=?}, and
@scheme{make-syntactic-closure} and @scheme{strip-syntactic-closures} are
also available.

@subsection{Types}

You can define new record types with
@hyperlink["http://srfi.schemers.org/srfi-9/srfi-9.html"]{SRFI-9}, or
inherited record types with
@hyperlink["http://srfi.schemers.org/srfi-99/srfi-99.html"]{SRFI-99}.
These are just syntactic sugar for the following more primitive type
constructors:

@schemeblock{
(register-simple-type <name-string> <num-fields>)
 => <type>

(make-type-predicate <opcode-name-string> <type>)
 => <opcode>  ; takes 1 arg, returns #t iff that arg is of the type

(make-constructor <constructor-name-string> <type>)
 => <opcode>  ; takes 0 args, returns a newly allocated instance of type

(make-getter <getter-name-string> <type> <field-index>)
 => <opcode>  ; takes 1 args, retrieves the field located at the index

(make-setter <setter-name-string> <type> <field-index>)
 => <opcode>  ; takes 2 args, sets the field located at the index
}

@subsection{Unicode}

Chibi supports Unicode strings, encoding them as utf8.  This provides easy
interoperability with many C libraries, but means that @scheme{string-ref} and
@scheme{string-set!} are O(n), so they should be avoided in
performance-sensitive code.

In general you should use high-level APIs such as @scheme{string-map}
to ensure fast string iteration.  String ports also provide a simple
way to efficiently iterate and construct strings, by looping over an
input string or accumulating characters in an output string.

The @scheme{in-string} and @scheme{in-string-reverse} iterators in the
@scheme{(chibi loop)} module will also iterate over strings
efficiently while hiding the low-level details.

In the event that you do need a low-level interface, such as when
writing your own iterator protocol, you should use the following
string cursor API instead of indexes.

@itemlist[
@item{@scheme{(string-cursor-start str)}
@p{returns a start cursor for the string}}
@item{@scheme{(string-cursor-end str)}
@p{returns a cursor one past the last valid cursor}}
@item{@scheme{(string-cursor-ref str cursor)}
@p{get the char at the given cursor}}
@item{@scheme{(string-cursor-next str cursor)}
@p{increment to the next cursor}}
@item{@scheme{(string-cursor-prev str cursor)}
@p{decrement to the previous cursor}}
@item{@scheme{(substring-cursor str cs1 [cs2])}
@p{take a substring from the given cursors}}
@item{@scheme{(string-cursor<? cs1 cs2)}
@p{cs1 is before cs2}}
@item{@scheme{(string-cursor<=? cs1 cs2)}
@p{cs1 is before or the same as cs2}}
@item{@scheme{(string-cursor=? cs1 cs2)}
@p{cs1 is the same as cs2}}
@item{@scheme{(string-cursor>? cs1 cs2)}
@p{cs1 is after cs2}}
@item{@scheme{(string-cursor>=? cs1 cs2)}
@p{cs1 is the same or after cs2}}
]

@section{Embedding in C}

@subsection{Quick Start}

To use Chibi-Scheme in a program you need to link against the
"libchibi-scheme" library and include the "eval.h" header file:

@ccode{#include <chibi/eval.h>}

All definitions begin with a "sexp_" prefix, or "SEXP_" for constants.
In addition to the prototypes and utility macros, this includes the
following type definitions:

@itemlist[
@item{@ctype{sexp} - an s-expression, used to represent all Scheme objects}
@item{@ctype{sexp_uint_t} - an unsigned integer using as many bits as sexp}
@item{@ctype{sexp_sint_t} - a signed integer using as many bits as sexp}
]

A simple program might look like:

@ccodeblock{
void dostuff(sexp ctx) {
  /* declare and preserve local variables */
  sexp_gc_var2(obj1, obj2);
  sexp_gc_preserve2(ctx, obj1, obj2);

  /* load a file containing Scheme code */
  obj1 = sexp_c_string(ctx, "/path/to/source/file.scm", -1);
  sexp_load(ctx, obj1, NULL);

  /* eval a C string as Scheme code */
  sexp_eval_string(ctx, "(some scheme expression)", -1, NULL);

  /* construct a Scheme expression to eval */
  obj1 = sexp_intern(ctx, "my-procedure", -1);
  obj2 = sexp_cons(ctx, obj1, SEXP_NULL);
  sexp_eval(ctx, obj2, NULL);

  /* release the local variables */
  sexp_gc_release2(ctx);
}

int main(int argc, char** argv) {
  sexp ctx;
  ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(ctx, NULL, stdin, stdout, stderr, 0);
  dostuff(ctx);
  sexp_destroy_context(ctx);
}
}

Looking at @cfun{main}, @cfun{sexp_make_eval_context} and
@cfun{sexp_destroy_context} create and destroy a "context", which
manages the heap and VM state.  The meaning of the arguments is
explained in detail below, but these values will give reasonable
defaults, in this case constructing an environment with the core
syntactic forms, opcodes, and standard C primitives.

This is still a fairly bare environment, so we call
@cfun{sexp_load_standard_env} to find and load the default
initialization file.

The resulting context can then be used to construct objects, call
functions, and most importantly evaluate code, as is done in
@cfun{dostuff}.  The default garbage collector for Chibi is precise,
which means we need to declare and preserve references to any
temporary values we may generate, which is what the
@cmacro{sexp_gc_var2}, @cmacro{sexp_gc_preserve2} and
@cmacro{sexp_gc_release2} macros do (there are similar macros for
values 1-6).  Precise GCs prevent a class of memory leaks (and
potential attackes based thereon), but if you prefer convenience then
Chibi can be compiled with a conservative GC and you can ignore these.

The interesting part is then the calls to @cfun{sexp_load},
@cfun{eval_string} and @cfun{eval} which evaluate code stored in
files, C strings, or represented as s-expressions respectively.

Destroying a context runs any finalizers for all objects in the heap
and then frees the heap memory (but has no effect on other contexts
you or other users of the library may have created).

@subsection{Contexts and Evaluation}

Contexts represent the state needed to perform evaluation.  This includes
keeping track of the heap (when using precise GC), a default environment,
execution stack, and any global values.  A program being evaluated in one
context may spawn multiple child contexts, such as when you call @scheme{eval},
and each child will share the same heap and globals.  When using multiple
interpreter threads, each thread has its own context.

You can also create independent contexts with their own separate heaps.  These
can run simultaneously in multiple OS threads without any need for
synchronization.

@itemlist[

@item{@ccode{sexp_make_context(sexp ctx, size_t size, size_t max_size)}
@p{
Creates a new context object.  The context has no associated environment, and
so cannot be used for evaluation, but can be used to construct Scheme objects
and call primitive C functions on them.

If @var{ctx} is non-NULL it becomes the "parent" context.  The resulting
context will share the same heap as its parent, and when using a precise GC
preserve any variables preserved by the parent, but the parent will not
preserve the child context by default.  Typically you either preserve the child
manually or use it to perform a single sub-task then discard it and return to
using only the parent.

Otherwise, a new heap is allocated with @var{size} bytes, expandable to a
maximum of @var{max_size} bytes, using the system defaults if either is 0.
}}

@item{@ccode{sexp_make_eval_context(sexp ctx, sexp stack, sexp env, sexp_uint_t size, sexp_uint_t max_size)}
@p{
Similar to sexp_make_context, but also associates a stack, environment, and
additional globals necessary to evaluate code.  Either or both of @var{stack}
and @var{env} may be NULL, in which case defaults will be generated.  The
default environment includes the compiled-in C primitives, as well as the 10
core forms: @scheme{define}, @scheme{set!}, @scheme{lambda}, @scheme{if},
@scheme{begin}, @scheme{quote}, @scheme{syntax-quote}, @scheme{define-syntax},
@scheme{let-syntax}, and @scheme{letrec-syntax}.
}}

@item{@ccode{sexp_load_standard_env(sexp ctx, sexp env, sexp version)}
@p{
Loads the standard parameters for @var{env}, constructs the feature list from
pre-compiled defaults, and loads the installed initialization file for
@var{version}, which should be the value @var{SEXP_SEVEN}.
Also creates an @scheme{interaction-environment} parameter
and sets @var{env} itself to that.
}}

@item{@ccode{sexp_load_standard_ports(sexp ctx, sexp env, FILE* in, FILE* out, FILE* err, int leave_open)}
@p{
Creates @scheme{current-input-port}, @scheme{current-output-port}, and
@scheme{current-error-port} parameters from @var{in}, @var{out} and
@var{err}, and binds them in @var{env}.  If @var{env} is @cvar{NULL}
the default context environment is used.  Any of the @ctype{FILE*} may
be @cvar{NULL}, in which case the corresponding port is not set.  If
@var{leave_open} is true, then the underlying @ctype{FILE*} is left
open after the Scheme port is closed, otherwise they are both closed
together.
}}

@item{@ccode{sexp_load(sexp ctx, sexp file, sexp env)}
@p{
Searches the installation path for the @var{file} and loads it in the
environment @var{env}.  @var{file} may be a dynamic library or source code.
}}

@item{@ccode{sexp_eval(sexp ctx, sexp obj, sexp env)}
@p{
Evaluates @var{obj} as a source form in the environment @var{env} and
returns the result.
}}

@item{@ccode{sexp_eval_string(sexp ctx, const char* str, int len, sexp env)}
@p{
Reads a s-expression from the C string @var{str} (or the first @var{len} bytes
if @var{len} is non-negative), evaluates the resulting form in the environment
@var{env}, and returns the result.
}}

@item{@ccode{sexp_apply(sexp ctx, sexp proc, sexp args)}
@p{
Applies the procedure @var{proc} to the arguments in the list @var{args} and
returns the result.
}}

@item{@ccode{sexp_context_env(sexp ctx)}
@p{
Returns the current default environment associated with the context @var{ctx}.
}}

@item{@ccode{sexp_env_define(sexp ctx, sexp env, sexp sym, sexp val)}
@p{
Adds a new binding for @var{sym} in @var{env} with value @var{val}.
}}

@item{@ccode{sexp_env_ref(sexp env, sexp sym, sexp dflt)}
@p{
Returns the current binding of @var{sym} in @var{env}, or @var{dflt} if there
is no binding.
}}

@item{@ccode{sexp_parameter_ref(sexp ctx, sexp param)}
@p{
Returns the current dynamic value of the parameter @var{param} in the
given context.
}}

]

@subsection{Garbage Collection}

Chibi uses a precise garbage collector by default, which means when performing
multiple computations on the C side you must explicitly preserve any temporary
values.  You can declare variables to be preserved with sexp_gc_var@italic{n},
for n from 1 to 6.@margin-note{You can declare additional macros for larger
values of n if needed.}

@ccode{
sexp_gc_var@italic{n}(obj@subscript{1}, obj@subscript{2}, ..., obj@subscript{n})
}

This is equivalent to the declaration

@ccode{
sexp obj@subscript{1}, obj@subscript{2}, ..., obj@subscript{n};
}

except it makes preservation possible.  Because it is a declaration it must
occur at the beginning of your function, and because it includes assignments
(in the macro-expanded form) it should occur after all other declarations.

To preserve these variables for a given context, you can then use
sexp_gc_preserve@italic{n}:

@ccode{
sexp_gc_preserve@italic{n}(ctx, obj@subscript{1}, obj@subscript{2}, ..., obj@subscript{n})
}

This can be delayed in your code until you know a potentially memory-allocating
computation will be performed, but once you call sexp_gc_preserve@italic{n} it
@emph{must} be paired with a matching sexp_gc_release@italic{n}:

@ccode{
sexp_gc_release@italic{n}(ctx);
}

Note each of these have different signatures. sexp_gc_var@italic{n} just lists
the variables to be declared.  sexp_gc_preserve@italic{n} prefixes these with
the context in which they are to be preserved, and sexp_gc_release@italic{n}
just needs the context.

A typical usage for these is:

@ccodeblock{
sexp foo(sexp ctx, sexp bar, sexp baz) {
  /* variable declarations */
  int i, j;
  ...
  sexp_gc_var3(tmp1, tmp2, res);

  /* asserts or other shortcut returns */
  sexp_assert_type(ctx, sexp_barp, SEXP_BAR, bar);
  sexp_assert_type(ctx, sexp_bazp, SEXP_BAZ, baz);

  /* preserve the variables in ctx */
  sexp_gc_preserve3(ctx, tmp1, tmp2, res);

  /* perform your computations */
  tmp1 = ...
  tmp2 = ...
  res = ...

  /* release before returning */
  sexp_gc_release3(ctx);

  return res;
}
}

If compiled with the Boehm GC, sexp_gc_var@italic{n} just translates to the
plain declaration, while sexp_gc_preserve@italic{n} and
sexp_gc_release@italic{n} become noops.

When interacting with a garbage collection system from another
language, or communicating between different Chibi managed heaps, you
may want to manually ensure objects are preserved irrespective of any
references to it from other objects in the same heap.  This can be
done with the @ccode{sexp_preserve_object} and
@ccode{sexp_release_object} utilities.

@ccode{
sexp_preserve_object(ctx, obj)
}

Increment the absolute reference count for @var{obj}.  So long as the
reference count is above 0, @var{obj} will not be reclaimed even if
there are no references to it from other object in the Chibi managed
heap.

@ccode{
sexp_release_object(ctx, obj)
}

Decrement the absolute reference count for @var{obj}.

@subsection{API Index}

@subsubsection{Type Predicates}

The sexp represents different Scheme types with the use of tag bits for
so-called "immediate" values, and a type tag for heap-allocated values.  The
following predicates can be used to distinguish these types.  Note the
predicates in C all end in "p".  For efficiency they are implemented as macros,
and so may evaluate their arguments multiple times.

@itemlist[
@item{@ccode{sexp_booleanp(obj)} - @var{obj} is @scheme{#t} or @scheme{#f}}
@item{@ccode{sexp_fixnump(obj)} - @var{obj} is an immediate integer}
@item{@ccode{sexp_flonump(obj)} - @var{obj} is an inexact real}
@item{@ccode{sexp_bignump(obj)} - @var{obj} is a heap-allocated integer}
@item{@ccode{sexp_integerp(obj)} - @var{obj} is an integer}
@item{@ccode{sexp_numberp(obj)} - @var{obj} is any kind of number}
@item{@ccode{sexp_charp(obj)} - @var{obj} is a character}
@item{@ccode{sexp_stringp(obj)} - @var{obj} is a string}
@item{@ccode{sexp_symbolp(obj)} - @var{obj} is a symbol}
@item{@ccode{sexp_idp(obj)} - @var{obj} is a symbol or hygienic identifier}
@item{@ccode{sexp_nullp(obj)} - @var{obj} is the null value}
@item{@ccode{sexp_pairp(obj)} - @var{obj} is a pair}
@item{@ccode{sexp_vectorp(obj)} - @var{obj} is a vector}
@item{@ccode{sexp_iportp(obj)} - @var{obj} is an input port}
@item{@ccode{sexp_oportp(obj)} - @var{obj} is an output port}
@item{@ccode{sexp_portp(obj)} - @var{obj} is any kind of port}
@item{@ccode{sexp_procedurep(obj)} - @var{obj} is a procedure}
@item{@ccode{sexp_opcodep(obj)} - @var{obj} is a primitive opcode}
@item{@ccode{sexp_applicablep(obj)} - @var{obj} is valid as the first arg to apply}
@item{@ccode{sexp_typep(obj)} - @var{obj} is a type}
@item{@ccode{sexp_exceptionp(obj)} - @var{obj} is an exception}
@item{@ccode{sexp_contextp(obj)} - @var{obj} is a context}
@item{@ccode{sexp_envp(obj)} - @var{obj} is an environment}
@item{@ccode{sexp_corep(obj)} - @var{obj} is a special form}
@item{@ccode{sexp_macrop(obj)} - @var{obj} is a macro}
@item{@ccode{sexp_synclop(obj)} - @var{obj} is a syntactic closure}
@item{@ccode{sexp_bytecodep(obj)} - @var{obj} is compiled bytecode}
@item{@ccode{sexp_cpointerp(obj)} - @var{obj} is an opaque C pointer}
]

@subsubsection{Constants}

The following shortcuts for various immediate values are available.

@itemlist[
@item{@ccode{SEXP_FALSE} - the false boolean}
@item{@ccode{SEXP_TRUE} - the true boolean}
@item{@ccode{SEXP_NULL} - the empty list}
@item{@ccode{SEXP_EOF} - the end-of-file object}
@item{@ccode{SEXP_VOID} - an undefined value often returned by mutators}
@item{@ccode{SEXP_ZERO} - shortcut for sexp_make_fixnum(0)}
@item{...}
@item{@ccode{SEXP_TEN} - shortcut for sexp_make_fixnum(10)}
@item{@ccode{SEXP_NEG_ONE} - shortcut for sexp_make_fixnum(-1)}
]

@subsubsection{Accessors}

The following macros provide access to the different components of the
Scheme types.  They do no type checking, essentially translating
directly to pointer offsets, so you should be sure to use the above
predicates to check types first.  They only evaluate their arguments
once.

@itemlist[
@item{@ccode{sexp_make_boolean(n)} - @scheme{#f} if @var{n} is 0, @scheme{#t} otherwise}
@item{@ccode{sexp_unbox_boolean(obj)} - 1 if @var{obj} is @scheme{#t}, 0 otherwise}
@item{@ccode{sexp_make_fixnum(n)} - creates a new fixnum representing int @var{n}}
@item{@ccode{sexp_unbox_fixnum(obj)} - converts a fixnum to a C integer}
@item{@ccode{sexp_make_character(ch)} - creates a new character representing char @var{ch}}
@item{@ccode{sexp_unbox_character(obj)} - converts a character to a C char}
@item{@ccode{sexp_car(pair)} - the car of @var{pair}}
@item{@ccode{sexp_cdr(pair)} - the cdr of @var{pair}}
@item{@ccode{sexp_ratio_numerator(q)} - the numerator of the ratio @var{q}}
@item{@ccode{sexp_ratio_denominator(q)} - the denominator of the ratio @var{q}}
@item{@ccode{sexp_complex_real(z)} - the real part of the complex @var{z}}
@item{@ccode{sexp_complex_imag(z)} - the imaginary part of the complex @var{z}}
@item{@ccode{sexp_string_length(str)} - the byte length of @var{str} as an int}
@item{@ccode{sexp_string_ref(str, i)} - the @var{i}'th byte of string @var{str}}
@item{@ccode{sexp_string_set(str, i, ch)} - set the @var{i}'th byte of string @var{str}}
@item{@ccode{sexp_vector_length(vec)} - the length of @var{vec} as an int}
@item{@ccode{sexp_vector_ref(vec, i)} - the @var{i}'th object of vector @var{vec}}
@item{@ccode{sexp_vector_set(vec, i, obj)} - set the @var{i}'th object of vector @var{vec}}
@item{@ccode{sexp_bytes_length(bv)} - the number of bytes in bytevector @var{bv}}
@item{@ccode{sexp_bytes_ref(bv, i)} - the @var{i}'th byte of bytevector @var{bv}}
@item{@ccode{sexp_bytes_set(bv, i, k)} - set the @var{i}'th byte of bytevector @var{bv}}
]

@subsubsection{Constructors}

Constructors allocate memory and so must be passed a context argument.
Any of these may fail and return the OOM exception object.

@itemlist[
@item{@ccode{sexp_cons(sexp ctx, sexp obj1, sexp obj2)} - create a new pair whose car is @var{obj1} and whose cdr is @var{obj2}}
@item{@ccode{sexp_list1(sexp ctx, sexp obj)} - alias for sexp_cons(ctx, obj, SEXP_NULL)}
@item{@ccode{sexp_list2(sexp ctx, sexp obj1, sexp obj2)} - create a list of two elements}
@item{@ccode{sexp_make_string(sexp ctx, sexp len, sexp ch)} - create a new Scheme string of @var{len} characters, all initialized to @var{ch}}
@item{@ccode{sexp_c_string(sexp ctx, const char* str, int len)} - create a new Scheme string copying the first @var{len} characters of the C string @var{str}.  If @var{len} is -1, uses strlen(@var{str}).}
@item{@ccode{sexp_intern(sexp ctx, const char* str, int len)} - interns a symbol from the first @var{len} characters of the C string @var{str}.  If @var{len} is -1, uses strlen(@var{str}).}
@item{@ccode{sexp_make_vector(sexp ctx, sexp len, sexp obj)} - create a new vector of @var{len} elements, all initialized to @var{obj}}
@item{@ccode{sexp_make_integer(sexp ctx, sexp_sint_t n)} - create an integer, heap allocating as a bignum if needed}
@item{@ccode{sexp_make_unsigned_integer(sexp ctx, sexp_uint_t n)} - create an unsigned integer, heap allocating as a bignum if needed}
]

@subsubsection{I/O}

@itemlist[
@item{@ccode{sexp_read(sexp ctx, sexp in)} - read a single datum from port @var{in}}
@item{@ccode{sexp_write(sexp ctx, sexp obj, sexp out)} - write @var{obj} to port @var{out}}
@item{@ccode{sexp_write_string(sexp ctx, char* str, sexp out)} - write the characters in @var{str} to port @var{out}}
@item{@ccode{sexp_display(sexp ctx, sexp obj, sexp out)} - display @var{obj} to port @var{out}}
@item{@ccode{sexp_newline(sexp ctx, sexp out)} - write a newline to port @var{out}}
@item{@ccode{sexp_print_exception(sexp ctx, sexp exn, sexp out)} - print an error message for @var{exn} to port @var{out}}
@item{@ccode{sexp_current_input_port(sexp ctx)} - the @scheme{current-input-port}}
@item{@ccode{sexp_current_output_port(sexp ctx)} - the @scheme{current-output-port}}
@item{@ccode{sexp_current_error_port(sexp ctx)} - the @scheme{current-error-port}}
@item{@ccode{sexp_debug(sexp ctx, char* msg, sexp obj)} - write @var{obj} with a debug message prefix to @scheme{current-error-port}}
@item{@ccode{sexp_read_from_string(sexp ctx, char* str, int len)} - read a single datum from @var{str}, using at most @var{len} bytes if @var{len} is non-negative}
@item{@ccode{sexp_write_to_string(sexp ctx, sexp obj)} - return a Scheme string representation of @var{obj}}
]

@subsubsection{Utilities}

@itemlist[
@item{@ccode{sexp_equalp(sexp ctx, sexp x, sexp y)} - @scheme{equal?}}
@item{@ccode{sexp_length(sexp ctx, sexp ls)} - @scheme{length}}
@item{@ccode{sexp_listp(sexp ctx, sexp x)} - @scheme{list?}}
@item{@ccode{sexp_memq(sexp ctx, sexp x, sexp ls)} - @scheme{memq}}
@item{@ccode{sexp_assq(sexp ctx, sexp x, sexp ls)} - @scheme{assq}}
@item{@ccode{sexp_reverse(sexp ctx, sexp ls)} - @scheme{reverse}}
@item{@ccode{sexp_nreverse(sexp ctx, sexp ls)} - @scheme{reverse!}}
@item{@ccode{sexp_append2(sexp ctx, sexp ls)} - @scheme{append} for two arguments}
@item{@ccode{sexp_copy_list(sexp ctx, sexp ls)} - return a shallow copy of @var{ls}}
@item{@ccode{sexp_list_to_vector(sexp ctx, sexp ls)} - @scheme{list->vector}}
@item{@ccode{sexp_symbol_to_string(sexp ctx, sexp sym)} - @scheme{symbol->string}}
@item{@ccode{sexp_string_to_symbol(sexp ctx, sexp str)} - @scheme{string->symbol}}
@item{@ccode{sexp_string_to_number(sexp ctx, sexp str)} - @scheme{string->number}}
]

@subsection{Customizing}

You can add your own types and primitives with the following functions.

@itemlist[

@item{@ccode{sexp sexp_define_foreign(sexp ctx, sexp env, const char* name, int num_args, sexp_proc1 func)}
@p{
Defines a new primitive procedure with the name @var{name} in the
environment @var{env}.  The procedure takes @var{num_args} arguments
and passes them to the C function @var{func}.  The C function must
take the standard calling convention:

@ccode{sexp func(sexp ctx, sexp self, sexp n, sexp arg@sub{1}, ..., sexp arg@sub{num_args})}

where @var{ctx} is the current context, @var{self} is the procedure
itself, and @var{n} is the number of arguments passed.

@var{func} is responsible for checking its own argument types.
}}

@item{@ccode{sexp sexp_define_foreign_opt(sexp ctx, sexp env, const char* name, int num_args, sexp_proc1 func, sexp dflt)}
@p{
Equivalent to @cfun{sexp_define_foreign}, except the final argument is
optional and defaults to the value @var{dflt}.
}}

@item{@ccode{sexp sexp_define_foreign_param(sexp ctx, sexp env, const char* name, int num_args, sexp_proc1 func, const char* param)}
@p{
Equivalent to @cfun{sexp_define_foreign_opt}, except instead of a fixed
default argument @var{param} should be the name of a parameter bound in
@var{env}.
}}

@item{@ccode{sexp sexp_register_simple_type(sexp ctx, sexp name, sexp parent, sexp slots)}
@p{
Defines a new simple record type having @var{slots} new slots in addition
to any inherited from the parent type @var{parent}.  If @var{parent} is false,
inherits from the default @var{object} record type.
}}

]

See the C FFI for an easy way to automate adding bindings for C
functions.

@section{C FFI}

The "chibi-ffi" script reads in the C function FFI definitions from an
input file and outputs the appropriate C wrappers into a file with the
same base name and the ".c" extension.  You can then compile that C
file into a shared library:

@command{
chibi-ffi file.stub
cc -fPIC -shared file.c -lchibi-scheme
}

(or using whatever flags are appropriate to generate shared libs on
your platform) and the generated .so file can be loaded directly with
@scheme{load}, or portably using @scheme{(include-shared "file")} in a
module definition (note that include-shared uses no suffix).

The goal of this interface is to make access to C types and functions
easy, without requiring the user to write any C code.  That means the
stubber needs to be intelligent about various C calling conventions
and idioms, such as return values passed in actual parameters.
Writing C by hand is still possible, and several of the core modules
provide C interfaces directly without using the stubber.

@subsection{Includes and Initializations}

@itemlist[
@item{@scheme{(c-include header)} - includes the file @var{header}}
@item{@scheme{(c-system-include header)} - includes the system file @var{header}}
@item{@scheme{(c-declare args ...)} - outputs @var{args} directly in the top-level C source}
@item{@scheme{(c-init args ...)} - evaluates @var{args} as C code after all other library initializations have been performed, with @cvar{ctx} and @cvar{env} in scope}
]

@subsection{Struct Interface}

C structs can be bound as Scheme types with the
@scheme{define-c-struct} form:

@schemeblock{
(define-c-struct struct_name
  [predicate: predicate-name]
  [constructor: constructor-name]
  [finalizer: c_finalizer_name]
  (type c_field_name getter-name setter-name) ...)
}

@var{struct_name} should be the name of a C struct type.  If provided,
@var{predicate-name} is bound to a procedure which takes one object
and returns @scheme{#t} iff the object is of type @var{struct_name}.

If provided, @var{constructor-name} is bound to a procedure of zero
arguments which creates and returns a newly allocated instance of the
type.

If a finalizer is provided, @var{c_finalizer_name} must be a C
function which takes one argument, a pointer to the struct, and
performs any cleanup or freeing of resources necessary.

The remaining slots are similar to the
@hyperlink["http://srfi.schemers.org/srfi-9/srfi-9.html"]{SRFI-9} syntax,
except they are prefixed with a C type (described below).  The
@var{c_field_name} should be a field name of @var{struct_name}.
@var{getter-name} will then be bound to a procedure of one argument, a
@{struct_name} type, which returns the given field.  If provided,
@var{setter-name} will be bound to a procedure of two arguments to
mutate the given field.

The variants @scheme{define-c-class} and @scheme{define-c-union} take
the same syntax but define types with the @ccode{class} and
@ccode{union} keywords respectively.  @scheme{define-c-type} just
defines accessors to an opaque type without any specific struct-like
keyword.

@schemeblock{
;; Example: the struct addrinfo returned by getaddrinfo.

(c-system-include "netdb.h")

(define-c-struct addrinfo
  finalizer: freeaddrinfo
  predicate: address-info?
  (int              ai_family    address-info-family)
  (int              ai_socktype  address-info-socket-type)
  (int              ai_protocol  address-info-protocol)
  ((link sockaddr)  ai_addr      address-info-address)
  (size_t           ai_addrlen   address-info-address-length)
  ((link addrinfo)  ai_next      address-info-next))
}

@subsection{Function and Constant Interface}

C functions are defined with:

@scheme{(define-c return-type name-spec (arg-type ...))}

where @var{name-space} is either a symbol name, or a list of
@scheme{(scheme-name c_name)}.  If just a symbol is used, the C name
is generated automatically by replacing any dashes (-) in the Scheme
name with underscores (_).

Each @var{arg-type} is a type suitable for input validation and
conversion as discussed below.

@schemeblock{
;; Example: define connect(2) in Scheme
(define-c int connect (int sockaddr int))
}

Constants can be defined with:

@scheme{(define-c-const type name-space)}

where @var{name-space} is the same form as in @scheme{define-c}.  This
defines a Scheme variable with the same value as the C constant.

@schemeblock{
;; Example: define address family constants in Scheme
(define-c-const int (address-family/unix "AF_UNIX"))
(define-c-const int (address-family/inet "AF_INET"))
}

@subsection{C Types}

@subsubsection{Basic Types}

@itemlist[
@item{@rawcode{void}}
@item{@rawcode{boolean}}
@item{@rawcode{char}}
@item{@rawcode{sexp} (no conversions)}
]

@subsubsection{Integer Types}

@itemlist[
@item{@rawcode{signed-char}}
@item{@rawcode{short}}
@item{@rawcode{int}}
@item{@rawcode{long}}
@item{@rawcode{unsigned-char}}
@item{@rawcode{unsigned-short}}
@item{@rawcode{unsigned-int}}
@item{@rawcode{unsigned-long}}
@item{@rawcode{size_t}}
@item{@rawcode{pid_t}}
@item{@rawcode{uid_t}}
@item{@rawcode{gid_t}}
@item{@rawcode{time_t} (in seconds, but using the chibi epoch of 2010/01/01)}
@item{@rawcode{errno} (as a return type returns @scheme{#f} on error)}
]

@subsubsection{Float Types}

@itemlist[
@item{@rawcode{float}}
@item{@rawcode{double}}
@item{@rawcode{long-double}}
]

@subsubsection{String Types}

@itemlist[
@item{@rawcode{string} - a null-terminated char*}
@item{@rawcode{env-string} - a @rawcode{VAR=VALUE} string represented as a @scheme{(VAR . VALUE)} pair in Scheme}
@item{@scheme{(array char)} is equivalent to @rawcode{string}}
]

@subsubsection{Port Types}

@itemlist[
@item{@rawcode{input-port}}
@item{@rawcode{output-port}}
]

@subsubsection{Struct Types}

Struct types are by default just referred to by the bare
@var{struct_name} from @scheme{define-c-struct}, and it is assumed you
want a pointer to that type.  To refer to the full struct, use the
struct modifier, as in @scheme{(struct struct-name)}.

@subsubsection{Type modifiers}

Any type may also be written as a list of modifiers followed by the
type itself.  The supported modifiers are:

@itemlist[

@item{@rawcode{const}
@p{Prepends the "const" C type modifier.
As a return or result parameter, makes non-immediates immutable.}}

@item{@rawcode{free}
@p{It's Scheme's responsibility to "free" this resource.
As a return or result parameter, registers the freep flag
this causes the type finalizer to be run when GCed.}}

@item{@rawcode{maybe-null}
@p{This pointer type may be NULL.
As a result parameter, NULL is translated to #f
normally this would just return a wrapped NULL pointer.
As an input parameter, #f is translated to NULL
normally this would be a type error.}}

@item{@rawcode{pointer}
@p{Create a pointer to this type.
As a return parameter, wraps the result in a vanilla cpointer.
As a result parameter, boxes then unboxes the value.}}

@item{@rawcode{reference}
@p{A stack-allocated pointer to this type.
As a result parameter, passes a stack-allocated pointer to
the value, then returns the dereferenced pointer.}}

@item{@rawcode{struct}
@p{Treat this struct type as a struct, not a pointer.
As an input parameter, dereferences the pointer.
As a type field, indicates a nested struct.}}

@item{@rawcode{link}
@p{Add a gc link.
As a field getter, link to the parent object, so the
parent won't be GCed so long as we have a reference
to the child.  This behavior is automatic for nested structs.}}

@item{@rawcode{result}
@p{Return a result in this parameter.
If there are multiple results (including the return type),
they are all returned in a list.
If there are any result parameters, a return type
of errno returns #f on failure, and as eliminated
from the list of results otherwise.}}

@item{@scheme{(value <expr>)}
@p{Specify a fixed value.
As an input parameter, this parameter is not provided
in the Scheme API but always passed as <expr>.}}

@item{@scheme{(default <expr>)}
@p{Specify a default value.
As the final input parameter, makes the Scheme parameter
optional, defaulting to <expr>.}}

@item{@scheme{(array <type> [<length>])}
@p{An array type.
Length must be specified for return and result parameters.
If specified, length can be either an integer, indicating a fixed size,
or the symbol null, indicating a NULL-terminated array.}}

]

@section{Standard Modules}

A number of SRFIs are provided in the default installation.  Note that
SRFIs 0, 6, 22, 23, 46 and 62 are built into the default environment so
there's no need to import them.  This list includes popular SRFIs or
SRFIs used in standard Chibi modules

@itemlist[

@item{@hyperlink["http://srfi.schemers.org/srfi-0/srfi-0.html"]{(srfi 0)  - cond-expand}}
@item{@hyperlink["http://srfi.schemers.org/srfi-1/srfi-1.html"]{(srfi 1)  - list library}}
@item{@hyperlink["http://srfi.schemers.org/srfi-2/srfi-2.html"]{(srfi 2)  - and-let*}}
@item{@hyperlink["http://srfi.schemers.org/srfi-6/srfi-6.html"]{(srfi 6)  - basic string ports}}
@item{@hyperlink["http://srfi.schemers.org/srfi-8/srfi-8.html"]{(srfi 8)  - receive}}
@item{@hyperlink["http://srfi.schemers.org/srfi-9/srfi-9.html"]{(srfi 9)  - define-record-type}}
@item{@hyperlink["http://srfi.schemers.org/srfi-11/srfi-11.html"]{(srfi 11) - let-values/let*-values}}
@item{@hyperlink["http://srfi.schemers.org/srfi-16/srfi-16.html"]{(srfi 16) - case-lambda}}
@item{@hyperlink["http://srfi.schemers.org/srfi-18/srfi-18.html"]{(srfi 18) - multi-threading support}}
@item{@hyperlink["http://srfi.schemers.org/srfi-22/srfi-22.html"]{(srfi 22) - running scheme scripts on Unix}}
@item{@hyperlink["http://srfi.schemers.org/srfi-23/srfi-23.html"]{(srfi 23) - error reporting mechanism}}
@item{@hyperlink["http://srfi.schemers.org/srfi-26/srfi-26.html"]{(srfi 26) - cut/cute partial application}}
@item{@hyperlink["http://srfi.schemers.org/srfi-27/srfi-27.html"]{(srfi 27) - sources of random bits}}
@item{@hyperlink["http://srfi.schemers.org/srfi-33/srfi-33.html"]{(srfi 33) - bitwise operators}}
@item{@hyperlink["http://srfi.schemers.org/srfi-38/srfi-38.html"]{(srfi 38) - read/write shared structures}}
@item{@hyperlink["http://srfi.schemers.org/srfi-39/srfi-39.html"]{(srfi 39) - parameter objects}}
@item{@hyperlink["http://srfi.schemers.org/srfi-46/srfi-46.html"]{(srfi 46) - basic syntax-rules extensions}}
@item{@hyperlink["http://srfi.schemers.org/srfi-55/srfi-55.html"]{(srfi 55) - require-extension}}
@item{@hyperlink["http://srfi.schemers.org/srfi-62/srfi-62.html"]{(srfi 62) - s-expression comments}}
@item{@hyperlink["http://srfi.schemers.org/srfi-69/srfi-69.html"]{(srfi 69) - basic hash tables}}
@item{@hyperlink["http://srfi.schemers.org/srfi-95/srfi-95.html"]{(srfi 95) - sorting and merging}}
@item{@hyperlink["http://srfi.schemers.org/srfi-98/srfi-98.html"]{(srfi 98) - environment access}}
@item{@hyperlink["http://srfi.schemers.org/srfi-99/srfi-99.html"]{(srfi 99) - ERR5RS records}}

]

Additional non-standard modules are put in the @scheme{(chibi)} module
namespace.

@itemlist[

@item{@hyperlink["lib/chibi/ast.html"]{(chibi ast) - Abstract Syntax Tree and other internal data types}}

@item{@hyperlink["lib/chibi/disasm.html"]{(chibi disasm) - Disassembler for the virtual machine}}

@item{@hyperlink["lib/chibi/equiv.html"]{(chibi equiv) - A version of @scheme{equal?} which is guaranteed to terminate}}

@item{@hyperlink["lib/chibi/filesystem.html"]{(chibi filesystem) - Interface to the filesystem and file descriptor objects}}

@item{@hyperlink["lib/chibi/generic.html"]{(chibi generic) - Generic methods for CLOS-style object oriented programming}}

@item{@hyperlink["lib/chibi/heap-stats.html"]{(chibi heap-stats) - Utilities for gathering statistics on the heap}}

@item{@hyperlink["lib/chibi/io.html"]{(chibi io) - Various I/O extensions and custom ports}}

@item{@hyperlink["lib/chibi/loop.html"]{(chibi loop) - Fast and extensible loop syntax}}

@item{@hyperlink["lib/chibi/match.html"]{(chibi match) - Intuitive and widely supported pattern matching syntax}}

@item{@hyperlink["lib/chibi/mime.html"]{(chibi mime) - Parse MIME files into SXML}}

@item{@hyperlink["lib/chibi/modules.html"]{(chibi modules) - Introspection for the module system itself}}

@item{@hyperlink["lib/chibi/net.html"]{(chibi net) - Simple networking interface}}

@item{@hyperlink["lib/chibi/pathname.html"]{(chibi pathname) - Utilities to decompose and manipulate pathnames}}

@item{@hyperlink["lib/chibi/process.html"]{(chibi process) - Interface to spawn processes and handle signals}}

@item{@hyperlink["lib/chibi/repl.html"]{(chibi repl) - A full-featured Read/Eval/Print Loop}}

@item{@hyperlink["lib/chibi/scribble.html"]{(chibi scribble) - A parser for the scribble syntax used to write this manual}}

@item{@hyperlink["lib/chibi/stty.html"]{(chibi stty) - A high-level interface to ioctl}}

@item{@hyperlink["lib/chibi/system.html"]{(chibi system) - Access to the host system and current user information}}

@item{@hyperlink["lib/chibi/test.html"]{(chibi test) - A simple unit testing framework}}

@item{@hyperlink["lib/chibi/time.html"]{(chibi time) - An interface to the current system time}}

@item{@hyperlink["lib/chibi/trace.html"]{(chibi trace) - A utility to trace procedure calls}}

@item{@hyperlink["lib/chibi/type-inference.html"]{(chibi type-inference) - An easy-to-use type inference system}}

@item{@hyperlink["lib/chibi/uri.html"]{(chibi uri) - Utilities to parse and construct URIs}}

@item{@hyperlink["lib/chibi/weak.html"]{(chibi weak) - Data structures with weak references}}

]
