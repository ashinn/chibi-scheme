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

@margin-note{Converging with the
R7RS Scheme small language standard when the standard is finalized.}
The default language is R5RS Scheme with support for additional
languages such as JavaScript to be provided in future releases.
Scheme is chosen as a substrate because its first class continuations
and guaranteed tail-call optimization makes implementing other
languages easy.

The system is designed in optional layers, beginning with a VM based
on a small set of opcodes, a set of primitives implemented in C, a
default language, a module system implementation, and a set of
standard modules.  You can choose whichever layer suits your needs
best and customize the rest.  Adding your own primitives or wrappers
around existing C libraries is easy with the C FFI.

Chibi is known to build and run on 32 and 64-bit Linux, OS X, Windows
(under cygwin) and Plan9.

@section{Installation}

To build, just run "make".  This will provide a shared library
"libchibi-scheme", as well as a sample "chibi-scheme" command-line
repl.  The "chibi-scheme-static" make target builds an equivalent
static executable.  If your make doesn't support GNU make
conditionals, then you'll need to edit the top of the Makefile to
choose the appropriate settings.  On Plan9 just run "mk".

You can edit the file chibi/features.h for a number of settings,
mostly disabling features to make the executable smaller.  You can
specify standard options directly as arguments to make, for example

@command{make CFLAGS=-Os CPPFLAGS=-DSEXP_USE_NO_FEATURES=1}

to optimize for size, or

@command{make LDFLAGS=-L/usr/local/lib CPPFLAGS=-I/usr/local/include}

to compile against a library installed in /usr/local.

By default Chibi uses a custom, precise, non-moving GC.  You can link
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
make cleaner
make chibi-scheme-static SEXP_USE_DL=0 CPPFLAGS=-DSEXP_USE_STATIC_LIBS
}

@section{Default Language}

@subsection{Scheme Standard}

The default language is mostly compatible with the R5RS and R7RS, with all
differences made by design, not through difficulty of implementation.

The following procedures are omitted:

@itemlist[
@item{@scheme{transcript-on} - removed from R7RS}
@item{@scheme{transcript-off} - removed from R7RS}
@item{@scheme{rationalize} - pending the addition of rational numbers}
]

@margin-note{Exact non-integer rationals are planned for a future release.}
Apart from this, the reader defaults case-sensitive, like R6RS and R7RS but
unlike the R5RS.  The default configuration includes fixnums, flonums and
bignums but no exact rationals or complex numbers.

Full continuations are supported, but currently continuations don't
take C code into account.  This means that you can call from Scheme to
C and then from C to Scheme again, but continuations passing through
this chain may not do what you expect.  The only higher-order C
functions (thus potentially running afoul of this) in the standard
environment are @scheme{load} and @scheme{eval}.

@scheme{load} is extended to accept an optional environment argument, like
@scheme{eval}.  You can also @scheme{load} shared libraries in addition to
Scheme source files - in this case the function @cfun{sexp_init_library} is
automatically called with the following signature:

@ccode{
  sexp_init_library(sexp context, sexp environment);
}

The following additional procedures are available in the default
environment:

@itemlist[
@item{@scheme{(current-error-port)} - bound to stderr}
@item{@scheme{(flush-output [port])} - flushes any buffered output to the port}
@item{@scheme{(string-concatenate list-of-strings [sep])} - append the strings joined by @var{sep}}
]

@subsection{Module System}

A configurable module system, in the style of the Scheme48 module
system, is provided if you choose to use it.

Modules names are hierarchical lists of symbols or numbers.  The definition of
the module @scheme{(foo bar baz)} is searched for in the file
foo/bar/baz.module.  This file should contain an expression of the form:

@schemeblock{
  (module (foo bar baz)
    <module-declarations> ...)
}

where @var{<module-declarations>} can be any of

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

The can be composed and perform basic selection and renaming of
individual identifiers from the given module.

Files are loaded relative to the .module file, and are written with
their extension (so you can use whatever suffix you prefer - .scm,
.ss, .sls, etc.).

Shared modules, on the other hand, should be specified @emph{without} the
extension - the correct suffix will be added portably (e.g. .so for Unix and
.dylib for OS X).

You may also use @scheme{cond-expand} and arbitrary macro expansions in a
module definition to generate @var{<module-declarations>}.

@subsection{Macro System}

@scheme{syntax-rules} macros are provided by default, with the extensions from
SRFI-46.  In addition, low-level hygienic macros are provided with a
syntactic-closures interface, including @scheme{sc-macro-transformer},
@scheme{rsc-macro-transformer}, and @scheme{er-macro-transformer}.  A good
introduction to syntactic-closures can be found at
@url{http://community.schemewiki.org/?syntactic-closures}

@scheme{identifier?}, @scheme{identifier->symbol}, @scheme{identifier=?}, and
@scheme{make-syntactic-closure} and @scheme{strip-syntactic-closures} are
available.

@subsection{Types}

You can define new record types with SRFI-9, or inherited record types
with SRFI-99.  These are just syntactic sugar for the following more
primitive type constructors:

@schemeblock{
(register-simple-type <name-string> <num-fields>)
 => <type>

(make-type-predicate <opcode-name-string> <type-id>)
 => <opcode>  ; takes 1 arg, returns #t iff that arg is of the type

(make-constructor <constructor-name-string> <type-id>)
 => <opcode>  ; takes 0 args, returns a newly allocated instance of type

(make-getter <getter-name-string> <type-id> <field-index>)
 => <opcode>  ; takes 1 args, retrieves the field located at the index

(make-setter <setter-name-string> <type-id> <field-index>)
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
  sexp_load(ctx, "/path/to/source/file.scm", NULL);

  /* eval a C string as Scheme code */
  sexp_eval_string(ctx, "(some scheme expression)", NULL);

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
@var{version}, which currently should be the value @var{SEXP_SEVEN}.
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
@item{@ccode{sexp_string_length(str)} - the byte length of @var{str} as an int}
@item{@ccode{sexp_string_ref(str, i)} - the @var{i}'th byte of string @var{str}}
@item{@ccode{sexp_string_set(str, i, ch)} - set the @var{i}'th byte of string @var{str}}
@item{@ccode{sexp_vector_length(vec)} - the length of @var{vec} as an int}
@item{@ccode{sexp_vector_ref(vec, i)} - the @var{i}'th object of vector @var{vec}}
@item{@ccode{sexp_vector_set(vec, i, obj)} - set the @var{i}'th object of vector @var{vec}}
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

@subsection{Struct Interface}

@schemeblock{
(define-c-struct struct-name
  [predicate: predicate-name]
  [constructor: constructor-name]
  [finalizer: c_finalizer_name]
  (type c_field_name getter-name setter-name) ...)
}

@subsection{Function Interface}

@scheme{(define-c return-type name-spec (arg-type ...))}

where name-space is either a symbol name, or a list of
(scheme-name c_name).  If just a symbol, the C name is taken
to be the same with -'s replaced by _'s.

@var{arg-type} is a type suitable for input validation and conversion.

@subsection{C Types}

@subsubsection{Basic Types}

@itemlist[
@item{@ccode{void}}
@item{@ccode{boolean}}
@item{@ccode{char}}
@item{@ccode{sexp} (no conversions)}
]

@subsubsection{Integer Types}

@itemlist[
@item{@ccode{signed-char}}
@item{@ccode{short}}
@item{@ccode{int}}
@item{@ccode{long}}
@item{@ccode{unsigned-char}}
@item{@ccode{unsigned-short}}
@item{@ccode{unsigned-int}}
@item{@ccode{unsigned-long}}
@item{@ccode{size_t}}
@item{@ccode{pid_t}}
@item{@ccode{uid_t}}
@item{@ccode{gid_t}}
@item{@ccode{time_t} (in seconds, but using the chibi epoch of 2010/01/01)}
@item{@ccode{errno} (as a return type returns @scheme{#f} on error)}
]

@subsubsection{Float Types}

@itemlist[
@item{@ccode{float}}
@item{@ccode{double}}
@item{@ccode{long-double}}
]

@subsubsection{String Types}

@itemlist[
@item{@ccode{string} - a null-terminated char*}
@item{@ccode{env-string} - a @ccode{VAR=VALUE} string represented as a @scheme{(VAR . VALUE)} pair in Scheme}
@item{@scheme{(array char)} is equivalent to @ccode{string}}
]

@subsubsection{Port Types}

@itemlist[
@item{@ccode{input-port}}
@item{@ccode{output-port}}
]

@subsubsection{Struct Types}

Struct types are by default just referred to by the bare
struct-name from define-c-struct, and it is assumed you want a
pointer to that type.  To refer to the full struct, use the struct
modifier, as in (struct struct-name).

@subsubsection{Type modifiers}

Any type may also be written as a list of modifiers followed by the
type itself.  The supported modifiers are:

@itemlist[

@item{@ccode{const}
@p{Prepends the "const" C type modifier.
As a return or result parameter, makes non-immediates immutable.}}

@item{@ccode{free}
@p{It's Scheme's responsibility to "free" this resource.
As a return or result parameter, registers the freep flag
this causes the type finalizer to be run when GCed.}}

@item{@ccode{maybe-null}
@p{This pointer type may be NULL.
As a result parameter, NULL is translated to #f
normally this would just return a wrapped NULL pointer.
As an input parameter, #f is translated to NULL
normally this would be a type error.}}

@item{@ccode{pointer}
@p{Create a pointer to this type.
As a return parameter, wraps the result in a vanilla cpointer.
As a result parameter, boxes then unboxes the value.}}

@item{@ccode{struct}
@p{Treat this struct type as a struct, not a pointer.
As an input parameter, dereferences the pointer.
As a type field, indicates a nested struct.}}

@item{@ccode{link}
@p{Add a gc link.
As a field getter, link to the parent object, so the
parent won't be GCed so long as we have a reference
to the child.  This behavior is automatic for nested structs.}}

@item{@ccode{result}
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

@subsection{Internals Interface}

The @scheme{(chibi ast)} module provides access to the Abstract Syntax Tree and
other internal data structures not typically needed for everyday programs.

@subsection{Disassember}

The @scheme{(chibi disasm)} module provides a disassembler for the virtual
machine.

@subsection{Printed Equivalence}

The @scheme{(chibi equiv)} module provides the @scheme{equiv?} procedure, which
is similar to @scheme{equal?} but is guaranteed to terminate.  It takes two
arguments and returns true iff they would print the same using SRFI-38 style
read/write syntax.

@subsection{Filesystem Interface}

The @scheme{(chibi filesystem)} module provides access to the filesystem and
file descriptor objects.

@subsection{Generic Functions}

The @scheme{(chibi generic)} module provides generic methods for CLOS-style
object oriented programming.

@subsection{Heap Introspection}

The @scheme{(chibi heap-stats)} module provides utilities for gathering
statistics on the heap.

@subsection{Input/Output Extensions}

The @scheme{(chibi io)} module provides various I/O extensions, including
custom ports.

@subsection{Extensible Loop Syntax}

The @scheme{(chibi loop)} module provides a fast and extensible loop syntax.

@subsection{Pattern-Matching}

The @scheme{(chibi match)} module provides an intuitive and widely supported
pattern matching syntax.

@subsection{RFC2045 MIME}

The @scheme{(chibi mime)} module

@subsection{Module Introspection}

The @scheme{(chibi modules)} module provides introspection utilities for the
module system itself.

@subsection{Networking Interface}

The @scheme{(chibi net)} module provides a simple networking interface.

@subsection{Pathname Utilities}

The @scheme{(chibi pathname)} module provides utilities to decompose and
manipulate pathnames.

@subsection{Processes and Signals}

The @scheme{(chibi process)} module provides utilities to spawn processes and
send and handle signals between processes.

@subsection{Read/Eval/Print Loop}

The @scheme{(chibi repl)} module provides a more full-featured repl than the
chibi-scheme executable.

@subsection{Scribble Syntax}

The @scheme{(chibi scribble)} module provides a parser for the scribble syntax
used to write this manual.

@subsection{Stty Interface}

The @scheme{(chibi stty)} provides a high-level interface to ioctl.

@subsection{System Information}

The @scheme{(chibi system)} module provides access to the host system and
current user information.

@subsection{Line Editing}

The @scheme{(chibi term edit-line)} provides an @scheme{edit-line} procedure
for interactive line editing.

@subsection{Times and Dates}

The @scheme{(chibi time)} provides an interface to the current system time.

@subsection{Type Inference}

The @scheme{(chibi type-inference)} is an easy-to-use type inference system.

@subsection{URI Utilities}

The @scheme{(chibi uri)} module provides utilities to parse and construct URIs.

@subsection{Weak References}

The @scheme{(chibi weak)} module provides data structures with weak references.

