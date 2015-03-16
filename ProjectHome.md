Chibi-Scheme is a very small library with no external dependencies
intended for use as an extension and scripting language in C programs.
In addition to support for lightweight VM-based threads, each VM itself
runs in an isolated heap allowing multiple VMs to run simultaneously in
different OS threads.  The default language is R7RS Scheme, including
first-class continuations and low-and-high-level hygienic macros.

The current release is 0.7.2, "Nitrogen":

> http://abrek.synthcode.com/chibi-scheme-0.7.2.tgz

This includes the following changes:

  * R7RS now the default language
  * Initial Snow2 package manager
  * Faster bignum arithmetic
  * Scheme Regular Expression support (SRFI 115)
  * Flexible and memoizable parse combinator library
  * Interning and smarter handling of file descriptors
  * Many bugfixes and new tests

and many others.  Most features are optional at compile time.  A
full-featured build produces a library around 200kb, whereas a
minimal build can be under 80kb.

Chibi-Scheme is known to work on 32 and 64-bit Linux, FreeBSD and
OS X, Plan9, Windows (using Cygwin) and iOS.

See the user manual for more details:

> http://synthcode.com/scheme/chibi/

Future releases should come at a faster pace now.  Near-term goals
include native x86 support, batch compilation, and alternate language
support.