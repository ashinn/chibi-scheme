# ![Chibi-Scheme](https://goo.gl/ZDtn4q)

**Minimal Scheme Implementation for use as an Extension Language**

http://synthcode.com/wiki/chibi-scheme

Chibi-Scheme is a very small library intended for use as an extension
and scripting language in C programs.  In addition to support for
lightweight VM-based threads, each VM itself runs in an isolated heap
allowing multiple VMs to run simultaneously in different OS threads.

There are no external dependencies so is relatively easy to drop into
any project.

Despite the small size, Chibi-Scheme attempts to do The Right Thing.
The default settings include:

* a full numeric tower, with rational and complex numbers
* full and seemless Unicode support
* low-level and high-level hygienic macros
* an extensible module system

Specifically, the default repl language contains all bindings from
[R7RS small](https://small.r7rs.org/), available explicitly as the
`(scheme small)` library.  The language is built in layers, however -
see the manual for instructions on compiling with fewer features or
requesting a smaller language on startup.

Chibi-Scheme is known to work on **32** and **64-bit** Linux, FreeBSD,
NetBSD, OpenBSD and OS X, Plan 9, Windows, iOS, Android,
ARM and [Emscripten](https://kripken.github.io/emscripten-site).  Basic
support for native Windows desktop also exists. See README-win32.md
for details and build instructions.

To build on most platforms just run `make && make test`.  This has a
few conditionals assuming GNU make.  If using another make, there are
a few parameters in Makefile.detect you need to set by hand.

This will provide a shared library *libchibi-scheme*, as well as a
sample *chibi-scheme* command-line repl.  You can then run

    sudo make install

to install the binaries and libraries.  You can optionally specify a
**PREFIX** for the installation directory:

    make PREFIX=/path/to/install/
    sudo make PREFIX=/path/to/install/ install

By default files are installed in **/usr/local**.

If you want to try out chibi-scheme without installing, be sure to set
`LD_LIBRARY_PATH` so it can find the shared libraries.

For more detailed documentation, run `make doc` and see the generated
*doc/chibi.html*.
