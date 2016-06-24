# ![Chibi-Scheme](https://goo.gl/ZDtn4q)

**Minimal Scheme Implementation for use as an Extension Language**

http://synthcode.com/wiki/chibi-scheme

Chibi-Scheme is a very small library intended for use as an extension
and scripting language in C programs.  In addition to support for
lightweight VM-based threads, each VM itself runs in an isolated heap
allowing multiple VMs to run simultaneously in different OS threads.

There are no external dependencies so is relatively easy to drop into
any project.

The default repl language contains all bindings from
[R7RS small](http://trac.sacrideo.us/wg/wiki/R7RSHomePage),
available explicitly as the `(scheme small)` library.  The
language is built in layers, however - see the manual for
instructions on compiling with fewer features or requesting
a smaller language on startup.

Chibi-Scheme is known to work on **32** and **64-bit** Linux,
FreeBSD and OS X, Plan9, Windows (using Cygwin), iOS, Android,
Arm and [Emscripten](https://kripken.github.io/emscripten-site).

To build on most platforms just run `make && make test`.  This will
provide a shared library *libchibi-scheme*, as well as a sample
*chibi-scheme* command-line repl.  You can then run

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
