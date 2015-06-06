# ![Chibi-Scheme](http://synthcode.com/images/chibi-scheme-med.png)

Minimal Scheme Implementation for use as an Extension Language

http://synthcode.com/wiki/chibi-scheme

Chibi-Scheme is a very small library intended for use as an extension
and scripting language in C programs.  In addition to support for
lightweight VM-based threads, each VM itself runs in an isolated heap
allowing multiple VMs to run simultaneously in different OS threads.

The default repl language contains all bindings from
[R7RS small](http://trac.sacrideo.us/wg/wiki/R7RSHomePage),
available explicitly as the `(scheme small)` library.

Support for additional languages such as JavaScript, Go, Lua and Bash
are planned for future releases.  Scheme is chosen as a substrate
because its first class continuations and guaranteed tail-call
optimization makes implementing other languages easy.

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
