Chibi-scheme for Windows
========================

Chibi-scheme provides limited support for native desktop Windows. To use 
fully-featured Chibi-scheme on Windows, consider using POSIX layer such as
Windows Subsytem for Linux(WSL), Cygwin or MSYS.

Currently, only R7RS Small libraries are available for the platform. 

Supported Environments
----------------------

Chibi-scheme can be compiled with following platforms:

* Microsoft Visual Studio 2017
* MinGW32
* MinGW64
* MSYS


Known Issues
------------

Following libraries are not ported yet:

* `(chibi net)`
* `(chibi process)` : `exit` is available through `(scheme process-context)`
* `(chibi stty)`
* `(chibi system)`
* `(chibi time)`

Following library is not completely ported:

* `(chibi filesystem)`

Other issues:

* SRFI-27: Due to C Runtime limitation, the library is not thread-safe
* `make install` is not supported on Windows platforms
* On MSVC, flonum precision is degraded when compared with other compilers
* Cross compilation is not supported


Build with MinGW(Makefile)
--------------------------

The top-level `Makefile` can be used with MinGW.

1. Open MinGW64 or MinGW32 command prompt
2. `make`
3. `make test`

Currently, `make doc` is not supported on these platforms.


Build with MSYS(Makefile)
-------------------------

By default, the Makefile will compile against native Windows API. To use
MSYS's own POSIX emulation layer, specify `PLATFORM=msys`.

1. Open MSYS command prompt
2. `make PLATFORM=msys`
3. `make PLATFORM=msys test`


Build with Visual Studio(CMake)
-------------------------------

Minimal `CMakeLists.txt` is provided as an example to build Chibi-scheme on
Windows platforms. This is only intended to be used with Windows platforms;
currently it does not provide features provided with standard `Makefile` nor
it does not support UNIX/APPLE platforms either.

1. (Make sure CMake was selected with Visual Studio installer)
2. Open this directory with "Open with Visual Studio"
3. Choose "x86-" or "x64-" configuration
4. "CMake" => "Build all"
5. "CMake" => "Tests" => "Run chibi-scheme Tests"


