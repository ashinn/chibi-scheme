# -*- makefile-gmake -*-

.PHONY: all libs dist clean cleaner dist-clean install uninstall test checkdefs
.PRECIOUS: %.c

# install configuration

CC        ?= cc
CD        ?= cd
RM        ?= rm -f
LS        ?= ls
INSTALL   ?= install
MKDIR     ?= $(INSTALL) -d
RMDIR     ?= rmdir -f
TAR       ?= tar
DIFF      ?= diff
GREP      ?= grep
FIND      ?= find
SYMLINK   ?= ln -s

PREFIX    ?= /usr/local
BINDIR    ?= $(PREFIX)/bin
LIBDIR    ?= $(PREFIX)/lib
SOLIBDIR  ?= $(PREFIX)/lib
INCDIR    ?= $(PREFIX)/include/chibi
MODDIR    ?= $(PREFIX)/share/chibi
BINMODDIR ?= $(PREFIX)/lib/chibi
MANDIR    ?= $(PREFIX)/share/man/man1

DESTDIR   ?=

GENSTUBS  ?= ./tools/chibi-ffi
GENSTATIC ?= ./tools/chibi-genstatic

########################################################################
# system configuration - if not using GNU make, set PLATFORM and the
# following flags as necessary.

# 
LIBDL = -ldl

ifndef PLATFORM
ifeq ($(shell uname),Darwin)
PLATFORM=macosx
else
ifeq ($(shell uname),FreeBSD)
PLATFORM=FreeBSD
else
ifeq ($(shell uname -o),Msys)
PLATFORM=mingw
SOLIBDIR = $(BINDIR)
DIFFOPTS = -b
else
ifeq ($(shell uname -o),Cygwin)
PLATFORM=cygwin
SOLIBDIR = $(BINDIR)
DIFFOPTS = -b
else
ifeq ($(shell uname -o),GNU/Linux)
PLATFORM=linux
else
PLATFORM=unix
endif
endif
endif
endif
endif
endif

ifeq ($(PLATFORM),macosx)
SO  = .dylib
EXE =
CLIBFLAGS = -dynamiclib
STATICFLAGS = -static-libgcc -DSEXP_USE_DL=0
else
ifeq ($(PLATFORM),mingw)
SO  = .dll
EXE = .exe
CC = gcc
CLIBFLAGS = -shared
CPPFLAGS += -DSEXP_USE_STRING_STREAMS=0 -DBUILDING_DLL
LDFLAGS += -Wl,--out-implib,libchibi-scheme$(SO).a
STATICFLAGS = -DSEXP_USE_DL=0
LIBDL = 
else
ifeq ($(PLATFORM),cygwin)
SO  = .dll
EXE = .exe
CC = gcc
CLIBFLAGS = -shared
CPPFLAGS += -DSEXP_USE_STRING_STREAMS=0
LDFLAGS += -Wl,--out-implib,libchibi-scheme$(SO).a
else
SO  = .so
EXE =
CLIBFLAGS = -fPIC -shared
STATICFLAGS = -static -DSEXP_USE_DL=0
ifeq ($(PLATFORM),FreeBSD)
LIBDL=
RLDFLAGS=-Wl,-R$(DESTDIR)$(LIBDIR)
endif
endif
endif
endif

ifeq ($(PLATFORM),unix)
#RLDFLAGS=-rpath $(LIBDIR)
RLDFLAGS=-Wl,-R$(LIBDIR)
endif

ifeq ($(USE_BOEHM),1)
SEXP_USE_BOEHM = 1
endif

ifeq ($(SEXP_USE_BOEHM),1)
GCLDFLAGS := -lgc
XCPPFLAGS := $(CPPFLAGS) -Iinclude $(D:%=-DSEXP_USE_%) -DSEXP_USE_BOEHM=1
else
GCLDFLAGS :=
XCPPFLAGS := $(CPPFLAGS) -Iinclude $(D:%=-DSEXP_USE_%)
endif

ifeq ($(SEXP_USE_DL),0)
XLDFLAGS  := $(LDFLAGS) $(RLDFLAGS) $(GCLDFLAGS) -lm
XCFLAGS   := -Wall -DSEXP_USE_DL=0 -g -g3 -Os $(CFLAGS)
else
XLDFLAGS  := $(LDFLAGS) $(RLDFLAGS) $(GCLDFLAGS) $(LIBDL) -lm
XCFLAGS   := -Wall -g -g3 -Os $(CFLAGS)
endif

########################################################################

all: chibi-scheme$(EXE) libs lib/chibi/ast$(SO)

CHIBI ?= LD_LIBRARY_PATH=".:$(LD_LIBRARY_PATH)" ./chibi-scheme$(EXE)

CHIBI_COMPILED_LIBS := lib/chibi/filesystem$(SO) lib/chibi/process$(SO) \
	lib/chibi/time$(SO) lib/chibi/system$(SO) lib/chibi/stty$(SO) \
	lib/chibi/weak$(SO) lib/chibi/heap-stats$(SO) lib/chibi/disasm$(SO) \
	lib/chibi/net$(SO)
CHIBI_IO_COMPILED_LIBS := lib/chibi/io/io$(SO)
CHIBI_OPT_COMPILED_LIBS := lib/chibi/optimize/rest$(SO) \
	lib/chibi/optimize/profile$(SO)
COMPILED_LIBS := $(CHIBI_COMPILED_LIBS) $(CHIBI_IO_COMPILED_LIBS) \
	$(CHIBI_OPT_COMPILED_LIBS) lib/srfi/18/threads$(SO) \
	lib/srfi/27/rand$(SO) lib/srfi/33/bit$(SO) lib/srfi/39/param$(SO) \
	lib/srfi/69/hash$(SO) lib/srfi/95/qsort$(SO) lib/srfi/98/env$(SO) \
	lib/scheme/time$(SO)

libs: $(COMPILED_LIBS)

BASE_INCLUDES = include/chibi/sexp.h include/chibi/features.h include/chibi/install.h include/chibi/bignum.h
INCLUDES = $(BASE_INCLUDES) include/chibi/eval.h

include/chibi/install.h: Makefile
	echo '#define sexp_so_extension "'$(SO)'"' > $@
	echo '#define sexp_default_module_path "'$(MODDIR):$(BINMODDIR)'"' >> $@
	echo '#define sexp_platform "'$(PLATFORM)'"' >> $@
	echo '#define sexp_version "'`cat VERSION`'"' >> $@
	echo '#define sexp_release_name "'`cat RELEASE`'"' >> $@

sexp.o: sexp.c gc.c opt/bignum.c $(BASE_INCLUDES) Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -o $@ $<

sexp-ulimit.o: sexp.c gc.c opt/bignum.c $(BASE_INCLUDES) Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -DSEXP_USE_LIMITED_MALLOC -o $@ $<

eval.o: eval.c opcodes.c vm.c opt/simplify.c $(INCLUDES) Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -o $@ $<

main.o: main.c $(INCLUDES) Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) -o $@ $<

libchibi-sexp$(SO): sexp.o
	$(CC) $(CLIBFLAGS) -o $@ $^ $(XLDFLAGS)

libchibi-scheme$(SO): eval.o sexp.o
	$(CC) $(CLIBFLAGS) -o $@ $^ $(XLDFLAGS)

chibi-scheme$(EXE): main.o libchibi-scheme$(SO)
	$(CC) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. -lchibi-scheme

chibi-scheme-static$(EXE): main.o eval.o sexp.o
	$(CC) $(XCFLAGS) $(STATICFLAGS) -o $@ $^ $(LDFLAGS) $(GCLDFLAGS) -lm

chibi-scheme-ulimit$(EXE): main.o eval.o sexp-ulimit.o
	$(CC) $(XCFLAGS) $(STATICFLAGS) -o $@ $^ $(LDFLAGS) $(GCLDFLAGS) -lm

clibs.c: $(GENSTATIC) lib lib/chibi lib/srfi chibi-scheme$(EXE) libs
	$(CHIBI) $< > $@

%.c: %.stub $(GENSTUBS) chibi-scheme$(EXE)
	-$(CHIBI) $(GENSTUBS) $<

# A special case, this needs to be linked with the LDFLAGS in case
# we're using Boehm.
lib/chibi/ast$(SO): lib/chibi/ast.c $(INCLUDES)
	-$(CC) $(CLIBFLAGS) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< $(XLDFLAGS) -L. -lchibi-scheme

lib/%$(SO): lib/%.c $(INCLUDES)
	-$(CC) $(CLIBFLAGS) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. -lchibi-scheme

%.html: %.scrbl tools/chibi-doc chibi-scheme$(EXE)
	$(CHIBI) tools/chibi-doc $< > $@

doc/lib/chibi/%.html: lib/chibi/%.sld tools/chibi-doc chibi-scheme$(EXE)
	$(CHIBI) tools/chibi-doc chibi.$* > $@

MODULE_DOCS := ast disasm equiv filesystem generic heap-stats io loop \
	match mime modules net pathname process repl scribble stty \
	system test time trace type-inference uri weak

doc: doc/chibi.html $(MODULE_DOCS:%=doc/lib/chibi/%.html)

clean:
	$(RM) *.o *.i *.s *.8
	$(RM) tests/basic/*.out tests/basic/*.err

cleaner: clean
	$(RM) chibi-scheme$(EXE) chibi-scheme-static$(EXE) chibi-scheme-ulimit$(EXE) libchibi-scheme$(SO) *.a include/chibi/install.h
	$(FIND) lib -name \*$(SO) -exec $(RM) -r '{}' \;
	$(FIND) lib -name \*.o -exec $(RM) -r '{}' \;

dist-clean: cleaner
	for f in `find lib -name \*.stub`; do $(RM) $${f%.stub}.c; done

checkdefs:
	@for d in $(D); do \
	    if ! $(GREP) -q " SEXP_USE_$${d%%=*} " include/chibi/features.h; then \
	        echo "WARNING: unknown definition $$d"; \
	    fi; \
	done

test-basic: chibi-scheme$(EXE)
	@for f in tests/basic/*.scm; do \
	    $(CHIBI) $$f >$${f%.scm}.out 2>$${f%.scm}.err; \
	    if $(DIFF) -q $(DIFFOPTS) $${f%.scm}.out $${f%.scm}.res; then \
	        echo "[PASS] $${f%.scm}"; \
	    else \
	        echo "[FAIL] $${f%.scm}"; \
	    fi; \
	done

test-memory: chibi-scheme-ulimit$(EXE)
	./tests/memory/memory-tests.sh

test-build:
	MAKE=$(MAKE) ./tests/build/build-tests.sh

test-threads: chibi-scheme$(EXE) lib/srfi/18/threads$(SO) lib/srfi/39/param$(SO) lib/srfi/98/env$(SO) lib/chibi/ast$(SO) lib/chibi/time$(SO)
	$(CHIBI) tests/thread-tests.scm

test-numbers: chibi-scheme$(EXE)
	$(CHIBI) tests/numeric-tests.scm

test-flonums: chibi-scheme$(EXE)
	$(CHIBI) tests/flonum-tests.scm

test-hash: chibi-scheme$(EXE) lib/srfi/69/hash$(SO)
	$(CHIBI) tests/hash-tests.scm

test-io: chibi-scheme$(EXE) lib/chibi/io/io$(SO)
	$(CHIBI) tests/io-tests.scm

test-match: chibi-scheme$(EXE)
	$(CHIBI) tests/match-tests.scm

test-loop: chibi-scheme$(EXE)
	$(CHIBI) tests/loop-tests.scm

test-sort: chibi-scheme$(EXE) lib/srfi/33/bit$(SO)
	$(CHIBI) tests/sort-tests.scm

test-records: chibi-scheme$(EXE)
	$(CHIBI) tests/record-tests.scm

test-weak: chibi-scheme$(EXE) lib/chibi/weak$(SO)
	$(CHIBI) tests/weak-tests.scm

test-unicode: chibi-scheme$(EXE)
	$(CHIBI) tests/unicode-tests.scm

test-libs: chibi-scheme$(EXE)
	$(CHIBI) tests/lib-tests.scm

test: chibi-scheme$(EXE)
	$(CHIBI) tests/r5rs-tests.scm

install: all
	$(MKDIR) $(DESTDIR)$(BINDIR)
	$(INSTALL) chibi-scheme$(EXE) $(DESTDIR)$(BINDIR)/
	$(INSTALL) tools/chibi-ffi $(DESTDIR)$(BINDIR)/
	$(INSTALL) tools/chibi-doc $(DESTDIR)$(BINDIR)/
	$(MKDIR) $(DESTDIR)$(MODDIR)/chibi/io $(DESTDIR)$(MODDIR)/chibi/loop $(DESTDIR)$(MODDIR)/chibi/match $(DESTDIR)$(MODDIR)/chibi/net $(DESTDIR)$(MODDIR)/chibi/optimize $(DESTDIR)$(MODDIR)/chibi/term
	$(MKDIR) $(DESTDIR)$(MODDIR)/scheme/char
	$(MKDIR) $(DESTDIR)$(MODDIR)/srfi/1 $(DESTDIR)$(MODDIR)/srfi/18 $(DESTDIR)$(MODDIR)/srfi/27 $(DESTDIR)$(MODDIR)/srfi/33 $(DESTDIR)$(MODDIR)/srfi/39 $(DESTDIR)$(MODDIR)/srfi/69 $(DESTDIR)$(MODDIR)/srfi/95 $(DESTDIR)$(MODDIR)/srfi/99 $(DESTDIR)$(MODDIR)/srfi/99/records
	$(INSTALL) lib/*.scm $(DESTDIR)$(MODDIR)/
	$(INSTALL) lib/chibi/*.sld lib/chibi/*.scm $(DESTDIR)$(MODDIR)/chibi/
	$(INSTALL) lib/chibi/io/*.scm $(DESTDIR)$(MODDIR)/chibi/io/
	$(INSTALL) lib/chibi/loop/*.scm $(DESTDIR)$(MODDIR)/chibi/loop/
	$(INSTALL) lib/chibi/match/*.scm $(DESTDIR)$(MODDIR)/chibi/match/
	$(INSTALL) lib/chibi/net/*.sld lib/chibi/net/*.scm $(DESTDIR)$(MODDIR)/chibi/net/
	$(INSTALL) lib/chibi/optimize/*.sld lib/chibi/optimize/*.scm $(DESTDIR)$(MODDIR)/chibi/optimize/
	$(INSTALL) lib/chibi/term/*.sld lib/chibi/term/*.scm $(DESTDIR)$(MODDIR)/chibi/term/
	$(INSTALL) lib/scheme/*.sld lib/scheme/*.scm $(DESTDIR)$(MODDIR)/scheme/
	$(INSTALL) lib/scheme/char/*.sld $(DESTDIR)$(MODDIR)/scheme/char/
	$(INSTALL) lib/srfi/*.sld lib/srfi/*.scm $(DESTDIR)$(MODDIR)/srfi/
	$(INSTALL) lib/srfi/1/*.scm $(DESTDIR)$(MODDIR)/srfi/1/
	$(INSTALL) lib/srfi/18/*.scm $(DESTDIR)$(MODDIR)/srfi/18/
	$(INSTALL) lib/srfi/27/*.scm $(DESTDIR)$(MODDIR)/srfi/27/
	$(INSTALL) lib/srfi/33/*.scm $(DESTDIR)$(MODDIR)/srfi/33/
	$(INSTALL) lib/srfi/39/*.scm $(DESTDIR)$(MODDIR)/srfi/39/
	$(INSTALL) lib/srfi/69/*.scm $(DESTDIR)$(MODDIR)/srfi/69/
	$(INSTALL) lib/srfi/95/*.scm $(DESTDIR)$(MODDIR)/srfi/95/
	$(INSTALL) lib/srfi/99/*.sld $(DESTDIR)$(MODDIR)/srfi/99/
	$(INSTALL) lib/srfi/99/records/*.sld lib/srfi/99/records/*.scm $(DESTDIR)$(MODDIR)/srfi/99/records/
	$(MKDIR) $(DESTDIR)$(BINMODDIR)/chibi/io/
	$(MKDIR) $(DESTDIR)$(BINMODDIR)/chibi/optimize/
	$(MKDIR) $(DESTDIR)$(BINMODDIR)/scheme/
	$(MKDIR) $(DESTDIR)$(BINMODDIR)/srfi/18 $(DESTDIR)$(BINMODDIR)/srfi/27 $(DESTDIR)$(BINMODDIR)/srfi/33 $(DESTDIR)$(BINMODDIR)/srfi/39 $(DESTDIR)$(BINMODDIR)/srfi/69 $(DESTDIR)$(BINMODDIR)/srfi/95 $(DESTDIR)$(BINMODDIR)/srfi/98
	$(INSTALL) $(CHIBI_COMPILED_LIBS) lib/chibi/ast$(SO) $(DESTDIR)$(BINMODDIR)/chibi/
	$(INSTALL) $(CHIBI_IO_COMPILED_LIBS) $(DESTDIR)$(BINMODDIR)/chibi/io/
	$(INSTALL) $(CHIBI_OPT_COMPILED_LIBS) $(DESTDIR)$(BINMODDIR)/chibi/optimize/
	$(INSTALL) lib/scheme/time$(SO) $(DESTDIR)$(BINMODDIR)/scheme/
	$(INSTALL) lib/srfi/18/threads$(SO) $(DESTDIR)$(BINMODDIR)/srfi/18
	$(INSTALL) lib/srfi/27/rand$(SO) $(DESTDIR)$(BINMODDIR)/srfi/27
	$(INSTALL) lib/srfi/33/bit$(SO) $(DESTDIR)$(BINMODDIR)/srfi/33
	$(INSTALL) lib/srfi/39/param$(SO) $(DESTDIR)$(BINMODDIR)/srfi/39
	$(INSTALL) lib/srfi/69/hash$(SO) $(DESTDIR)$(BINMODDIR)/srfi/69
	$(INSTALL) lib/srfi/95/qsort$(SO) $(DESTDIR)$(BINMODDIR)/srfi/95
	$(INSTALL) lib/srfi/98/env$(SO) $(DESTDIR)$(BINMODDIR)/srfi/98
	$(MKDIR) $(DESTDIR)$(INCDIR)
	$(INSTALL) $(INCLUDES) $(DESTDIR)$(INCDIR)/
	$(MKDIR) $(DESTDIR)$(LIBDIR)
	$(MKDIR) $(DESTDIR)$(SOLIBDIR)
	$(INSTALL) libchibi-scheme$(SO) $(DESTDIR)$(SOLIBDIR)/
	-$(INSTALL) libchibi-scheme.a $(DESTDIR)$(SOLIBDIR)/
	$(MKDIR) $(DESTDIR)$(MANDIR)
	$(INSTALL) doc/chibi-scheme.1 $(DESTDIR)$(MANDIR)/
	-if type ldconfig >/dev/null 2>/dev/null; then ldconfig; fi

uninstall:
	-$(RM) $(DESTDIR)$(BINDIR)/chibi-scheme$(EXE)
	-$(RM) $(DESTDIR)$(BINDIR)/chibi-scheme-static$(EXE)
	-$(RM) $(DESTDIR)$(SOLIBDIR)/libchibi-scheme$(SO)
	-$(RM) $(DESTDIR)$(LIBDIR)/libchibi-scheme$(SO).a
	-$(CD) $(DESTDIR)$(INCDIR) && $(RM) $(INCLUDES)
	-$(RM) $(DESTDIR)$(MODDIR)/*.{sld,scm} $(DESTDIR)$(MODDIR)/*/*.{sld,scm} $(DESTDIR)$(MODDIR)/*/*/*.{sld,scm}
	-$(CD) $(DESTDIR)$(BINMODDIR) && $(RM) $(COMPILED_LIBS:lib/%=%) chibi/ast$(SO)
	-if [ -d $(DESTDIR)$(BINMODDIR) ] && ! $(LS) -A $(DESTDIR)$(BINMODDIR) | $(GREP) -q -E .; then $(RMDIR) $(DESTDIR)$(BINMODDIR); fi

dist: dist-clean
	$(RM) chibi-scheme-`cat VERSION`.tgz
	$(MKDIR) chibi-scheme-`cat VERSION`
	@for f in `hg manifest`; do $(MKDIR) chibi-scheme-`cat VERSION`/`dirname $$f`; $(SYMLINK) `pwd`/$$f chibi-scheme-`cat VERSION`/$$f; done
	$(TAR) cphzvf chibi-scheme-`cat VERSION`.tgz chibi-scheme-`cat VERSION`
	$(RM) -r chibi-scheme-`cat VERSION`

mips-dist: dist-clean
	$(RM) chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`.tgz
	$(MKDIR) chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
	@for f in `hg manifest`; do $(MKDIR) chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`/`dirname $$f`; $(SYMLINK) `pwd`/$$f chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`/$$f; done
	$(TAR) cphzvf chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`.tgz chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
	$(RM) -r chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
