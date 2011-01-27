# -*- makefile-gmake -*-

.PHONY: all libs doc dist clean cleaner dist-clean install uninstall test checkdefs
.PRECIOUS: %.c

# install configuration

CC       ?= cc
PREFIX   ?= /usr/local
BINDIR   ?= $(PREFIX)/bin
LIBDIR   ?= $(PREFIX)/lib
SOLIBDIR ?= $(PREFIX)/lib
INCDIR   ?= $(PREFIX)/include/chibi
MODDIR   ?= $(PREFIX)/share/chibi
LIBDIR   ?= $(PREFIX)/lib/chibi
MANDIR   ?= $(PREFIX)/share/man/man1

DESTDIR  ?=

GENSTUBS  ?= ./tools/genstubs.scm
GENSTATIC ?= ./tools/genstatic.scm

########################################################################
# system configuration - if not using GNU make, set PLATFORM and the
# following flags as necessary.

# 
LIBDL = -ldl

ifndef PLATFORM
ifeq ($(shell uname),Darwin)
PLATFORM=macosx
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
PLATFORM=unix
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
endif
endif
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
XLDFLAGS  := $(LDFLAGS) $(GCLDFLAGS) -lm
XCFLAGS   := -Wall -DSEXP_USE_DL=0 -g3 $(CFLAGS)
else
XLDFLAGS  := $(LDFLAGS) $(GCLDFLAGS) $(LIBDL) -lm
XCFLAGS   := -Wall -g3 $(CFLAGS)
endif

########################################################################

all: chibi-scheme$(EXE) libs lib/chibi/ast$(SO)

CHIBI ?= LD_LIBRARY_PATH=".:$(LD_LIBRARY_PATH)" ./chibi-scheme$(EXE)

COMPILED_LIBS := lib/srfi/18/threads$(SO) lib/srfi/27/rand$(SO) \
	lib/srfi/33/bit$(SO) lib/srfi/39/param$(SO) lib/srfi/69/hash$(SO) \
	lib/srfi/95/qsort$(SO) lib/srfi/98/env$(SO) lib/chibi/net$(SO) \
	lib/chibi/filesystem$(SO) lib/chibi/process$(SO) lib/chibi/time$(SO) \
	lib/chibi/system$(SO) lib/chibi/io/io$(SO) lib/chibi/stty$(SO) \
	lib/chibi/weak$(SO) lib/chibi/heap-stats$(SO) lib/chibi/disasm$(SO)

libs: $(COMPILED_LIBS)

INCLUDES = include/chibi/sexp.h include/chibi/features.h include/chibi/install.h include/chibi/bignum.h

include/chibi/install.h: Makefile
	echo '#define sexp_so_extension "'$(SO)'"' > $@
	echo '#define sexp_default_module_dir "'$(MODDIR)'"' >> $@
	echo '#define sexp_platform "'$(PLATFORM)'"' >> $@
	echo '#define sexp_version "'`cat VERSION`'"' >> $@
	echo '#define sexp_release_name "'`cat RELEASE`'"' >> $@

sexp.o: sexp.c gc.c opt/bignum.c $(INCLUDES) Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -o $@ $<

eval.o: eval.c opcodes.c vm.c opt/simplify.c $(INCLUDES) include/chibi/eval.h Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -o $@ $<

main.o: main.c $(INCLUDES) include/chibi/eval.h Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) -o $@ $<

libchibi-sexp$(SO): sexp.o
	$(CC) $(CLIBFLAGS) -o $@ $^ $(XLDFLAGS)

libchibi-scheme$(SO): eval.o sexp.o
	$(CC) $(CLIBFLAGS) -o $@ $^ $(XLDFLAGS)

chibi-scheme$(EXE): main.o libchibi-scheme$(SO)
	$(CC) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. -lchibi-scheme

chibi-scheme-static$(EXE): main.o eval.o sexp.o
	$(CC) $(XCFLAGS) $(STATICFLAGS) -o $@ $^ $(LDFLAGS) $(GCLDFLAGS) -lm

clibs.c: $(GENSTATIC) lib lib/chibi lib/srfi chibi-scheme$(EXE) libs
	$(CHIBI) $< > $@

%.c: %.stub $(GENSTUBS) chibi-scheme$(EXE)
	-$(CHIBI) $(GENSTUBS) $<

lib/chibi/ast$(SO): lib/chibi/ast.c $(INCLUDES)
	-$(CC) $(CLIBFLAGS) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< $(GCLDFLAGS) -L. -lchibi-scheme

lib/%$(SO): lib/%.c $(INCLUDES)
	-$(CC) $(CLIBFLAGS) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. -lchibi-scheme

clean:
	rm -f *.o *.i *.s *.8
	rm -f tests/basic/*.out tests/basic/*.err

cleaner: clean
	rm -f chibi-scheme$(EXE) chibi-scheme-static$(EXE) *.a include/chibi/install.h
	find lib -name \*$(SO) -exec rm -rf '{}' \;

dist-clean: cleaner
	for f in `find lib -name \*.stub`; do rm -f $${f%.stub}.c; done

checkdefs:
	@for d in $(D); do \
	    if ! grep -q " SEXP_USE_$${d%%=*} " include/chibi/features.h; then \
	        echo "WARNING: unknown definition $$d"; \
	    fi; \
	done

test-basic: chibi-scheme$(EXE)
	@for f in tests/basic/*.scm; do \
	    $(CHIBI) $$f >$${f%.scm}.out 2>$${f%.scm}.err; \
	    if diff -q $(DIFFOPTS) $${f%.scm}.out $${f%.scm}.res; then \
	        echo "[PASS] $${f%.scm}"; \
	    else \
	        echo "[FAIL] $${f%.scm}"; \
	    fi; \
	done

test-build:
	./tests/build/build-tests.sh

test-threads: chibi-scheme$(EXE) lib/srfi/18/threads$(SO) lib/srfi/39/param$(SO) lib/srfi/98/env$(SO) lib/chibi/ast$(SO) lib/chibi/time$(SO)
	$(CHIBI) tests/thread-tests.scm

test-numbers: chibi-scheme$(EXE)
	$(CHIBI) tests/numeric-tests.scm

test-flonums: chibi-scheme$(EXE)
	$(CHIBI) tests/flonum-tests.scm

test-hash: chibi-scheme$(EXE) lib/srfi/69/hash$(SO)
	$(CHIBI) tests/hash-tests.scm

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

install: chibi-scheme$(EXE)
	mkdir -p $(DESTDIR)$(BINDIR)
	cp chibi-scheme$(EXE) $(DESTDIR)$(BINDIR)/
	cp tools/genstubs.scm $(DESTDIR)$(BINDIR)/
	mkdir -p $(DESTDIR)$(MODDIR)
	cp -r lib/* $(DESTDIR)$(MODDIR)/
	mkdir -p $(DESTDIR)$(INCDIR)
	cp $(INCLUDES) include/chibi/eval.h $(DESTDIR)$(INCDIR)/
	mkdir -p $(DESTDIR)$(LIBDIR)
	mkdir -p $(DESTDIR)$(SOLIBDIR)
	cp libchibi-scheme$(SO) $(DESTDIR)$(SOLIBDIR)/
	cp libchibi-scheme$(SO) $(DESTDIR)$(SOLIBDIR)/
	-cp libchibi-scheme.a $(DESTDIR)$(LIBDIR)/
	mkdir -p $(DESTDIR)$(MANDIR)
	cp doc/chibi-scheme.1 $(DESTDIR)$(MANDIR)/
	-if type ldconfig >/dev/null 2>/dev/null; then ldconfig; fi

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/chibi-scheme$(EXE)
	rm -f $(DESTDIR)$(BINDIR)/chibi-scheme-static$(EXE)
	rm -f $(DESTDIR)$(SOLIBDIR)/libchibi-scheme$(SO)
	rm -f $(DESTDIR)$(LIBDIR)/libchibi-scheme$(SO).a
	cd $(DESTDIR)$(INCDIR) && rm -f $(INCLUDES) include/chibi/eval.h
	rm -rf $(DESTDIR)$(MODDIR)

dist: dist-clean
	rm -f chibi-scheme-`cat VERSION`.tgz
	mkdir chibi-scheme-`cat VERSION`
	for f in `hg manifest`; do mkdir -p chibi-scheme-`cat VERSION`/`dirname $$f`; ln -s `pwd`/$$f chibi-scheme-`cat VERSION`/$$f; done
	tar cphzvf chibi-scheme-`cat VERSION`.tgz chibi-scheme-`cat VERSION`
	rm -rf chibi-scheme-`cat VERSION`

mips-dist: dist-clean
	rm -f chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`.tgz
	mkdir chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
	for f in `hg manifest`; do mkdir -p chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`/`dirname $$f`; ln -s `pwd`/$$f chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`/$$f; done
	tar cphzvf chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`.tgz chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
	rm -rf chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
