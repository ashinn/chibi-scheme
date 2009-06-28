# -*- makefile-gmake -*-

.PHONY: all doc dist clean cleaner test install uninstall

all: chibi-scheme

CC     ?= cc
PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
LIBDIR ?= $(PREFIX)/lib
INCDIR ?= $(PREFIX)/include/chibi
MODDIR ?= $(PREFIX)/share/chibi

ifndef PLATFORM
ifeq ($(shell uname),Darwin)
PLATFORM=macosx
else
PLATFORM=unix
endif
endif

ifeq ($(PLATFORM),macosx)
SO  = .dylib
EXE =
CLIBFLAGS = -dynamiclib
STATICFLAGS = -static-libgcc
else
ifeq ($(PLATFORM),mingw)
SO  = .dll
EXE = .exe
CLIBFLAGS = -fPIC -shared
else
SO  = .so
EXE =
CLIBFLAGS = -fPIC -shared
STATICFLAGS = -static
endif
endif

ifdef USE_BOEHM
GCLDFLAGS := -lgc
XCPPFLAGS := $(CPPFLAGS) -Iinclude -DUSE_BOEHM=1
else
GCLDFLAGS :=
XCPPFLAGS := $(CPPFLAGS) -Iinclude
endif

XLDFLAGS  := $(LDFLAGS) $(GCLDFLAGS) -lm
XCFLAGS   := -Wall -g $(CFLAGS)

INCLUDES = include/chibi/sexp.h include/chibi/config.h include/chibi/install.h

include/chibi/install.h: Makefile
	echo '#define sexp_module_dir "'$(MODDIR)'"' > $@

sexp.o: sexp.c gc.c $(INCLUDES) Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -o $@ $<

eval.o: eval.c debug.c opcodes.c include/chibi/eval.h $(INCLUDES) Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -o $@ $<

main.o: main.c $(INCLUDES) Makefile
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) -o $@ $<

libchibi-scheme$(SO): eval.o sexp.o
	$(CC) $(CLIBFLAGS) -o $@ $^ $(XLDFLAGS)

chibi-scheme$(EXE): main.o libchibi-scheme$(SO)
	$(CC) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. -lchibi-scheme

chibi-scheme-static$(EXE): main.o eval.o sexp.o
	$(CC) $(XCFLAGS) $(STATICFLAGS) -o $@ $^ $(XLDFLAGS)

clean:
	rm -f *.o *.i *.s

cleaner: clean
	rm -f chibi-scheme chibi-scheme-static *$(SO)
	rm -rf *.dSYM

test-basic: chibi-scheme
	@for f in tests/basic/*.scm; do \
	    ./chibi-scheme $$f >$${f%.scm}.out 2>$${f%.scm}.err; \
	    if diff -q $${f%.scm}.out $${f%.scm}.res; then \
	        echo "[PASS] $${f%.scm}"; \
	    else \
	        echo "[FAIL] $${f%.scm}"; \
	    fi; \
	done

test: chibi-scheme
	./chibi-scheme tests/r5rs-tests.scm

install: chibi-scheme
	mkdir -p $(BINDIR)
	cp chibi-scheme $(BINDIR)/
	mkdir -p $(MODDIR)
	cp init.scm $(MODDIR)/
	mkdir -p $(INCDIR)
	cp $(INCLUDES) include/chibi/eval.h $(INCDIR)/
	mkdir -p $(LIBDIR)
	cp libchibi-scheme$(SO) $(LIBDIR)/
	if type ldconfig >/dev/null 2>/dev/null; then ldconfig; fi

uninstall:
	rm -f $(BINDIR)/chibi-scheme*
	rm -f $(LIBDIR)/libchibi-scheme$(SO)
	cd $(INCDIR) && rm -f $(INCLUDES) include/chibi/eval.h
	rm -f $(MODDIR)/*.scm

dist: cleaner
	rm -f chibi-scheme-`cat VERSION`.tgz
	mkdir chibi-scheme-`cat VERSION`
	for f in `hg manifest`; do mkdir -p chibi-scheme-`cat VERSION`/`dirname $$f`; ln -s `pwd`/$$f chibi-scheme-`cat VERSION`/$$f; done
	tar cphzvf chibi-scheme-`cat VERSION`.tgz chibi-scheme-`cat VERSION`
	rm -rf chibi-scheme-`cat VERSION`
