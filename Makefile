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
else ifeq ($(PLATFORM),mingw)
SO  = .dll
EXE = .exe
CLIBFLAGS = -fPIC shared
else
SO  = .so
EXE =
CLIBFLAGS = -fPIC -shared
STATICFLAGS = -static
endif

ifdef USE_BOEHM
GCLDFLAGS := -lgc
else
GCLDFLAGS :=
endif

LDFLAGS  := $(LDFLAGS) -lm
CPPFLAGS := $(CPPFLAGS) -Iinclude
CFLAGS   := $(CFLAGS) -Wall -O2 -g

INCLUDES = include/chibi/sexp.h include/chibi/config.h include/chibi/install.h

include/chibi/install.h: Makefile
	echo '#define sexp_module_dir "'$(MODDIR)'"' > $@

sexp.o: sexp.c gc.c $(INCLUDES) Makefile
	$(CC) -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

eval.o: eval.c debug.c opcodes.c include/chibi/eval.h $(INCLUDES) Makefile
	$(CC) -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

main.o: main.c $(INCLUDES) Makefile
	$(CC) -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

libchibi-scheme$(SO): eval.o sexp.o
	$(CC) -dynamiclib -o $@ $^

chibi-scheme$(EXE): main.o libchibi-scheme$(SO)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $< $(LDFLAGS) $(GCLDFLAGS) -L. -lchibi-scheme

chibi-scheme-static$(EXE): main.o eval.o sexp.o
	$(CC) $(CFLAGS) $(STATICFLAGS) -o $@ $^ $(LDFLAGS) $(GCLDFLAGS)

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
	./chibi-scheme -l syntax-rules.scm tests/r5rs-tests.scm

install: chibi-scheme
	cp chibi-scheme $(BINDIR)/
	mkdir -p $(MODDIR)
	cp init.scm syntax-rules.scm $(MODDIR)/
	mkdir -p $(INCDIR)
	cp $(INCLUDES) include/chibi/eval.h $(INCDIR)/
	mkdir -p $(LIBDIR)
	cp libchibi-scheme$(SO) $(LIBDIR)/

uninstall:
	rm -f $(BINDIR)/chibi-scheme*
	rm -f $(LIBDIR)/libchibischeme$(SO)
	cd $(INCDIR) && rm -f $(INCLUDES) include/chibi/eval.h
	rm -f $(MODDIR)/*.scm

dist: cleaner
	rm -f chibi-scheme-`cat VERSION`.tgz
	mkdir chibi-scheme-`cat VERSION`
	for f in `hg manifest`; do mkdir -p chibi-scheme-`cat VERSION`/`dirname $$f`; ln -s `pwd`/$$f chibi-scheme-`cat VERSION`/$$f; done
	tar cphzvf chibi-scheme-`cat VERSION`.tgz chibi-scheme-`cat VERSION`
	rm -rf chibi-scheme-`cat VERSION`
