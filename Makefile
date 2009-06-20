# -*- makefile-gmake -*-

.PHONY: all doc dist clean cleaner test install uninstall

all: chibi-scheme

CC     ?= cc
PREFIX ?= /usr/local
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib
INCDIR=$(PREFIX)/include/chibi
MODDIR=$(PREFIX)/share/chibi

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
else ifeq ($(PLATFORM),mingw)
SO  = .dll
EXE = .exe
CLIBFLAGS = -fPIC shared
else
SO  = .so
EXE =
CLIBFLAGS = -fPIC -shared
endif

ifdef USE_BOEHM
GCLDFLAGS := -lgc
else
GCLDFLAGS :=
endif

LDFLAGS  := $(LDFLAGS) -lm
CPPFLAGS := $(CPPFLAGS) -Iinclude
CFLAGS   := $(CFLAGS) -Wall -O2 -g

sexp.o: sexp.c gc.c include/chibi/sexp.h include/chibi/config.h Makefile
	$(CC) -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

eval.o: eval.c debug.c opcodes.c include/chibi/eval.h include/chibi/sexp.h include/chibi/config.h Makefile
	$(CC) -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

main.o: main.c eval.c debug.c opcodes.c include/chibi/eval.h include/chibi/sexp.h include/chibi/config.h Makefile
	$(CC) -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

libchibi-scheme$(SO): eval.o sexp.o
	$(CC) -dynamiclib -o $@ $^

chibi-scheme$(EXE): main.o libchibi-scheme$(SO)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $< $(LDFLAGS) $(GCLDFLAGS) -L. -lchibi-scheme

chibi-scheme-static$(EXE): main.o eval.o sexp.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(GCLDFLAGS)

clean:
	rm -f *.o *.i *.s

cleaner: clean
	rm -f chibi-scheme
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

# install: chibi-scheme
# 	cp chibi-scheme $(BINDIR)/
# 	mkdir -p $(MODDIR)
# 	cp init.scm $(MODDIR)/
# 	mkdir -p $(INCDIR)
# 	cp *.h $(INCDIR)/
# 	cp *.$(SO) $(LIBDIR)/

# uninstall:
# 	rm -f $(BINDIR)/chibi-scheme
# 	rm -f $(LIBDIR)/libchibischeme.$(SO)
# 	rm -f $(LIBDIR)/libchibisexp.$(SO)
# 	rm -f $(INCDIR)/*.h
# 	rm -f $(MODDIR)/*.scm

dist: cleaner
	rm -f chibi-scheme-`cat VERSION`.tgz
	mkdir chibi-scheme-`cat VERSION`
	for f in `hg manifest`; do mkdir -p chibi-scheme-`cat VERSION`/`dirname $$f`; ln -s `pwd`/$$f chibi-scheme-`cat VERSION`/$$f; done
	tar cphzvf chibi-scheme-`cat VERSION`.tgz chibi-scheme-`cat VERSION`
	rm -rf chibi-scheme-`cat VERSION`
