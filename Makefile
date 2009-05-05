
.PHONY: all doc dist clean cleaner test install uninstall

all: chibi-scheme

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib
INCDIR=$(PREFIX)/include/chibi-scheme
MODDIR=$(PREFIX)/share/chibi-scheme

LDFLAGS=-lm

# -Oz for smaller size on darwin
CFLAGS=-Wall -g -Os -save-temps

#GC_OBJ=./gc/gc.a
GC_OBJ=

./gc/gc.a: ./gc/alloc.c
	cd gc && make

sexp.o: sexp.c sexp.h config.h defaults.h Makefile
	gcc -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

eval.o: eval.c debug.c opcodes.c eval.h sexp.h config.h defaults.h Makefile
	gcc -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

main.o: main.c eval.c debug.c opcodes.c eval.h sexp.h config.h defaults.h Makefile
	gcc -c $(CPPFLAGS) $(CFLAGS) -o $@ $<

chibi-scheme: main.o sexp.o $(GC_OBJ)
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

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
	cd chibi-scheme-`cat VERSION`; tar xzvf ../gc.tar.gz; mv gc[0-9].[0-9] gc
	tar cphzvf chibi-scheme-`cat VERSION`.tgz chibi-scheme-`cat VERSION`
	rm -rf chibi-scheme-`cat VERSION`
