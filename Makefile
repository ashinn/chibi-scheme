
.PHONY: all doc dist clean cleaner test install uninstall

all: chibi-scheme

CFLAGS=-Wall -g -fno-inline -save-temps -Os

GC_OBJ=./gc/gc.a

$GC_OBJ: ./gc/alloc.c
	cd gc && make test

sexp.o: sexp.c sexp.h config.h Makefile
	gcc -c $(CFLAGS) -o $@ $<

eval.o: eval.c debug.c eval.h sexp.h config.h Makefile
	gcc -c $(CFLAGS) -o $@ $<

# main.o: main.c eval.h sexp.h config.h Makefile
# 	gcc -c $(CFLAGS) -o $@ $<

chibi-scheme: eval.o sexp.o $(GC_OBJ)
	gcc $(CFLAGS) -o $@ $^

clean:
	rm -f *.o *.i *.s

cleaner: clean
	rm -f chibi-scheme
	rm -rf *.dSYM

test: chibi-scheme
	@for f in tests/*.scm; do \
	    ./chibi-scheme $$f >$${f%.scm}.out 2>$${f%.scm}.err; \
	    if diff -q $${f%.scm}.out $${f%.scm}.res; then \
	        echo "[PASS] $${f%.scm}"; \
	    else \
	        echo "[FAIL] $${f%.scm}"; \
	    fi; \
	done

