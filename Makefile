
.PHONY: all doc dist clean cleaner test install uninstall

all: chibi-scheme

CFLAGS=-g -fno-inline -Os

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
	rm -f *.o

cleaner: clean
	rm -f chibi-scheme
	rm -rf *.dSYM

