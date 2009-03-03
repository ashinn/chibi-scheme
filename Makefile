
.PHONY: all doc dist clean cleaner test install uninstall

all: chibi-scheme

GC_OBJ=./gc/gc.a

$GC_OBJ: ./gc/alloc.c
	cd gc && make test

sexp.o: sexp.c sexp.h
	gcc -c -g -Os -o $@ $<

eval.o: eval.c eval.h sexp.h
	gcc -c -g -Os -o $@ $<

chibi-scheme: sexp.o eval.o $(GC_OBJ)
	gcc -g -Os -o $@ $^

clean:
	rm -f *.o

cleaner: clean
	rm -f chibi-scheme
	rm -rf *.dSYM

