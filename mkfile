</$objtype/mkfile

BIN=/$objtype/bin
TARG=chibi-scheme
MODDIR=/sys/lib/chibi-scheme

CPPFLAGS= -Iinclude -DPLAN9 -DUSE_STRING_STREAMS=0 -DUSE_DEBUG=0
CFLAGS=	-c -B $CPPFLAGS

OFILES=sexp.$O eval.$O main.$O

HFILES=include/chibi/sexp.h include/chibi/eval.h include/chibi/config.h

include/chibi/install.h: mkfile
	echo '#define sexp_module_dir "'$MODDIR'"' > include/chibi/install.h

%.i: %.c include/chibi/install.h $HFILES
	cpp $CPPFLAGS $stem.c > $target

sexp.$O: sexp.i
	$CC $CFLAGS -c -o $target sexp.i

eval.$O: eval.i
	$CC $CFLAGS -c -o $target eval.i

main.$O: main.i
	$CC $CFLAGS -c -o $target main.i

chibi-scheme: sexp.$O eval.$O main.$O
	$LD -o $target $prereq

#</sys/src/cmd/mkone

install:
	mkdir $MODDIR
	cp init.scm $MODDIR
