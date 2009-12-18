</$objtype/mkfile

BIN=/$objtype/bin
TARG=chibi-scheme
MODDIR=/sys/lib/chibi-scheme

CPPFLAGS= -Iinclude -DPLAN9 '-DSEXP_USE_STRING_STREAMS=0' '-DSEXP_USE_DEBUG=0' '-DSEXP_USE_MODULES=0'
CFLAGS= -p $CPPFLAGS

OFILES=sexp.$O eval.$O main.$O
HFILES=include/chibi/sexp.h include/chibi/eval.h include/chibi/config.h include/chibi/install.h

</sys/src/cmd/mkone

include/chibi/install.h: mkfile
	echo '#define sexp_default_module_dir "'$MODDIR'"' > include/chibi/install.h
	echo '#define sexp_platform "plan9"' >> include/chibi/install.h

install:V: $BIN/$TARG
	test -d $MODDIR || mkdir -p $MODDIR
	cp init.scm $MODDIR/

test:V:
	./$O.out tests/r5rs-tests.scm

sexp.c:N:	gc.c opt/bignum.c
