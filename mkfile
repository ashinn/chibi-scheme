</$objtype/mkfile

BIN=/$objtype/bin
TARG=chibi-scheme
MODDIR=/sys/lib/chibi-scheme

CPPFLAGS= -Iinclude -DPLAN9 -DUSE_STRING_STREAMS=0 -DUSE_DEBUG=0
CFLAGS=	-c -B $CPPFLAGS

OFILES=sexp.$O eval.$O main.$O
IFILES=${OFILES:%.$O=%.i}
HFILES=include/chibi/sexp.h include/chibi/eval.h include/chibi/config.h include/chibi/install.h

%.i: %.c $HFILES
	cpp $CPPFLAGS $stem.c > $target

%.$O: %.i
	$CC $CFLAGS -c -o $target $prereq

all:V: $TARG

include/chibi/install.h: mkfile
	echo '#define sexp_module_dir "'$MODDIR'"' > include/chibi/install.h

$TARG: $OFILES
	$LD $LDFLAGS -o $target $prereq

$BIN/%: %
	cp $stem $target

clean:V:
	rm -f $IFILES $TARG *.[$OS]

install:V: $BIN/$TARG
	mkdir -p $MODDIR
	cp init.scm $MODDIR/
