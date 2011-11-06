</$objtype/mkfile

BIN=/$objtype/bin
TARG=chibi-scheme
MODDIR=/sys/lib/chibi-scheme

CPPFLAGS= -Iinclude -DPLAN9 '-DSEXP_USE_STRING_STREAMS=0' '-DSEXP_USE_DEBUG=0' '-DSEXP_USE_MODULES=0' '-DSEXP_USE_GREEN_THREADS=0'
CFLAGS= -p $CPPFLAGS

OFILES=sexp.$O eval.$O main.$O
HFILES=include/chibi/sexp.h include/chibi/eval.h include/chibi/features.h include/chibi/install.h

</sys/src/cmd/mkone

include/chibi/install.h: mkfile
	echo '#define sexp_default_module_path "'$MODDIR'"' > include/chibi/install.h
	echo '#define sexp_so_extension ""' >> include/chibi/install.h
	echo '#define sexp_platform "plan9"' >> include/chibi/install.h
	echo '#define sexp_version "'`{cat VERSION}'"' >> include/chibi/install.h
	echo '#define sexp_release_name "'`{cat RELEASE}'"' >> include/chibi/install.h

install:V: $BIN/$TARG
	test -d $MODDIR || mkdir -p $MODDIR
	{cd lib; tar c .} | {cd $MODDIR ; tar x }

test:V:
	./$O.out tests/r5rs-tests.scm

test-threads:
	./$O.out tests/thread-tests.scm

test-numbers:
	./$O.out tests/numeric-tests.scm

test-flonums:
	./$O.out tests/flonum-tests.scm

test-hash:
	./$O.out tests/hash-tests.scm

test-match:
	./$O.out tests/match-tests.scm

test-loop:
	./$O.out tests/loop-tests.scm

test-sort:
	./$O.out tests/sort-tests.scm

test-records:
	./$O.out tests/record-tests.scm

test-weak:
	./$O.out tests/weak-tests.scm

test-unicode:
	./$O.out tests/unicode-tests.scm

test-libs:
	./$O.out tests/lib-tests.scm

sexp.c:N:	gc.c opt/bignum.c
