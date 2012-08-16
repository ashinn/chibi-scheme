</$objtype/mkfile

BIN=/$objtype/bin
TARG=chibi-scheme
MODDIR=/sys/lib/chibi-scheme

CHIBI=./$O.out
GENSTATIC=./tools/chibi-genstatic

CPPFLAGS= -Iinclude -DPLAN9 -DSEXP_USE_GREEN_THREADS=0
CFLAGS= -p $CPPFLAGS
CFLAGS_STATIC=$CFLAGS -DSEXP_USE_STATIC_LIBS

OFILES=gc.$O sexp.$O bignum.$O opcodes.$O plan9.$O vm.$O simplify.$O eval.$O main.$O $STATIC
HFILES=include/chibi/sexp.h include/chibi/eval.h include/chibi/features.h include/chibi/install.h
CLEANFILES=tests/basic/*.out tests/basic/*.err

EXCLUDE=srfi.18 srfi.27 chibi.filesystem chibi.io \
	chibi.net chibi.process chibi.stty chibi.system \
	chibi.time

CHIBI_LIBS = lib/chibi/filesystem.c lib/chibi/process.c \
	lib/chibi/time.c lib/chibi/system.c lib/chibi/stty.c \
	lib/chibi/weak.c lib/chibi/heap-stats.c lib/chibi/disasm.c \
	lib/chibi/net.c
CHIBI_IO_COMPILED_LIBS = lib/chibi/io/io.c
CHIBI_OPT_COMPILED_LIBS = lib/chibi/optimize/rest.c \
	lib/chibi/optimize/profile.c
COMPILED_LIBS = $CHIBI_COMPILED_LIBS $CHIBI_IO_COMPILED_LIBS \
	$CHIBI_OPT_COMPILED_LIBS \
	lib/srfi/33/bit.c lib/srfi/39/param.c \
	lib/srfi/69/hash.c lib/srfi/95/qsort.c lib/srfi/98/env.c \
	lib/scheme/time.c

</sys/src/cmd/mkone

clean:
	rm -f $CLEANFILES

clibs.$O: clibs.c

$TARG: $O.out
	rm $OFILES
	mk 'CFLAGS=$CFLAGS_STATIC' clibs.$O $OFILES
	mk 'CFLAGS=$CFLAGS_STATIC' 'STATIC=clibs.$O' default

target: $O.out
	mv $O.out $TARG

%.c:   %.stub
	$CHIBI ./tools/chibi-ffi $stem.stub

include/chibi/install.h: mkfile
	echo '#define sexp_default_module_path "'$MODDIR'"' > include/chibi/install.h
	echo '#define sexp_so_extension ".no-such-file"' >> include/chibi/install.h
	echo '#define sexp_platform "plan9"' >> include/chibi/install.h
	echo '#define sexp_version "'`{cat VERSION}'"' >> include/chibi/install.h
	echo '#define sexp_release_name "'`{cat RELEASE}'"' >> include/chibi/install.h

dist-clean: clean
	rm -f include/chibi/install.h clibs.c

install:V: $BIN/$TARG
	test -d $MODDIR || mkdir -p $MODDIR
	{cd lib; tar c .} | {cd $MODDIR ; tar x }

clibs.c:V: $GENSTATIC $CHIBI $COMPILED_LIBS
	du -a lib | sed 's/^[0-9]*[ 	]*//' | grep '\.sld$' | \
	$CHIBI $GENSTATIC \
	-x srfi.27 -x srfi.18 -x chibi.filesystem -x chibi.io \
	-x chibi.net -x chibi.process -x chibi.stty -x chibi.system \
	-x chibi.time \
	> ,clibs.c && mv ,clibs.c clibs.c

test:V:
	./$O.out -xscheme tests/r5rs-tests.scm

test-threads:
	./$O.out -xscheme tests/thread-tests.scm

test-numbers:
	./$O.out -xscheme tests/numeric-tests.scm

test-flonums:
	./$O.out -xscheme tests/flonum-tests.scm

test-hash:
	./$O.out -xscheme tests/hash-tests.scm

test-match:
	./$O.out -xscheme tests/match-tests.scm

test-loop:
	./$O.out -xscheme tests/loop-tests.scm

test-sort:
	./$O.out -xscheme tests/sort-tests.scm

test-records:
	./$O.out -xscheme tests/record-tests.scm

test-weak:
	./$O.out -xscheme tests/weak-tests.scm

test-unicode:
	./$O.out -xscheme tests/unicode-tests.scm

test-libs:
	./$O.out -xscheme tests/lib-tests.scm
