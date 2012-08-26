# -*- makefile-gmake -*-

.PHONY: dist mips-dist cleaner test checkdefs
.DEFAULT_GOAL := all

CHIBI_FFI ?= $(CHIBI) tools/chibi-ffi
CHIBI_FFI_DEPENDENCIES ?= $(CHIBI_DEPENDENCIES) tools/chibi-ffi

CHIBI_DOC ?= $(CHIBI) tools/chibi-doc
CHIBI_DOC_DEPENDENCIES ?= $(CHIBI_DEPENDENCIES) tools/chibi-doc

GENSTATIC ?= ./tools/chibi-genstatic

CHIBI ?= LD_LIBRARY_PATH=".:$(LD_LIBRARY_PATH)" ./chibi-scheme$(EXE)
CHIBI_DEPENDENCIES = ./chibi-scheme$(EXE)

########################################################################

CHIBI_COMPILED_LIBS = lib/chibi/filesystem$(SO) lib/chibi/process$(SO) \
	lib/chibi/time$(SO) lib/chibi/system$(SO) lib/chibi/stty$(SO) \
	lib/chibi/weak$(SO) lib/chibi/heap-stats$(SO) lib/chibi/disasm$(SO) \
	lib/chibi/net$(SO)
CHIBI_IO_COMPILED_LIBS = lib/chibi/io/io$(SO)
CHIBI_OPT_COMPILED_LIBS = lib/chibi/optimize/rest$(SO) \
	lib/chibi/optimize/profile$(SO)
COMPILED_LIBS = $(CHIBI_COMPILED_LIBS) $(CHIBI_IO_COMPILED_LIBS) \
	$(CHIBI_OPT_COMPILED_LIBS) lib/srfi/18/threads$(SO) \
	lib/srfi/27/rand$(SO) lib/srfi/33/bit$(SO) lib/srfi/39/param$(SO) \
	lib/srfi/69/hash$(SO) lib/srfi/95/qsort$(SO) lib/srfi/98/env$(SO) \
	lib/scheme/time$(SO)

BASE_INCLUDES = include/chibi/sexp.h include/chibi/features.h include/chibi/install.h include/chibi/bignum.h
INCLUDES = $(BASE_INCLUDES) include/chibi/eval.h

MODULE_DOCS := ast disasm equiv filesystem generic heap-stats io loop \
	match mime modules net pathname process repl scribble stty \
	system test time trace type-inference uri weak

HTML_LIBS = $(MODULE_DOCS:%=doc/lib/chibi/%.html)

########################################################################

include Makefile.libs

########################################################################
# Library config.
#
# This is to allow "make SEXP_USE_BOEHM=1" and "make SEXP_USE_DL=0" to
# automatically include the necessary compiler and linker flags in
# addition to setting those features.  If not using GNU make just
# comment out the ifs and use the else branches for the defaults.

ifeq ($(SEXP_USE_BOEHM),1)
GCLDFLAGS := -lgc
XCPPFLAGS := $(CPPFLAGS) -Iinclude $(D:%=-DSEXP_USE_%) -DSEXP_USE_BOEHM=1
else
GCLDFLAGS :=
XCPPFLAGS := $(CPPFLAGS) -Iinclude $(D:%=-DSEXP_USE_%)
endif

ifeq ($(SEXP_USE_DL),0)
XLDFLAGS  := $(LDFLAGS) $(RLDFLAGS) $(GCLDFLAGS) -lm
XCFLAGS   := -Wall -DSEXP_USE_DL=0 -g -g3 -Os $(CFLAGS)
else
XLDFLAGS  := $(LDFLAGS) $(RLDFLAGS) $(GCLDFLAGS) $(LIBDL) -lm
XCFLAGS   := -Wall -g -g3 -Os $(CFLAGS)
endif

########################################################################

all: chibi-scheme$(EXE) all-libs lib/chibi/ast$(SO)

include/chibi/install.h: Makefile
	echo '#define sexp_so_extension "'$(SO)'"' > $@
	echo '#define sexp_default_module_path "'$(MODDIR):$(BINMODDIR)'"' >> $@
	echo '#define sexp_platform "'$(PLATFORM)'"' >> $@
	echo '#define sexp_version "'`cat VERSION`'"' >> $@
	echo '#define sexp_release_name "'`cat RELEASE`'"' >> $@

%.o: %.c $(BASE_INCLUDES)
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -o $@ $<

sexp-ulimit.o: sexp.c $(BASE_INCLUDES)
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) $(CLIBFLAGS) -DSEXP_USE_LIMITED_MALLOC -o $@ $<

main.o: main.c $(INCLUDES)
	$(CC) -c $(XCPPFLAGS) $(XCFLAGS) -o $@ $<

SEXP_OBJS = gc.o sexp.o bignum.o
SEXP_ULIMIT_OBJS = gc.o sexp-ulimit.o bignum.o
EVAL_OBJS = opcodes.o vm.o eval.o simplify.o

libchibi-sexp$(SO): $(SEXP_OBJS)
	$(CC) $(CLIBFLAGS) -o $@ $^ $(XLDFLAGS)

libchibi-scheme$(SO): $(SEXP_OBJS) $(EVAL_OBJS)
	$(CC) $(CLIBFLAGS) -o $@ $^ $(XLDFLAGS)

chibi-scheme$(EXE): main.o libchibi-scheme$(SO)
	$(CC) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. -lchibi-scheme

chibi-scheme-static$(EXE): main.o $(SEXP_OBJS) $(EVAL_OBJS)
	$(CC) $(XCFLAGS) $(STATICFLAGS) -o $@ $^ $(LDFLAGS) $(GCLDFLAGS) -lm

chibi-scheme-ulimit$(EXE): main.o $(SEXP_ULIMIT_OBJS) $(EVAL_OBJS)
	$(CC) $(XCFLAGS) $(STATICFLAGS) -o $@ $^ $(LDFLAGS) $(GCLDFLAGS) -lm

clibs.c: $(GENSTATIC) chibi-scheme$(EXE)
	$(FIND) lib -name \*.sld | $(CHIBI) $(GENSTATIC) > $@

# A special case, this needs to be linked with the LDFLAGS in case
# we're using Boehm.
lib/chibi/ast$(SO): lib/chibi/ast.c $(INCLUDES)
	-$(CC) $(CLIBFLAGS) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< $(XLDFLAGS) -L. -lchibi-scheme

doc/lib/chibi/%.html: lib/chibi/%.sld $(CHIBI_DOC_DEPENDENCIES)
	$(CHIBI_DOC) chibi.$* > $@

doc: doc/chibi.html doc-libs

%.html: %.scrbl $(CHIBI_DOC_DEPENDENCIES)
	$(CHIBI_DOC) $< > $@

########################################################################
# Dist builds - rules to build generated files included in distribution
# (currently just char-sets since it takes a long time and we don't want
# to bundle the raw Unicode files or require a net connection to build).

data/%.txt:
	curl --silent http://www.unicode.org/Public/UNIDATA/$*.txt > $@

build-lib/chibi/char-set/derived.scm: data/UnicodeData.txt data/DerivedCoreProperties.txt chibi-scheme$(EXE)
	$(CHIBI) tools/extract-unicode-props.scm --default > $@

lib/chibi/char-set/ascii.scm: build-lib/chibi/char-set/derived.scm chibi-scheme$(EXE)
	$(CHIBI) -Abuild-lib tools/optimize-char-sets.scm --ascii chibi.char-set.compute > $@

lib/chibi/char-set/full.scm: build-lib/chibi/char-set/derived.scm chibi-scheme$(EXE)
	$(CHIBI) -Abuild-lib tools/optimize-char-sets.scm chibi.char-set.compute > $@

lib/scheme/char/case-offsets.scm: data/CaseFolding.txt chibi-scheme$(EXE) all-libs
	$(CHIBI) tools/extract-case-offsets.scm $< > $@

########################################################################
# Tests

checkdefs:
	@for d in $(D); do \
	    if ! $(GREP) -q " SEXP_USE_$${d%%=*} " include/chibi/features.h; then \
	        echo "WARNING: unknown definition $$d"; \
	    fi; \
	done

test-basic: chibi-scheme$(EXE)
	@for f in tests/basic/*.scm; do \
	    $(CHIBI) -xscheme $$f >$${f%.scm}.out 2>$${f%.scm}.err; \
	    if $(DIFF) -q $(DIFFOPTS) $${f%.scm}.out $${f%.scm}.res; then \
	        echo "[PASS] $${f%.scm}"; \
	    else \
	        echo "[FAIL] $${f%.scm}"; \
	    fi; \
	done

test-memory: chibi-scheme-ulimit$(EXE)
	./tests/memory/memory-tests.sh

test-build:
	MAKE=$(MAKE) ./tests/build/build-tests.sh

test-threads: chibi-scheme$(EXE) lib/srfi/18/threads$(SO) lib/srfi/39/param$(SO) lib/srfi/98/env$(SO) lib/chibi/ast$(SO) lib/chibi/time$(SO)
	$(CHIBI) -xscheme tests/thread-tests.scm

test-numbers: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/numeric-tests.scm

test-flonums: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/flonum-tests.scm

test-hash: chibi-scheme$(EXE) lib/srfi/69/hash$(SO)
	$(CHIBI) -xscheme tests/hash-tests.scm

test-io: chibi-scheme$(EXE) lib/chibi/io/io$(SO)
	$(CHIBI) -xscheme tests/io-tests.scm

test-match: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/match-tests.scm

test-loop: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/loop-tests.scm

test-sort: chibi-scheme$(EXE) lib/srfi/33/bit$(SO)
	$(CHIBI) -xscheme tests/sort-tests.scm

test-srfi-1: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/srfi-1-tests.scm

test-records: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/record-tests.scm

test-weak: chibi-scheme$(EXE) lib/chibi/weak$(SO)
	$(CHIBI) -xscheme tests/weak-tests.scm

test-unicode: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/unicode-tests.scm

test-process: chibi-scheme$(EXE) lib/chibi/process$(SO)
	$(CHIBI) -xscheme tests/process-tests.scm

test-system: chibi-scheme$(EXE) lib/chibi/system$(SO)
	$(CHIBI) -xscheme tests/system-tests.scm

test-libs: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/lib-tests.scm

test: chibi-scheme$(EXE)
	$(CHIBI) -xscheme tests/r5rs-tests.scm

bench-gabriel: chibi-scheme$(EXE)
	./benchmarks/gabriel/run.sh

########################################################################
# Packaging

clean: clean-libs
	-$(RM) *.o *.i *.s *.8 tests/basic/*.out tests/basic/*.err

cleaner: clean
	-$(RM) chibi-scheme$(EXE) chibi-scheme-static$(EXE) chibi-scheme-ulimit$(EXE) \
	    libchibi-scheme$(SO) *.a include/chibi/install.h \
	    $(shell $(FIND) lib -name \*.o)

dist-clean: dist-clean-libs cleaner

install: all
	$(MKDIR) $(DESTDIR)$(BINDIR)
	$(INSTALL) chibi-scheme$(EXE) $(DESTDIR)$(BINDIR)/
	$(INSTALL) tools/chibi-ffi $(DESTDIR)$(BINDIR)/
	$(INSTALL) tools/chibi-doc $(DESTDIR)$(BINDIR)/
	$(MKDIR) $(DESTDIR)$(MODDIR)/chibi/char-set $(DESTDIR)$(MODDIR)/chibi/io $(DESTDIR)$(MODDIR)/chibi/iset $(DESTDIR)$(MODDIR)/chibi/loop $(DESTDIR)$(MODDIR)/chibi/match $(DESTDIR)$(MODDIR)/chibi/net $(DESTDIR)$(MODDIR)/chibi/optimize $(DESTDIR)$(MODDIR)/chibi/term
	$(MKDIR) $(DESTDIR)$(MODDIR)/scheme/char
	$(MKDIR) $(DESTDIR)$(MODDIR)/scheme/time
	$(MKDIR) $(DESTDIR)$(MODDIR)/srfi/1 $(DESTDIR)$(MODDIR)/srfi/18 $(DESTDIR)$(MODDIR)/srfi/27 $(DESTDIR)$(MODDIR)/srfi/33 $(DESTDIR)$(MODDIR)/srfi/39 $(DESTDIR)$(MODDIR)/srfi/69 $(DESTDIR)$(MODDIR)/srfi/95 $(DESTDIR)$(MODDIR)/srfi/99 $(DESTDIR)$(MODDIR)/srfi/99/records
	$(INSTALL) lib/*.scm $(DESTDIR)$(MODDIR)/
	$(INSTALL) lib/chibi/*.sld lib/chibi/*.scm $(DESTDIR)$(MODDIR)/chibi/
	$(INSTALL) lib/chibi/char-set/*.sld lib/chibi/char-set/*.scm $(DESTDIR)$(MODDIR)/chibi/char-set/
	$(INSTALL) lib/chibi/io/*.scm $(DESTDIR)$(MODDIR)/chibi/io/
	$(INSTALL) lib/chibi/iset/*.sld lib/chibi/iset/*.scm $(DESTDIR)$(MODDIR)/chibi/iset/
	$(INSTALL) lib/chibi/loop/*.scm $(DESTDIR)$(MODDIR)/chibi/loop/
	$(INSTALL) lib/chibi/match/*.scm $(DESTDIR)$(MODDIR)/chibi/match/
	$(INSTALL) lib/chibi/net/*.sld lib/chibi/net/*.scm $(DESTDIR)$(MODDIR)/chibi/net/
	$(INSTALL) lib/chibi/optimize/*.sld lib/chibi/optimize/*.scm $(DESTDIR)$(MODDIR)/chibi/optimize/
	$(INSTALL) lib/chibi/term/*.sld lib/chibi/term/*.scm $(DESTDIR)$(MODDIR)/chibi/term/
	$(INSTALL) lib/scheme/*.sld lib/scheme/*.scm $(DESTDIR)$(MODDIR)/scheme/
	$(INSTALL) lib/scheme/char/*.sld lib/scheme/char/*.scm $(DESTDIR)$(MODDIR)/scheme/char/
	$(INSTALL) lib/scheme/time/*.sld $(DESTDIR)$(MODDIR)/scheme/time/
	$(INSTALL) lib/srfi/*.sld lib/srfi/*.scm $(DESTDIR)$(MODDIR)/srfi/
	$(INSTALL) lib/srfi/1/*.scm $(DESTDIR)$(MODDIR)/srfi/1/
	$(INSTALL) lib/srfi/18/*.scm $(DESTDIR)$(MODDIR)/srfi/18/
	$(INSTALL) lib/srfi/27/*.scm $(DESTDIR)$(MODDIR)/srfi/27/
	$(INSTALL) lib/srfi/33/*.scm $(DESTDIR)$(MODDIR)/srfi/33/
	$(INSTALL) lib/srfi/39/*.scm $(DESTDIR)$(MODDIR)/srfi/39/
	$(INSTALL) lib/srfi/69/*.scm $(DESTDIR)$(MODDIR)/srfi/69/
	$(INSTALL) lib/srfi/95/*.scm $(DESTDIR)$(MODDIR)/srfi/95/
	$(INSTALL) lib/srfi/99/*.sld $(DESTDIR)$(MODDIR)/srfi/99/
	$(INSTALL) lib/srfi/99/records/*.sld lib/srfi/99/records/*.scm $(DESTDIR)$(MODDIR)/srfi/99/records/
	$(MKDIR) $(DESTDIR)$(BINMODDIR)/chibi/io/
	$(MKDIR) $(DESTDIR)$(BINMODDIR)/chibi/optimize/
	$(MKDIR) $(DESTDIR)$(BINMODDIR)/scheme/
	$(MKDIR) $(DESTDIR)$(BINMODDIR)/srfi/18 $(DESTDIR)$(BINMODDIR)/srfi/27 $(DESTDIR)$(BINMODDIR)/srfi/33 $(DESTDIR)$(BINMODDIR)/srfi/39 $(DESTDIR)$(BINMODDIR)/srfi/69 $(DESTDIR)$(BINMODDIR)/srfi/95 $(DESTDIR)$(BINMODDIR)/srfi/98
	$(INSTALL) $(CHIBI_COMPILED_LIBS) lib/chibi/ast$(SO) $(DESTDIR)$(BINMODDIR)/chibi/
	$(INSTALL) $(CHIBI_IO_COMPILED_LIBS) $(DESTDIR)$(BINMODDIR)/chibi/io/
	$(INSTALL) $(CHIBI_OPT_COMPILED_LIBS) $(DESTDIR)$(BINMODDIR)/chibi/optimize/
	$(INSTALL) lib/scheme/time$(SO) $(DESTDIR)$(BINMODDIR)/scheme/
	$(INSTALL) lib/srfi/18/threads$(SO) $(DESTDIR)$(BINMODDIR)/srfi/18
	$(INSTALL) lib/srfi/27/rand$(SO) $(DESTDIR)$(BINMODDIR)/srfi/27
	$(INSTALL) lib/srfi/33/bit$(SO) $(DESTDIR)$(BINMODDIR)/srfi/33
	$(INSTALL) lib/srfi/39/param$(SO) $(DESTDIR)$(BINMODDIR)/srfi/39
	$(INSTALL) lib/srfi/69/hash$(SO) $(DESTDIR)$(BINMODDIR)/srfi/69
	$(INSTALL) lib/srfi/95/qsort$(SO) $(DESTDIR)$(BINMODDIR)/srfi/95
	$(INSTALL) lib/srfi/98/env$(SO) $(DESTDIR)$(BINMODDIR)/srfi/98
	$(MKDIR) $(DESTDIR)$(INCDIR)
	$(INSTALL) $(INCLUDES) $(DESTDIR)$(INCDIR)/
	$(MKDIR) $(DESTDIR)$(LIBDIR)
	$(MKDIR) $(DESTDIR)$(SOLIBDIR)
	$(INSTALL) libchibi-scheme$(SO) $(DESTDIR)$(SOLIBDIR)/
	-$(INSTALL) libchibi-scheme.a $(DESTDIR)$(SOLIBDIR)/
	$(MKDIR) $(DESTDIR)$(MANDIR)
	$(INSTALL) doc/chibi-scheme.1 $(DESTDIR)$(MANDIR)/
	$(INSTALL) doc/chibi-ffi.1 $(DESTDIR)$(MANDIR)/
	$(INSTALL) doc/chibi-doc.1 $(DESTDIR)$(MANDIR)/
	-if type ldconfig >/dev/null 2>/dev/null; then ldconfig; fi

uninstall:
	-$(RM) $(DESTDIR)$(BINDIR)/chibi-scheme$(EXE)
	-$(RM) $(DESTDIR)$(BINDIR)/chibi-scheme-static$(EXE)
	-$(RM) $(DESTDIR)$(BINDIR)/chibi-ffi
	-$(RM) $(DESTDIR)$(BINDIR)/chibi-doc
	-$(RM) $(DESTDIR)$(SOLIBDIR)/libchibi-scheme$(SO)
	-$(RM) $(DESTDIR)$(LIBDIR)/libchibi-scheme$(SO).a
	-$(CD) $(DESTDIR)$(INCDIR) && $(RM) $(INCLUDES)
	-$(RM) $(DESTDIR)$(MODDIR)/srfi/99/records/*.{sld,scm}
	-$(RM) $(DESTDIR)$(MODDIR)/*.{sld,scm} $(DESTDIR)$(MODDIR)/*/*.{sld,scm} $(DESTDIR)$(MODDIR)/*/*/*.{sld,scm}
	-$(CD) $(DESTDIR)$(MODDIR) && $(RM) $(COMPILED_LIBS:lib/%=%) chibi/ast$(SO)
	-$(CD) $(DESTDIR)$(BINMODDIR) && $(RM) $(COMPILED_LIBS:lib/%=%) chibi/ast$(SO)
	-$(RMDIR) $(DESTDIR)$(MODDIR)/chibi/io $(DESTDIR)$(BINMODDIR)/chibi/io
	-$(RMDIR) $(DESTDIR)$(MODDIR)/chibi/loop $(DESTDIR)$(BINMODDIR)/chibi/loop
	-$(RMDIR) $(DESTDIR)$(MODDIR)/chibi/match $(DESTDIR)$(BINMODDIR)/chibi/match
	-$(RMDIR) $(DESTDIR)$(MODDIR)/chibi/net $(DESTDIR)$(BINMODDIR)/chibi/net
	-$(RMDIR) $(DESTDIR)$(MODDIR)/chibi/optimize $(DESTDIR)$(BINMODDIR)/chibi/optimize
	-$(RMDIR) $(DESTDIR)$(MODDIR)/chibi/term $(DESTDIR)$(BINMODDIR)/chibi/term
	-$(RMDIR) $(DESTDIR)$(MODDIR)/chibi $(DESTDIR)$(BINMODDIR)/chibi
	-$(RMDIR) $(DESTDIR)$(MODDIR)/scheme/char $(DESTDIR)$(BINMODDIR)/scheme/char
	-$(RMDIR) $(DESTDIR)$(MODDIR)/scheme $(DESTDIR)$(BINMODDIR)/scheme
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/1 $(DESTDIR)$(BINMODDIR)/srfi/1
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/18 $(DESTDIR)$(BINMODDIR)/srfi/18
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/27 $(DESTDIR)$(BINMODDIR)/srfi/27
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/33 $(DESTDIR)$(BINMODDIR)/srfi/33
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/39 $(DESTDIR)$(BINMODDIR)/srfi/39
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/69 $(DESTDIR)$(BINMODDIR)/srfi/69
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/95 $(DESTDIR)$(BINMODDIR)/srfi/95
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/98 $(DESTDIR)$(BINMODDIR)/srfi/98
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/99/records $(DESTDIR)$(BINMODDIR)/srfi/99/records
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi/99 $(DESTDIR)$(BINMODDIR)/srfi/99
	-$(RMDIR) $(DESTDIR)$(MODDIR)/srfi $(DESTDIR)$(BINMODDIR)/srfi
	-$(RMDIR) $(DESTDIR)$(MODDIR) $(DESTDIR)$(BINMODDIR)
	-$(RM) $(DESTDIR)$(MANDIR)/chibi-scheme.1 $(DESTDIR)$(MANDIR)/chibi-ffi.1 $(DESTDIR)$(MANDIR)/chibi-doc.1

dist: dist-clean
	$(RM) chibi-scheme-`cat VERSION`.tgz
	$(MKDIR) chibi-scheme-`cat VERSION`
	@for f in `hg manifest`; do $(MKDIR) chibi-scheme-`cat VERSION`/`dirname $$f`; $(SYMLINK) `pwd`/$$f chibi-scheme-`cat VERSION`/$$f; done
	$(TAR) cphzvf chibi-scheme-`cat VERSION`.tgz chibi-scheme-`cat VERSION`
	$(RM) -r chibi-scheme-`cat VERSION`

mips-dist: dist-clean
	$(RM) chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`.tgz
	$(MKDIR) chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
	@for f in `hg manifest`; do $(MKDIR) chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`/`dirname $$f`; $(SYMLINK) `pwd`/$$f chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`/$$f; done
	$(TAR) cphzvf chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`.tgz chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
	$(RM) -r chibi-scheme-`date +%Y%m%d`-`hg tags|head -1|sed -n 's/.* \([0-9]*\):.*/\1/p'`
