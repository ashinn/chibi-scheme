#!/bin/sh

# Test basic build options.

# For bootstrapping reasons this is a shell script, instead of a
# scheme script using (chibi process).

# We just check each build against r5rs-tests.scm - some of the
# libraries will fail to build (notably if modules or user-defined
# types are disabled).

BUILDDIR=tests/build
FAILURES=0
MAKE=${MAKE:-make}
vars=CFLAGS="-O0 -g0 -w"
i=0

for opts in $(cat ${BUILDDIR}/build-opts.txt); do
    # If compiling with static libs, we need to bootstrap to build
    # clibs.c.
    if echo ${opts} | grep -q 'SEXP_USE_STATIC_LIBS=1'; then
        staticopts=$(echo ${opts} | sed 's/-DSEXP_USE_STATIC_LIBS=1;*//')
        staticopts=$(echo ${staticopts} | tr ';' ' ')
        $MAKE cleaner 2>&1 >/dev/null
        rm -f clibs.c
        $MAKE -j 8 "$vars" $staticopts 2>&1 >${BUILDDIR}/build${i}-bootstrap.out
        $MAKE -j 8 "$vars" $staticopts clibs.c 2>&1 >${BUILDDIR}/build${i}-clibs.out
    fi
    # Try to build then run tests.
    opts=$(echo ${opts} | tr ';' ' ')
    $MAKE cleaner 2>&1 >/dev/null
    if $MAKE -j 8 "$vars" $opts chibi-scheme 2>&1 >${BUILDDIR}/build${i}-make.out; then
        sync
        if $MAKE test 2>&1 | tee ${BUILDDIR}/build${i}-test.out \
            | grep -q -E 'FAIL|ERROR'; then
            echo "[FAIL] ${i}: tests failed with $opts"
            FAILURES=$((FAILURES + 1))
        else
            echo "[PASS] ${i}: tests passed with $opts"
        fi
    else
        echo "[FAIL] ${i}: couldn't build with $opts"
        FAILURES=$((FAILURES + 1))
    fi
    i=$((i+1))
done
$MAKE cleaner 2>&1 >/dev/null

if [ $FAILURES = 0 ]; then
    echo "build-tests: all tests passed"
else
    echo "build-tests: ${FAILURES} tests failed"
fi

