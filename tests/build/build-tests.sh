#!/bin/sh

# test basic build options

# for bootstrapping reasons this is a shell script, instead of a
# scheme script using (chibi process)

# we just check each build against r5rs-tests.scm -
# some of the libraries will fail to build (notably
# if modules or user-defined types are disabled).

BUILDDIR=tests/build
FAILURES=0
MAKE=${MAKE:-make}
i=0

for opts in $(cat ${BUILDDIR}/build-opts.txt); do
    opts=$(echo ${opts} | tr ';' ' ')
    $MAKE cleaner 2>&1 >/dev/null
    if $MAKE $opts chibi-scheme 2>&1 >${BUILDDIR}/build${i}-make.out; then
        if $MAKE test 2>&1 | tee ${BUILDDIR}/build${i}-test.out | grep -q -E 'FAIL|ERROR'; then
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

if [ $FAILURES = 0 ]; then
    echo "build-tests: all tests passed"
else
    echo "build-tests: ${FAILURES} tests failed"
fi

