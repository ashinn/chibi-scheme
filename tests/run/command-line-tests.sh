#!/bin/sh

# Test chibi-scheme command-line options.
# Should be run from a standard build.

TESTDIR=$(dirname $0)
FAILURES=0
i=0

run_chibi() {
    LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH DYLD_LIBRARY_PATH=.:$DYLD_LIBRARY_PATH CHIBI_MODULE_PATH=lib ./chibi-scheme "$@"
}

for t in $TESTDIR/*.args; do
    IFS=$'\r\n' GLOBIGNORE='*' :; args=($(cat $t))
    run_chibi ${args[@]} 2> ${t%.args}.err > ${t%.args}.out
    if diff -w -q ${t%.args}.out ${t%.args}.res \
            && ([ ! -e ${t%.args}.err-res ] || \
                    diff -w -q ${t%.args}.err ${t%.args}.err-res); then
        echo "[PASS] $(basename ${t%.args})"
    else
        echo "[FAIL] $(basename ${t%.args})"
        FAILURES=$((FAILURES + 1))
    fi
    i=$((i+1))
done

if [ $FAILURES = 0 ]; then
    echo "command-line-tests: all ${i} tests passed"
else
    echo "command-line-tests: ${FAILURES} out of ${i} tests failed"
    exit 1
fi
