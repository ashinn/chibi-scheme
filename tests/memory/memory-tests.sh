#!/bin/sh

for f in tests/memory/*.scm; do
    ./chibi-scheme-ulimit -xscheme $f >${f%.scm}.out 2>${f%.scm}.err
    if diff -q ${f%.scm}.out ${f%.scm}.res \
        && diff -q ${f%.scm}.err ${f%.scm}.err-res; then
        echo "[PASS] ${f%.scm}"
        rm -f ${f%.scm}.out ${f%.scm}.err
    else
        echo "[FAIL] ${f%.scm}"
    fi
done
