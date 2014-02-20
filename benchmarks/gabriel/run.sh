#!/bin/sh

BENCHDIR=$(dirname $0)
if [ "${BENCHDIR%%/*}" == "." ]; then
    BENCHDIR=$(pwd)${BENCHDIR#.}
fi
CHIBIHOME=${BENCHDIR%%/benchmarks/gabriel}
CHIBI="${CHIBI:-${CHIBIHOME}/chibi-scheme} -I$CHIBIHOME"

cd $BENCHDIR
for t in *.sch; do
    echo "${t%%.sch}"
    LD_LIBRARY_PATH="$CHIBIHOME" DYLD_LIBRARY_PATH="$CHIBIHOME" \
        $CHIBI -I"$CHIBIHOME/lib" -lchibi-prelude.scm $t
done
cd -
