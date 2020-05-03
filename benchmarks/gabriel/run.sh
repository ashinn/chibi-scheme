#!/bin/sh

# set -ex

BENCHDIR=$(dirname $0)
if [ "${BENCHDIR%%/*}" = "." ]; then
    BENCHDIR="$(pwd)${BENCHDIR#.}"
fi
OUTPUT="$BENCHDIR/out.txt"
DB="$BENCHDIR/times.tsv"
CHIBIHOME="${BENCHDIR%%/benchmarks/gabriel}"
CHIBI="${CHIBI:-${CHIBIHOME}/chibi-scheme} -I$CHIBIHOME"
HEAP="2M"

cd "$BENCHDIR"
for t in *.sch; do
    echo "program: ${t%%.sch}"
    LD_LIBRARY_PATH="$CHIBIHOME" DYLD_LIBRARY_PATH="$CHIBIHOME" \
        $CHIBI -I"$CHIBIHOME/lib" -h"$HEAP" -q -lchibi-prelude.scm "$t"
done | tee "$OUTPUT"
cd -

if [ ! -f "$DB" ]; then
    echo $'program\tuser_ms\tsystem_ms\treal_ms\tgc_ms\tgc_count\ttimestamp\tcommit\tfeatures\tinit_heap\tcpu' > "$DB"
fi

#DATE=$(date -Iseconds)
DATE=$(date +%s)
COMMIT=$(git -C "$CHIBIHOME" rev-parse HEAD)
FEATURES=$(LD_LIBRARY_PATH="$CHIBIHOME" DYLD_LIBRARY_PATH="$CHIBIHOME" $CHIBI -q -p'(cddr *features*)' | tr ' ' , | tr -d '()')
CPU=$(lscpu | perl -ne 'if (s/^Model name:\s*//){s/\b(Intel|Core|Atom|AMD|CPU)(\s*\(\w+\))?\s*//gi;s/\s*@\s*[.\d]+[KMGT]Hz\b\s*//gi;print}')
perl -ane 'if (/^program:\s*(\w+)/) {$p=$1} elsif (/^user:\s*(\d+)\s*system:\s*(\d+)\s*real:\s*(\d+)(?:\s*gc:\s*(\d+)\s*(?:\((\d+)\s*times\))?)?/) {print"$p\t$1\t$2\t$3\t$4\t$5\t'"$DATE"'\t'"$COMMIT"'\t'"$FEATURES"'\t'"$HEAP"'\t'"$CPU"'\n"}' "$OUTPUT" >> "$DB"
