#!/bin/bash

# set -ex

BENCHDIR=$(dirname $0)
if [ "${BENCHDIR%%/*}" = "." ]; then
    BENCHDIR="$(pwd)${BENCHDIR#.}"
fi

TS1="${1:--2}"
TS2="${2:--1}"
DB="${3:-${BENCHDIR}/times.tsv}"

if [ "$TS1" -lt 1000000000 ]; then
    SORT_OPTS='-nu'
    if [ "$TS1" -lt 0 ]; then
        SORT_OPTS='-nru'
        TS1=$((0 - TS1))
    fi
    TS1=$(cut -f 7 "$DB" | sort "$SORT_OPTS" | tail -n +$TS1 | head -1)
fi
if [ "$TS2" -lt 1000000000 ]; then
    SORT_OPTS='-nu'
    if [ "$TS2" -lt 0 ]; then
        SORT_OPTS='-nru'
        TS2=$((0 - TS2))
    fi
    TS2=$(cut -f 7 "$DB" | sort "$SORT_OPTS" | tail -n +$TS2 | head -1)
fi

join -t $'\t' \
     <(grep $'\t'"$TS1"$'\t' "$DB" | cut -f 1-2,5) \
     <(grep $'\t'"$TS2"$'\t' "$DB" | cut -f 1-2,5) \
    | perl -F'\t' -ane 'sub gain{($_[0]<=0)?0:100*($_[1]-$_[0])/$_[0]} $u=gain($F[1], $F[3]); $g=gain($F[2], $F[4]); printf STDOUT "%s\t%d\t%d\t%.2f%%\t%d\t%d\t%.2f%%\n", $F[0], $F[1], $F[3], $u, $F[2], $F[4], $g'
