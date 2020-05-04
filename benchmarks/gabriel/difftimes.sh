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
    TS1=$(cut -f 7 "$DB" | sort -nru | tail -n +$((0 - TS1)) | head -1)
fi
if [ "$TS2" -lt 1000000000 ]; then
    TS2=$(cut -f 7 "$DB" | sort -nru | tail -n +$((0 - TS2)) | head -1)
fi

join -t $'\t' \
     <(grep $'\t'"$TS1"$'\t' "$DB" | cut -f 1-2) \
     <(grep $'\t'"$TS2"$'\t' "$DB" | cut -f 1-2) \
    | perl -F'\t' -ane '$g=($F[1]<=0)?0:100*($F[2]-$F[1])/$F[1]; printf STDOUT "%s\t%d\t%d\t%.2f%%\n", @F, $g'
