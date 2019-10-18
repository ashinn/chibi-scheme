#!/bin/bash -x

set -euo pipefail
IFS=$'\n\t'

rm -rf /tmp/chibi-scheme

make -j1 \
  CFLAGS="-g -Og" \
  PREFIX="/usr" \
  LIBDIR="/usr/lib64" \
  SOLIBDIR="/usr/lib64" \
  BINMODDIR="/usr/lib64/chibi" \
  MODDIR="/usr/lib64/chibi"

make install \
  DESTDIR=/tmp/chibi-scheme \
  MANDIR="/usr/man/man1" \
  PREFIX="/usr" \
  LIBDIR="/usr/lib64" \
  SOLIBDIR="/usr/lib64" \
  BINMODDIR="/usr/lib64/chibi" \
  MODDIR="/usr/lib64/chibi"

pushd /tmp/chibi-scheme
makepkg -l y -c y ../chibi-scheme-0.8.0.2git-x86_64-1dirty.tgz
upgradepkg --reinstall /tmp/chibi-scheme-0.8.0.2git-x86_64-1dirty.tgz
popd

chown -R lockywolf:users .


