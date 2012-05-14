#!/bin/sh

export PATH=/usr/local/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

tar xzvf chibi-scheme-@VERSION@.tgz
cd chibi-scheme-@VERSION@
make
make install
cp tests/r5rs-tests.scm ..
cd ..
chibi-scheme r5rs-tests.scm | tee r5rs-tests.out
