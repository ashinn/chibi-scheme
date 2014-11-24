
(cond-expand
 (modules (import (only (chibi) load)
                  (only (chibi test) test-begin test-end)))
 (else (load "tests/r5rs-tests.scm")))

(test-begin "libraries")

(load "tests/srfi-1-tests.scm")
(load "tests/srfi-2-tests.scm")
(load "tests/srfi-16-tests.scm")
(load "tests/srfi-26-tests.scm")
(load "tests/srfi-27-tests.scm")
(load "tests/srfi-38-tests.scm")
(load "tests/flonum-tests.scm")
(load "tests/numeric-tests.scm")
(load "tests/loop-tests.scm")
(load "tests/match-tests.scm")
(load "tests/scribble-tests.scm")
(load "tests/string-tests.scm")
(load "tests/iset-tests.scm")
(load "tests/uri-tests.scm")
(load "tests/mime-tests.scm")
(load "tests/regexp-tests.scm")
(load "tests/prime-tests.scm")
(load "tests/md5-tests.scm")
(load "tests/sha-tests.scm")
;; (load "tests/rsa-tests.scm")
(load "tests/tar-tests.scm")
(load "tests/term-ansi-tests.scm")
(cond-expand (full-unicode (load "tests/unicode-tests.scm")) (else #f))

(cond-expand
 (modules
  (load "tests/record-tests.scm")
  (load "tests/hash-tests.scm")
  (load "tests/sort-tests.scm")
  (load "tests/parse-tests.scm")
  ;; (load "tests/weak-tests.scm")
  (load "tests/io-tests.scm")
  (load "tests/process-tests.scm")
  (load "tests/system-tests.scm")
  )
 (else #f))

(cond-expand
 ((and modules threads)
  (load "tests/thread-tests.scm"))
 (else #f))

(test-end)
