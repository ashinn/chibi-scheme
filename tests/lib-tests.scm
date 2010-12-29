
(import (chibi test))

(test-begin "libraries")

(load "tests/flonum-tests.scm")
(load "tests/numeric-tests.scm")
(load "tests/hash-tests.scm")
(load "tests/sort-tests.scm")
(load "tests/loop-tests.scm")
(load "tests/match-tests.scm")
(load "tests/record-tests.scm")
(load "tests/thread-tests.scm")
;;(load "tests/weak-tests.scm")
(cond-expand (utf-8 (load "tests/unicode-tests.scm")) (else #f))

(test-end)
