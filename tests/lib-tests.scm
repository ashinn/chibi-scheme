
(import (chibi test))

(test-begin "libraries")

(load "tests/flonum-tests.scm")
(load "tests/numeric-tests.scm")
(load "tests/hash-tests.scm")
(load "tests/sort-tests.scm")
(load "tests/loop-tests.scm")
(load "tests/match-tests.scm")

(test-end)
