;;;; these will fail when compiled either without flonums or trig funcs

(cond-expand
 (modules (import (only (chibi test) test-begin test-assert test test-end)))
 (else #f))

(test-begin "floating point")

(test-assert (= -5 (floor -4.3)))
(test-assert (= -4 (ceiling -4.3)))
(test-assert (= -4 (truncate -4.3)))
(test-assert (= -4 (round -4.3)))
(test-assert (= 3 (floor 3.5)))
(test-assert (= 4 (ceiling 3.5)))
(test-assert (= 3 (truncate 3.5)))
(test-assert (= 4 (round 3.5)))

(test 1124378190243790143.0 (exact->inexact 1124378190243790143))

;; (test "1124378190243790143.0"
;;     (number->string (exact->inexact 1124378190243790143)))

(test-end)
