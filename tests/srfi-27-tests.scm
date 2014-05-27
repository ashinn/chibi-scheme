
(import (scheme base)
        (srfi 27)
        (chibi test))

(test-begin "srfi-27")

(define (test-random rand n)
  (test-assert (<= 0 (rand n) (- n 1))))

(let ((rs (make-random-source)))
  ;; chosen by fair dice roll.  guaranteed to be random
  (random-source-pseudo-randomize! rs 4 4)
  (let ((rand (random-source-make-integers rs)))
    (do ((k 0 (+ k 5))
         (n 1 (* n 2)))
        ((> k 1024))
      (test-random rand n))))

(test-end)
