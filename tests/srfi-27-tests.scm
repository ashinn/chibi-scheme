
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
      (test-random rand n))
    (let* ((state (random-source-state-ref rs))
           (x (rand 1000000)))
      ;; the next int won't be the same, but it will be after
      ;; resetting the state
      (test-not (= x (rand 1000000)))
      (random-source-state-set! rs state)
      (test x (rand 1000000)))))

(test-end)
