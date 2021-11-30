(define-library (srfi 229 test)
  (export run-tests)
  (import (scheme base)
          (chibi test)
          (srfi 229))
  (begin
    (define (run-tests)
      (test-group
       "srfi-229: tagged procedures"

       (define f
         (lambda/tag 42
             (x)
           (* x x)))

       (define f*
         (lambda/tag 43
             (x)
           (* x x)))

       (define g
         (let ((y 10))
           (lambda/tag y ()
             (set! y (+ y 1))
             y)))

       (define h
         (let ((box (vector #f)))
           (case-lambda/tag box
            (() (vector-ref box 0))
            ((val) (vector-set! box 0 val)))))

       (test #t (procedure/tag? f))
       (test 9 (f 3))
       (test 42 (procedure-tag f))
       (test #f (eqv? f f*))
       (test 10 (procedure-tag g))
       (test 10 (let ((y 9)) (procedure-tag g)))
       (test 11 (g))
       (test 10 (procedure-tag g))
       (h 1)
       (test 1 (vector-ref (procedure-tag h) 0))
       (test 1 (h))))))
