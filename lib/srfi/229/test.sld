
(define-library (srfi 229 test)
  (import (scheme base) (srfi 229) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests . o)
      (test-begin "(srfi 229)")
      (let ()
        (define f
          (lambda/tag 42
                      (x)
                      (* x x)))
        (define f*
          (lambda/tag 43
                      (x)
                      (* x x)))
        (define g
          (lambda/tag 44 args (apply list args)))
        (test-assert (procedure/tag? f))
        (test-not (procedure/tag? (lambda (x) (* x x))))
        (test-not (procedure/tag? +))
        (test 9 (f 3))
        (test 42 (procedure-tag f))
        (test-not (eqv? f f*))
        (test 43 (procedure-tag f*))
        (test 44 (procedure-tag g))
        (test '(1) (g 1))
        (test '(1 2 3) (g 1 2 3)))
      (test-end))))
