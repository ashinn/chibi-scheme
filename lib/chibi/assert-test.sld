
(define-library (chibi assert-test)
  (import (chibi) (chibi assert) (chibi test))
  (export run-tests)
  (begin
    (define-syntax test-assert
      (syntax-rules ()
        ((test-assert irritants expr)
         (protect (exn
                   (else
                    (test irritants (exception-irritants exn))))
           expr
           (error "assertion not triggered")))))
    (define (run-tests)
      (test-begin "assert")
      (test-assert '((= x (+ x 1))
                     (x 3))
        (let ((x 3)) (assert (= x (+ x 1)))))
      (test-assert '((= x (+ y 1))
                     (x 3)
                     (y 42))
        (let ((x 3) (y 42)) (assert (= x (+ y 1)))))
      (test-assert '((eq? x 'three)
                     (x 3))
        (let ((x 3)) (assert (eq? x 'three))))
      (test-assert '((eq? x 'three)
                     "expected three: "
                     3)
        (let ((x 3)) (assert (eq? x 'three) "expected three: " x)))
      (test-end))))
