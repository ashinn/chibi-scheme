(define-library (srfi 273 test)
  (import (scheme base)
          (scheme eval)
          (scheme repl)
          (srfi 253)
          (srfi 273)
          (chibi test))
  (export run-tests)
  (begin
    (define-syntax check-arg-true
      (syntax-rules ()
        ((_ pred val)
         (begin
           (check-arg pred val)
           #t))))
    (define (run-tests)
      (define-check email? string?)
      (define-check positive-integer?
        (lambda (x) (and (integer? x) (positive? x))))

      (test-begin "srfi-273: extensions to data (type-)checking")
      (test-group "define-check"
        (test-assert (check-arg-true email? "srfi-273@srfi.schemers.org"))
        (test-assert (check-arg-true positive-integer? 3))
        (test-error (check-arg-true positive-integer? 0))
        (test-error (check-arg-true positive-integer? -8)))

      (test-group "define-values-checked"
        (test-assert (begin
                       (define-values-checked (quot rem) (integer? integer?)
                         (truncate/ 1 2))
                       #t))
        (test-assert (begin
                       (define-values-checked (a) (real?) 3)
                       #t))
        (test-error (begin
                      (define-values-checked (quot rem) (integer? string?)
                        (truncate/ 1 2))
                      #t))
        (test-error (define-values-checked (a) (string?) 3))
        ;; Ensure that symbols are not bound on type mismatch
        (test-error (define-values-checked (x y) (integer? string?)
                      (truncate/ 1 2)))
        (test-error (eval 'x (interaction-environment)))
        (test-error (eval 'y (interaction-environment))))

      (test-end))))
