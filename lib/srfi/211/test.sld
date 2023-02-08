(define-library (srfi 211 test)
  (export run-tests)
  (import (scheme base)
          (only (chibi) er-macro-transformer)
          (srfi 211 variable-transformer)
          (srfi 211 identifier-syntax)
          (chibi test))
  (begin
    (define (run-tests)
      (test-begin "srfi-211")

      (test '(1 2)
            (let-syntax
                ((foo (identifier-syntax (list 1 2))))
              foo))

      (test '(5 5)
            (let ((x 3))
              (let-syntax
                  ((foo (identifier-syntax
                         (_ x)
                         ((set! _ e) (set! x e)))))
                (set! foo (+ foo 2))
                (list foo x))))

      (test 42
            (let-syntax
                ((foo (make-variable-transformer
                       (er-macro-transformer
                        (lambda (e r c)
                          (list-ref e 2))))))
              (set! foo 42)))

      (test-end))))

;; Local Variables:
;; mode: scheme
;; End:
