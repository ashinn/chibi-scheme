
(define-library (chibi optional-test)
  (import (scheme base) (chibi optional))
  (cond-expand
   (chibi (import (chibi test)))
   (else
    (import (scheme write))
    ;; inline (chibi test) to avoid circular dependencies in snow
    ;; installations
    (begin
      (define-syntax test
       (syntax-rules ()
         ((test expect expr)
          (test 'expr expect expr))
         ((test name expect expr)
          (guard (exn (else (display "!\nERROR: ") (write name) (newline)
                            (write exn) (newline)))
            (let* ((res expr)
                   (pass? (equal? expect expr)))
              (display (if pass? "." "x"))
              (cond
               ((not pass?)
                (display "\nFAIL: ") (write name) (newline))))))))
      (define-syntax test-assert
        (syntax-rules ()
          ((test-assert expr) (test #t expr))))
      (define-syntax test-error
        (syntax-rules ()
          ((test-error expr)
           (test-assert (guard (exn (else #t)) expr #f)))))
      (define (test-begin name)
        (display name))
      (define (test-end)
        (newline)))))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "optional")
      (test '(0 11 12)
          (let-optionals '(0) ((a 10) (b 11) (c 12))
            (list a b c)))
      (test '(0 11 12)
          ((opt-lambda ((a 10) (b 11) (c 12))
             (list a b c))
           0))
      (test '(0 11 12)
          ((opt-lambda (a (b 11) (c 12))
             (list a b c))
           0))
      (test '(0 1 (2 3 4))
          (let-optionals* '(0 1 2 3 4) ((a 10) (b 11) . c)
            (list a b c)))
      (test '(0 1 (2 3 4))
          (let-optionals '(0 1 2 3 4) ((a 10) (b 11) . c)
            (list a b c)))
      (cond-expand
       (gauche)     ; gauche detects this at compile-time, can't catch
       (else (test-error '(0 11 12)
                         ((opt-lambda (a (b 11) (c 12))
                            (list a b c))))))
      (let ()
        (define-opt (f a (b 11) (c 12))
          (list a b c))
        (cond-expand
         (gauche)
         (else
          (test-error (f))))
        (test '(0 11 12) (f 0))
        (test '(0 1 12) (f 0 1))
        (test '(0 1 2) (f 0 1 2))
        (test '(0 1 2) (f 0 1 2 3)))
      (test-end))))
