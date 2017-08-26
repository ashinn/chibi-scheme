(define-library (srfi 139 test)
  (export run-tests)
  (import (scheme base)
          (chibi test)
          (srfi 139))
  (begin
    (define-syntax-parameter abort
      (syntax-rules ()
        ((_ . _)
         (syntax-error "abort used outside of a loop"))))

    (define-syntax-parameter foo
      (syntax-rules ()
        ((foo) 'old)))

    (define-syntax forever
      (syntax-rules ()
        ((forever body1 body2 ...)
         (call-with-current-continuation
          (lambda (escape) 
            (syntax-parameterize
             ((abort
               (syntax-rules ()
                 ((abort value (... ...))
                  (escape value (... ...))))))
             (let loop ()
               body1 body2 ... (loop))))))))

    (define (run-tests)
      (test-begin "srfi-139: syntax parameters")

      (test (list 'old 'new)
          (let ((new
                 (syntax-parameterize
                  ((foo (syntax-rules ()
                          ((foo) 'new))))
                  (foo))))
            (list (foo) new)))

      (test 10
          (let ((i 0))
            (forever
             (set! i (+ 1 i))
             (when (= i 10)
               (abort)))
            i))

      (test-end))))
