
(define-library (chibi log-test)
  (export run-tests)
  (import (scheme base) (scheme inexact) (srfi 130)
          (chibi log) (chibi show) (chibi test))
  (begin
    (define-syntax log->string
      (syntax-rules ()
        ((log->string expr ...)
         (let ((out (open-output-string)))
           (parameterize ((current-error-port out))
             (log-open default-logger)
             expr ...
             (get-output-string out))))))
    (define-syntax log->string/no-dates
      (syntax-rules ()
        ((log->string/no-dates expr ...)
         (string-join
          (map (lambda (line) (substring line 20))
               (string-split (log->string expr ...) "\n"))
          "\n"))))
    (define (run-tests)
      (test-begin "(chibi log)")
      (test "D four: 4"
          (log->string/no-dates
           (log-debug "four: " (+ 2 2))))
      (test "I pi: 3.14"
          (log->string/no-dates
           (log-info "pi: " (with ((precision 2)) (acos -1)))))
      (test-assert
        (string-prefix? "E "
                        (log->string/no-dates
                         (with-logged-errors (/ 1 0)))))
      (test "W warn\nE error"
          (log->string/no-dates
           (with-log-level
            'warn
            (log-info "info")
            (log-warn "warn")
            (log-error "error"))))
      (test-end))))
