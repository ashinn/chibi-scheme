(define-library (pingala test-map)
  (export test-map test-exit)
  (import (scheme base) (scheme process-context))
  (begin
    (define failed? #f)
    (define (test-exit) (exit (if failed? 1 0)))
    (define-syntax test-map
      (syntax-rules ()
        ((test-map expected proc values)
         (let ((res (map proc 'values)))
           (cond
            ((not (equal? res 'expected))
             (set! failed? #t)
             (display "FAIL: expected ")
             (write 'expected)
             (display " but got ")
             (write res)
             (newline)))))))))
