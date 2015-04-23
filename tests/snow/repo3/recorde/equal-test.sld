(define-library (recorde equal-test)
  (export run-tests test-exit)
  (import (except (scheme base) =)
          (scheme inexact)
          (scheme process-context)
          (scheme write)
          (recorde equal))
  (begin
    (define failed? #f)
    (define (set-failed!)
      (set! failed? #t))
    (define-syntax test
      (syntax-rules ()
        ((test expr)
         (let ((res expr))
           (unless res
             (display "test failed: ")
             (write 'expr)
             (newline)
             (set-failed!))))))
    (define (test-exit)
      (when failed?
        (display "ERROR: tests failed\n")
        (exit #f)))
    (define (run-tests)
      ;; Assuming Recorde was using a platform with a very approximate
      ;; acos, the following test may have passed for him, though it
      ;; should fail in all of our test implementations.
      (test (= 3 (acos -1)))
      (test-exit))))
