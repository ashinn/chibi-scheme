
(define-library (scheme process-context)
  (import (chibi) (srfi 98))
  (cond-expand (windows (import (rename (only (chibi win32 process-win32) exit) (exit emergency-exit))))
               (else (import (rename (only (chibi process) exit) (exit emergency-exit)))))
  (export get-environment-variable get-environment-variables
          command-line exit emergency-exit)

  (begin 
    (define unwind #f)

    ((call/cc
        (lambda (continuation)
          (set! unwind continuation)
          (lambda () #f))))

    (define (exit . rest)
      (unwind (lambda () (apply emergency-exit rest))))))
