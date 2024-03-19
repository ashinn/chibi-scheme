
(define-library (scheme process-context)
  (import (chibi) (only (scheme base) call/cc) (srfi 98))
  (cond-expand (windows (import (prefix (only (chibi win32 process-win32) exit) process-)))
               (else (import (prefix (only (chibi process) exit) process-))))
  (export get-environment-variable get-environment-variables
          command-line exit emergency-exit)

  (begin
    (define unwind #f)

    ((call/cc
        (lambda (cont)
          (set! unwind cont)
          (lambda () #f))))

    (define emergency-exit process-exit)

    (define (exit . rest)
      (unwind (lambda () (apply emergency-exit rest))))))
