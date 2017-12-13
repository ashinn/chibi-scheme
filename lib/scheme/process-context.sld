
(define-library (scheme process-context)
  (import (chibi) (srfi 98))
  (cond-expand (windows (import (only (chibi win32 process-win32) exit)))
               (else (import (only (chibi process) exit))))
  (export get-environment-variable get-environment-variables
          command-line exit emergency-exit)
  ;; TODO: Make exit unwind and finalize properly.
  (begin (define emergency-exit exit)))
