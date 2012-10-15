
(define-library (scheme process-context)
  (import (chibi) (srfi 98) (only (chibi process) exit))
  (export get-environment-variable get-environment-variables
          command-line exit emergency-exit)
  ;; TODO: Make exit unwind and finalize properly.
  (begin (define emergency-exit exit)))
