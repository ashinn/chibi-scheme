
(define-library (scheme process-context)
  (import (chibi) (srfi 98) (only (chibi process) exit))
  (export get-environment-variable get-environment-variables
          command-line exit))
