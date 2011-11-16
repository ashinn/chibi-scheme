
(define-library (scheme process-context)
  (import (scheme) (srfi 98) (only (chibi process) exit))
  (export get-environment-variable get-environment-variables
          command-line exit))
