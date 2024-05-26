
(define-library (scheme process-context)
  (import (chibi) (srfi 98))
  (cond-expand (windows (import (only (chibi win32 process-win32) exit emergency-exit)))
               (else (import (only (chibi process) exit emergency-exit))))
  (export get-environment-variable get-environment-variables
          command-line exit emergency-exit))
