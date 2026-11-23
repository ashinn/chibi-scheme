(define-library (chibi win32 process-win32)
  (import (scheme base) (only (chibi) travel-to-point! %dk %point-depth %point-parent))
  (export exit emergency-exit)
  (cond-expand
   (windows
    (include-shared "process-win32")
    (include "process-win32.scm"))
   (else
    (import (only (chibi process) exit emergency-exit)))))
