(define-library (chibi temp-file)
  (export call-with-temp-file call-with-temp-dir)
  (import (scheme base) (scheme time)
          (chibi filesystem) (chibi pathname))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (cond-expand
   (chibi (import (only (chibi process) current-process-id)))
   (chicken (import (only (posix) current-process-id)))
   (else (begin (define (current-process-id) 0))))
  (include "temp-file.scm"))
