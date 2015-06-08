
(define-library (chibi binary-record)
  (import (scheme base)
          (srfi 1) (srfi 9)
          (chibi io) (chibi string)
          (only (chibi) identifier? er-macro-transformer))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (export define-binary-record-type)
  (include "binary-record.scm"))
