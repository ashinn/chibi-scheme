
(define-library (chibi binary-record)
  (import (scheme base)
          (srfi 1) (srfi 9)
          (chibi io) (chibi string)
          (only (chibi) identifier? er-macro-transformer))
  (cond-expand
   ((library (srfi 60)) (import (srfi 60)))
   (else (import (srfi 33))))
  (export define-binary-record-type)
  (include "binary-record.scm"))
