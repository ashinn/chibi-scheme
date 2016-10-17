
(define-library (chibi binary-record)
  (import (scheme base)
          (srfi 1)
          (chibi string))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (cond-expand
   (chibi
    (import (only (chibi) identifier? er-macro-transformer)))
   (chicken
    (import chicken)
    (begin
      (define identifier? symbol?)))
   (sagittarius
    (import (sagittarius))
    (begin
      (define identifier? symbol?))))
  (export define-binary-record-type)
  (include "binary-record.scm"))
