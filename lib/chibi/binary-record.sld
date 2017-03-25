
(define-library (chibi binary-record)
  (import (scheme base) (srfi 1))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (cond-expand
   ((library (srfi 130)) (import (srfi 130)))
   (else (import (srfi 13))))
  (export
   ;; interface
   define-binary-record-type
   ;; binary types
   u8 u16/le u16/be padded-string fixed-string
   octal decimal hexadecimal
   ;; auxiliary syntax
   make: pred: read: write: block:
   ;; indirect exports
   define-binary-type defrec define-auxiliary-syntax
   syntax-let-optionals*)
  (include "binary-types.scm")
  (cond-expand
   (chicken
    (include "binary-record-chicken.scm"))
   (else
    (include "binary-record.scm"))))
