
(define-library (chibi binary-record)
  (import (scheme base) (srfi 1))
  (cond-expand
   ((library (srfi 151)) (import (srfi 151)))
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (cond-expand
   ((library (srfi 130)) (import (srfi 130)))
   (else (import (srfi 13))))
  (cond-expand
   ;; ((library (auto))
   ;;  (import (only (auto) make: pred: read: write: block:)))
   (else
    ;; indirect exports for chicken
    (export defrec define-auxiliary-syntax syntax-let-optionals*)
    (begin
      (define-syntax define-auxiliary-syntax
        (syntax-rules ()
          ((define-auxiliary-syntax name)
           (define-syntax name
             (syntax-rules ()
               ((name . x)
                (syntax-error "invalid use of auxiliary syntax"
                              (name . x))))))))
      (define-auxiliary-syntax make:)
      (define-auxiliary-syntax pred:)
      (define-auxiliary-syntax read:)
      (define-auxiliary-syntax write:)
      (define-auxiliary-syntax block:))))
  (export
   ;; interface
   define-binary-record-type
   ;; binary types
   u8 u16/le u16/be padded-string fixed-string
   octal decimal hexadecimal
   ;; auxiliary syntax
   make: pred: read: write: block:
   ;; new types
   define-binary-type)
  (include "binary-types.scm")
  (cond-expand
   (chicken
    (include "binary-record-chicken.scm"))
   (else
    (include "binary-record.scm"))))
