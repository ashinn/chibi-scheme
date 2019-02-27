
(define-library (chibi show base)
  (export
   show fn forked with with! each each-in-list call-with-output
   displayed written written-shared written-simply numeric nothing
   escaped maybe-escaped numeric/si numeric/fitted numeric/comma
   ;; internal
   output-default extract-shared-objects write-to-string write-with-shares
   call-with-shared-ref call-with-shared-ref/cdr)
  (import (scheme base) (scheme write) (scheme complex) (scheme inexact)
          (srfi 1) (srfi 69) (chibi string) (chibi monad environment))
  (cond-expand
   (chibi
    (import (only (chibi) let-optionals*)))
   (else
    (begin
      (define-syntax let-optionals*
        (syntax-rules ()
          ((let-optionals* opt-ls () . body)
           (begin . body))
          ((let-optionals* (op . args) vars . body)
           (let ((tmp (op . args)))
             (let-optionals* tmp vars . body)))
          ((let-optionals* tmp ((var default) . rest) . body)
           (let ((var (if (pair? tmp) (car tmp) default))
                 (tmp2 (if (pair? tmp) (cdr tmp) '())))
             (let-optionals* tmp2 rest . body)))
          ((let-optionals* tmp tail . body)
           (let ((tail tmp)) . body)))))))
  (include "base.scm")
  (include "write.scm"))
