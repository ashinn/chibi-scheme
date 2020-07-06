
(define-library (srfi 159 base)
  (export
   show displayed written written-shared written-simply
   escaped maybe-escaped
   numeric numeric/comma numeric/si numeric/fitted
   nl fl space-to tab-to nothing each each-in-list
   joined joined/prefix joined/suffix joined/last joined/dot
   joined/range padded padded/right padded/both
   trimmed trimmed/right trimmed/both trimmed/lazy
   fitted fitted/right fitted/both output-default
   fn with with! forked call-with-output)
  (import (scheme base) (scheme write) (scheme complex) (scheme inexact)
          (srfi 1) (srfi 69) (srfi 130) (chibi monad environment)
          (chibi show shared))
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
  (include "../166/show.scm")
  (include "../166/write.scm"))
