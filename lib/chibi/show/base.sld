
(define-library (chibi show base)
  (export
   show fn fn-fork with update! each each-in-list call-with-output
   displayed written written-shared written-simply numeric nothing
   ;; internal
   output-default extract-shared-objects write-to-string write-with-shares
   call-with-shared-ref call-with-shared-ref/cdr)
  (import (scheme base) (scheme write) (scheme complex) (scheme inexact)
          (srfi 1) (srfi 69) (chibi string) (chibi monad environment))
  (include "base.scm")
  (include "write.scm"))
