
(define-library (chibi test)
  (export
   ;; basic interface
   test test-equal test-error test-assert test-not test-values
   test-group current-test-group
   test-begin test-end test-syntax-error test-propagate-info
   test-run test-exit test-equal?
   ;; test and group data
   test-get-name! test-group-name test-group-ref
   test-group-set! test-group-inc! test-group-push!
   ;; parameters
   current-test-verbosity
   current-test-applier current-test-skipper current-test-reporter
   current-test-group-reporter test-failure-count
   current-test-epsilon current-test-comparator
   current-test-filters current-test-removers
   current-test-group-filters current-test-group-removers
   current-column-width)
  (import (scheme base)
          (scheme write)
          (scheme complex)
          (scheme process-context)
          (scheme time)
          (chibi diff)
          (chibi term ansi))
  (cond-expand
   (chibi
    (import (only (chibi) pair-source print-exception)))
   (chicken
    (import (only (chicken) print-error-message))
    (begin
      (define (pair-source x) #f)
      (define print-exception print-error-message)))
   (else
    (begin
      (define (pair-source x) #f)
      (define print-exception write))))
  (include "test.scm"))
