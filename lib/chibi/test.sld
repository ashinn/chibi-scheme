
(define-library (chibi test)
  (export
   ;; basic interface
   test test-equal test-error test-assert test-not test-values
   test-group current-test-group
   test-begin test-end test-syntax-error test-propagate-info
   test-run test-skip test-exit test-equal?
   ;; test and group data
   test-get-name! test-group-name test-group-ref
   test-group-set! test-group-inc! test-group-push!
   ;; parameters
   current-test-value-formatter current-test-verbosity
   current-test-applier current-test-skipper current-test-reporter
   current-test-group-reporter test-failure-count
   current-test-epsilon current-test-comparator
   current-test-filters current-test-removers
   current-test-group-filters current-test-group-removers
   current-column-width current-group-indent)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (scheme complex)
          (scheme process-context)
          (scheme time)
          (chibi diff)
          (chibi term ansi)
          (chibi optional))
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
  (cond-expand
   ((library (srfi 13))
    (import (only (srfi 13) string-contains)))
   ((library (srfi 130))
    (import (only (srfi 130) string-contains)))
   (else
    (define (string-contains str pat)
      (let* ((pat-len (string-length pat))
             (limit (- (string-length str) pat-len)))
        (let lp1 ((i 0))
          (cond
           ((> i limit) #f)
           (else
            (let lp2 ((j i) (k 0))
              (cond ((>= k pat-len) #t)
                    ((not (eqv? (string-ref str j) (string-ref pat k)))
                     (lp1 (+ i 1)))
                    (else (lp2 (+ j 1) (+ k 1))))))))))))
  (include "test.scm"))
