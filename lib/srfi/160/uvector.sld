
(define-library (srfi 160 uvector)
  (import (scheme base) (scheme write))
  (export define-uvector-procedures)
  (begin
    (define-syntax define-uvector-procedures
      (syntax-rules ()
        ((define-uvector-procedures
           ;; primitives supplied
           u? uvector? make-uvector
           uvector-length uvector-ref uvector-set!
           ;; derived
           uvector uvector-unfold uvector-unfold-right
           uvector-copy uvector-reverse-copy uvector-append
           uvector-concatenate uvector-append-subvectors
           uvector-empty? uvector=
           uvector-take uvector-take-right
           uvector-drop uvector-drop-right
           uvector-segment uvector-fold uvector-fold-right
           uvector-map uvector-map! uvector-for-each
           uvector-count uvector-cumulate
           uvector-take-while uvector-take-while-right
           uvector-drop-while uvector-drop-while-right
           uvector-index uvector-index-right
           uvector-skip uvector-skip-right
           uvector-binary-search uvector-any uvector-every
           uvector-partition uvector-filter uvector-remove
           uvector-swap! uvector-fill!
           uvector-reverse! uvector-copy! uvector-reverse-copy!
           uvector->list reverse-uvector->list list->uvector
           uvector->vector vector->uvector
           make-uvector-generator write-uvector)
         (begin

           ))))))
