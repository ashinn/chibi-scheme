
(define-library (scheme lseq)
  (import (srfi 127))
  (export
   ;; Constructors
   generator->lseq 
   ;; Predicates
   lseq?         lseq=?
   ;; Selectors
   lseq-car     lseq-cdr
   lseq-first   lseq-rest lseq-ref
   lseq-take    lseq-drop   
   ;; The whole lazy sequence
   lseq-realize lseq->generator
   lseq-length
   lseq-append  lseq-zip
   ;; Mapping and filtering
   lseq-map        lseq-for-each
   lseq-filter     lseq-remove
   ;; Searching
   lseq-find         lseq-find-tail 
   lseq-any          lseq-every
   lseq-index
   lseq-take-while   lseq-drop-while
   lseq-member       lseq-memq     lseq-memv))
