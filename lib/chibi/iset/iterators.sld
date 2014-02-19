
(define-library (chibi iset iterators)
  (import (chibi) (srfi 9) (srfi 33) (chibi iset base))
  (include "iterators.scm")
  (export
   iset-empty? iset-fold iset-fold-node iset-for-each iset-for-each-node
   iset->list iset-size iset= iset<= iset>=
   ;; low-level cursors
   iset-cursor iset-cursor? iset-cursor-next iset-ref end-of-iset?))
