
(define-library (chibi iset iterators)
  (cond-expand
   (chibi (import (chibi) (srfi 9)))
   (else (import (scheme base))))
  (import (chibi iset base))
  (cond-expand
   ((library (srfi 60)) (import (srfi 60)))
   (else (import (srfi 33))))
  (include "iterators.scm")
  (export
   iset-empty? iset-fold iset-fold-node iset-for-each iset-for-each-node
   iset->list iset-size iset= iset<= iset>=
   ;; low-level cursors
   iset-cursor iset-cursor? iset-cursor-next iset-ref end-of-iset?))
