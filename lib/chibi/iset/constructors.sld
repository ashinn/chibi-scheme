
(define-library (chibi iset constructors)
  (cond-expand
   (chibi (import (chibi)))
   (else (import (scheme base))))
  (import (chibi iset base) (chibi iset iterators))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (include "constructors.scm")
  (export
   iset iset-copy list->iset list->iset! iset-map
   iset-adjoin iset-adjoin! iset-delete iset-delete!
   iset-union iset-union! iset-intersection iset-intersection!
   iset-difference iset-difference!
   ;; low-level
   iset-copy-node iset-squash-bits! iset-insert-left! iset-insert-right!))
