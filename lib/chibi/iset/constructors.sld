
(define-library (chibi iset constructors)
  (import (chibi) (srfi 33) (chibi iset base) (chibi iset iterators))
  (include "constructors.scm")
  (export
   iset iset-copy list->iset list->iset! iset-map
   iset-adjoin iset-adjoin! iset-delete iset-delete!
   iset-union iset-union! iset-intersection iset-intersection!
   iset-difference iset-difference!
   ;; low-level
   iset-copy-node iset-squash-bits! iset-insert-left! iset-insert-right!))
