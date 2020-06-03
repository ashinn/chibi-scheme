
;; Don't import this - it's temporarily used to compute optimized
;; char-set representations.

(define-library (chibi char-set width)
  (import (chibi) (chibi iset) (chibi char-set))
  (include "width.scm")
  (export
   char-set:zero-width
   char-set:full-width
   char-set:ambiguous-width
   ))
