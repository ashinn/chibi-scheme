
;; Don't import this - it's temporarily used to compute optimized
;; char-set representations.

(define-library (chibi char-set compute)
  (import (chibi) (chibi iset) (chibi char-set))
  (include "derived.scm" "compute.scm")
  (export
   char-set:lower-case
   char-set:upper-case
   char-set:title-case
   char-set:letter
   char-set:punctuation
   char-set:symbol
   char-set:blank
   char-set:whitespace
   char-set:digit
   char-set:letter+digit
   char-set:hex-digit
   char-set:iso-control
   char-set:graphic
   char-set:printing))
