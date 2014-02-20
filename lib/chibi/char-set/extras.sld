
(define-library (chibi char-set extras)
  (import (chibi) (chibi iset) (chibi char-set base))
  (include "extras.scm")
  (export
   char-set ucs-range->char-set char-set-copy char-set-size
   list->char-set char-set->list string->char-set char-set->string
   char-set-adjoin! char-set-adjoin char-set-union char-set-union!
   char-set-intersection char-set-intersection!
   char-set-difference char-set-difference!
   char-set-complement char-set:empty char-set:ascii char-set:full))
