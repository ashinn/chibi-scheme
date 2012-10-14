
(define-library (chibi iset base)
  (import (chibi) (srfi 9) (srfi 33))
  (include "base.scm")
  (export
   %make-iset make-iset iset? iset-contains? Integer-Set
   iset-start iset-end iset-bits iset-left iset-right
   iset-start-set! iset-end-set! iset-bits-set! iset-left-set! iset-right-set!))
