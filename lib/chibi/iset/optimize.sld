
(define-library (chibi iset optimize)
  (import (chibi) (srfi 9) (srfi 33)
          (chibi iset base)
          (chibi iset iterators)
          (chibi iset constructors))
  (include "optimize.scm")
  (export
   iset-balance iset-balance! iset-optimize iset-optimize! iset->code))
