
(define-library (chibi iset optimize)
  (cond-expand
   (chibi (import (chibi) (srfi 9)))
   (else (import (scheme base))))
  (import (chibi iset base)
          (chibi iset iterators)
          (chibi iset constructors))
  (cond-expand
   ((library (srfi 60)) (import (srfi 60)))
   (else
    (import (srfi 33))
    (begin
      (define (%mask size) (bitwise-not (arithmetic-shift -1 size)))
      (define (extract-bit-field size position n)
        (bitwise-and (%mask size) (arithmetic-shift n (- position)))))))
  (include "optimize.scm")
  (export
   iset-balance iset-balance! iset-optimize iset-optimize! iset->code))
