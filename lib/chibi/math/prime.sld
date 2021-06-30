
(define-library (chibi math prime)
  (import (scheme base) (scheme inexact) (chibi optional) (srfi 1) (srfi 27))
  (cond-expand
   ((library (srfi 151)) (import (srfi 151)))
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (export prime? nth-prime prime-above prime-below
          factor factor-alist perfect?
          totient aliquot
          provable-prime? probable-prime?
          random-prime random-prime-distinct-from
          coprime? random-coprime modular-inverse modular-expt
          miller-rabin-composite?)
  (include "prime.scm"))
