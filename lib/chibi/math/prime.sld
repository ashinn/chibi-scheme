
(define-library (chibi math prime)
  (import (scheme base) (scheme inexact) (srfi 27) (srfi 33))
  (export prime? nth-prime prime-above prime-below factor perfect? totient
          provable-prime? probable-prime?
          random-prime random-prime-distinct-from
          coprime? random-coprime modular-inverse modular-expt
          miller-rabin-composite?)
  (include "prime.scm"))
