(define-library (pingala factorial)
  (export factorial)
  (import (scheme base))
  (begin
    (define (factorial n)
      (let lp ((n n) (res 1))
        (if (<= n 1) res (lp (- n 1) (* res n)))))))
