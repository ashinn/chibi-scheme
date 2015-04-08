(define (binomial n k)
  (/ (factorial n)
     (* (factorial k)
        (factorial (- n k)))))
