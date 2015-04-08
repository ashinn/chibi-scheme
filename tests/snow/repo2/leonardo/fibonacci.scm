(define (fib n)
  (let lp ((n n) (a 1) (b 1))
    (if (< n 2)
        a
        (lp (- n 1) (+ a b) a))))
