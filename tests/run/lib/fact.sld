(define-library (fact)
  (export fact)
  (import (scheme base))
  (begin
    (define (fact n)
      (if (< n 2)
          1
          (* n (fact (- n 1)))))))
