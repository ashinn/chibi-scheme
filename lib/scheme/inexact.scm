
(define (nan? x)
  (and (real? x) (not (= x x))))

(define (finite? x)
  (if (real? x)
      (and (not (nan? x)) (not (= x +inf.0)) (not (= x -inf.0)))
      (and (complex? x) (finite? (real-part x)) (finite? (imag-part x)))))

(define (infinite? x)
  (and (number? x) (not (finite? x)) (not (nan? x))))
