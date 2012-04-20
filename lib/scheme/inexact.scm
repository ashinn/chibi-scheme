
(define (finite? x)
  (and (real? x) (not (nan? x)) (not (= x +inf.0)) (not (= x -inf.0))))

(define (nan? x)
  (and (real? x) (not (= x x))))
