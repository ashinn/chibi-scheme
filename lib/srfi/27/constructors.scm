
(define (random-source-make-integers rs)
  (lambda (n) (%random-integer rs n)))

(define (random-source-make-reals rs . o)
  (lambda () (%random-real rs)))

