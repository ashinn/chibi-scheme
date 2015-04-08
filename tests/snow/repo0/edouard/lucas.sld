(define-library (edouard lucas)
  (export lucas)
  (import (scheme base))
  (begin
    (define (lucas n)
      (if (< n 2)
          (if (= n 1) 1 2)
          (+ (lucas (- n 1)) (lucas (- n 2)))))))
