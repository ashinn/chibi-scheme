;; first bernoulli numbers
(define (bernoulli m)
  (do ((k 0 (+ k 1))
       (sum 0 (do ((v 0 (+ v 1))
                   (sum sum (+ sum
                               (* (expt -1 v)
                                  (binomial k v)
                                  (/ (expt v m) (+ k 1))))))
                  ((> v k) sum))))
      ((> k m) sum)))
