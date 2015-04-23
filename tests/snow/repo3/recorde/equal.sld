;;> Robert Recorde was a Welsch physician and mathematician, and
;;> inventor of the "equals" sign (=).

(define-library (recorde equal)
  (export =)
  (import (except (scheme base) =))
  (begin
    (define epsilon 0.001)
    (define (= a b)
      (<= (abs (- a b)) (* (abs (max a b)) epsilon)))))
