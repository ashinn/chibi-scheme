(import (scheme base) (scheme process-context) (pythagoras hypotenuse))

(define (test expect expr)
  (cond
   ((not (equal? expect expr))
    (write-string "FAIL\n")
    (exit #f))))

(test 5.0 (hypotenuse 3.0 4.0))
(test 13.0 (hypotenuse 5.0 12.0))
(test 25.0 (hypotenuse 7.0 24.0))
(test 17.0 (hypotenuse 8.0 15.0))
(test 41.0 (hypotenuse 9.0 40.0))
(test 61.0 (hypotenuse 11.0 60.0))
