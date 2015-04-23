(import (scheme base) (scheme process-context) (leonardo fibonacci))

(define (test expect expr)
  (cond
   ((not (equal? expect expr))
    (write-string "FAIL\n")
    (exit #f))))

(test 1 (fib 0))
(test 1 (fib 1))
(test 2 (fib 2))
(test 3 (fib 3))
(test 5 (fib 4))
(test 8 (fib 5))
(test 13 (fib 6))
