
((lambda (a b)
   ((lambda (c d e)
      (write (+ e (* c 1000) (* a 100) (* b 10) d))
      (newline))
    (- a 2) (+ b 2) 10000))
 3 5)

