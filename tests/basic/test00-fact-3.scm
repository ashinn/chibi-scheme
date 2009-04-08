
(define (fact-helper x res)
  (if (= x 0)
      res
      (fact-helper (- x 1) (* res x))))

(define (fact x)
  (fact-helper x 1))

(display "(fact 3) => ")
(write (fact 3))
(newline)


