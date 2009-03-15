
(define (fact-helper x res)
  (if (zero? x)
      res
      (fact-helper (- x 1) (* res x))))

(define (fact x)
  (fact-helper x 1))

(display "(fact 3) => ")
(write (fact 3))
(newline)


