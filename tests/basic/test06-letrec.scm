
(letrec ((add (lambda (a b) (+ a b))))
  (write (add 3 4))
  (newline))

(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
         (odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))))
  (write (even? 1000))
  (newline)
  (write (even? 1001))
  (newline)
  (write (odd? 1000))
  (newline)
  )

