
(let ((a 3)
      (b 5))
  (let ((c (- a 2))
        (d (+ b 2))
        (e #f))
    (set! e 10000)
    (write (+ e (* c 1000) (* a 100) (* b 10) d))
    (newline)))
