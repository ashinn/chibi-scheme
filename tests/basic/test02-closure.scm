
(define (make-counter n)
  (lambda ()
    (set! n (+ n 1))
    n))

(define f (make-counter 0))
(define g (make-counter 100))

(write (f)) (newline)
(write (f)) (newline)
(write (g)) (newline)
(write (g)) (newline)
(write (f)) (newline)
(write (g)) (newline)

