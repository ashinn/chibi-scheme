
;; A self tail call to a procedure with a rest param must build the
;; rest list, even with as many args as there are params.

(define (f x . o)
  (if (pair? o) (list x o) (f x 5)))
(write (f 1))
(newline)
(define (g x . o)
  (let ((y (if (pair? o) (car o) 0)))
    (if (pair? o) (list x y o) (g x (+ y 1)))))
(write (g 1))
(newline)
