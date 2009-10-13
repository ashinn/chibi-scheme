
(define (xcons a b) (cons b a))

(define (cons* x . args)
  (let lp ((rev '()) (x x) (ls args))
    (if (null? ls)
        (append-reverse rev x)
        (lp (cons x rev) (car ls) (cdr ls)))))

(define (make-list n . o)
  (let ((default (if (pair? o) (car o))))
    (let lp ((n n) (res '()))
      (if (<= n 0) res (lp (- n 1) (cons default res))))))

(define (list-tabulate n proc)
  (let lp ((n n) (res '()))
    (if (< n 0) res (lp (- n 1) (cons (proc n) res)))))

(define (list-copy ls) (reverse! (reverse ls)))

(define (circular-list x . args)
  (let ((res (cons x args)))
    (set-cdr! (last-pair res) res)
    res))

(define (iota count . o)
  (let ((start (if (pair? o) (car o) count))
        (step (if (and (pair? o) (pair? (cdr o))) (cadr o) 1)))
    (let lp ((i count) (n (- start step)) (res '()))
      (if (<= i 0)
          res
          (lp (- i 1) (- n step) (cons n res))))))

