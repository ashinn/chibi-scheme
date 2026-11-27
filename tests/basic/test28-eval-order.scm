;; Native code evaluates args right to left, as the VM does, whenever
;; the order could show: calls, lets, opcodes, loop and self jumps.

(define trace '())
(define (note x) (set! trace (cons x trace)) x)
(define (traced thunk) (set! trace '()) (thunk) (reverse trace))

(define (f a b c) (list a b c))
(define (loop n) (let lp ((i n) (a 0) (b 0)) (if (> i 0) (lp (- i 1) (note 'a) (note 'b)) 'done)))
(define (self n a) (if (> n 0) (self (note (- n 1)) (note 'a)) a))
(define (fl) (+ (note 1.5) (note 2.5)))
(define (cmp) (if (< (note 1) (note 2)) 'yes 'no))
(define (pairs ls) (if (pair? ls) (cons (note (car ls)) (pairs (cdr ls))) '()))

(write (list (traced (lambda () (f (note 1) (note 2) (note 3))))
             (traced (lambda () (let ((x (note 1)) (y (note 2))) (+ x y))))
             (traced (lambda () (cons (note 1) (note 2))))
             (traced (lambda () (loop 1)))
             (traced (lambda () (self 1 0)))
             (traced fl)
             (traced cmp)
             (traced (lambda () (pairs '(1 2 3))))))
(newline)

;; reads of what another arg changes see the change, or not, alike
(define v (vector 0))
(define (bump!) (vector-set! v 0 (+ (vector-ref v 0) 1)) 10)
(define (sum-bump) (+ (vector-ref v 0) (bump!)))
(write (list (sum-bump) (sum-bump)))
(newline)

;; backtracking through continuations finds the same answer first
(define fail (lambda () 'none))
(define (in-range a b)
  (call-with-current-continuation
   (lambda (k)
     (let try ((a a))
       (if (> a b)
           (fail)
           (let ((save fail))
             (set! fail (lambda () (set! fail save) (try (+ a 1))))
             (k a)))))))
(write (let ((x (in-range 1 9)) (y (in-range 1 9)) (z (in-range 1 9)))
         (if (= (* z z) (+ (* x x) (* y y))) (list x y z) (fail))))
(newline)
