;; Named lets whose name is only called in tail position are compiled
;; natively in place, with the calls as jumps.

;; in and out of tail position
(define (sum ls)
  (let lp ((ls ls) (acc 0))
    (if (null? ls) acc (lp (cdr ls) (+ acc (car ls))))))
(define (plus-sum ls)
  (+ 100 (let lp ((ls ls) (acc 0))
           (if (null? ls) acc (lp (cdr ls) (+ acc (car ls)))))))
(define (sum-then ls)
  (let ((s (let lp ((ls ls) (acc 0))
             (if (pair? ls) (lp (cdr ls) (+ acc (car ls))) acc))))
    (let lp ((i 0) (res '()))
      (if (< i 2) (lp (+ i 1) (cons s res)) res))))
(write (list (sum '(1 2 3)) (plus-sum '(1 2 3)) (sum-then '(1 2 3))))
(newline)

;; nested, jumping from the inner loop to the outer
(define (pairs n)
  (let lp ((i 0) (res '()))
    (if (< i n)
        (let lp2 ((j 0) (res res))
          (if (< j i) (lp2 (+ j 1) (cons (list i j) res)) (lp (+ i 1) res)))
        (reverse res))))
(write (pairs 3))
(newline)

;; closures capture each iteration's values
(define (thunks)
  (let lp ((i 0) (acc '()))
    (if (< i 3)
        (lp (+ i 1) (cons (lambda () i) acc))
        (map (lambda (f) (f)) acc))))
(write (thunks))
(newline)

;; not inlined: lp escapes, is called in a non-tail position, its
;; param is mutated, or its body has internal defines
(define (escapes) (let lp ((i 0)) (if (< i 3) (lp (+ i 1)) lp)))
(define (non-tail ls) (let lp ((ls ls)) (if (null? ls) 0 (+ (car ls) (lp (cdr ls))))))
(define (mutated) (let lp ((i 0)) (set! i (+ i 1)) (if (< i 3) (lp i) i)))
(define (defines n)
  (let lp ((i 0))
    (define (next) (+ i 1))
    (if (< i n) (lp (next)) i)))
(write (list (procedure? (escapes)) (non-tail '(1 2 3)) (mutated) (defines 4)))
(newline)

;; the procedure's own calls from the loop's tail are tail calls only
;; when the loop is in the procedure's tail position
(define (f n)
  (let lp ((i 0))
    (cond ((< i 2) (lp (+ i 1))) ((> n 0) (f (- n 1))) (else 'done))))
(define (g n)
  (if (= n 0) 0 (+ 1 (let lp ((i 0)) (if (< i 1) (lp (+ i 1)) (g (- n 1)))))))
(define (even2? n)
  (let lp ((k 0)) (if (< k 1) (lp (+ k 1)) (if (= n 0) #t (odd2? (- n 1))))))
(define (odd2? n)
  (let lp ((k 0)) (if (< k 1) (lp (+ k 1)) (if (= n 0) #f (even2? (- n 1))))))
(write (list (f 100000) (g 5) (even2? 100000)))
(newline)

;; unboxed flonum params
(define (halves n)
  (let lp ((i 0) (x 0.0))
    (if (< i n) (lp (+ i 1) (+ x 0.5)) x)))
(write (list (halves 10) (* 2.0 (let lp ((x 1.0)) (if (< x 100.0) (lp (* x 3.0)) x)))))
(newline)
