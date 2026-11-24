
;; Flonum loop and let variables used only in float arithmetic are
;; kept unboxed in native code, so check the values they take through
;; the raw paths: self tail jumps, lets, and boxing on the way out.

(define (sum-halves n)
  (let lp ((i 0) (acc 0.0))
    (if (< i n) (lp (+ i 1) (+ acc 0.5)) acc)))
(write (list (sum-halves 0) (sum-halves 5)))
(newline)

;; swapping unboxed params in a self tail call
(define (fib-ish n)
  (let lp ((i 0) (a 0.0) (b 1.0))
    (if (< i n) (lp (+ i 1) b (+ a b)) a)))
(write (list (fib-ish 10) (fib-ish 11)))
(newline)

;; unboxed lets, in and out of tail position
(define (poly n)
  (let lp ((i 0) (x 1.0) (acc 0.0))
    (if (< i n)
        (let ((x2 (* x x)))
          (let ((x3 (* x2 x)))
            (lp (+ i 1) (+ x 1.0)
                (+ acc (+ (* 2.0 x3) (+ x2 (let ((h (* x 0.5))) (+ h h))))))))
        acc)))
(write (poly 3))
(newline)

;; let in a loop body, with the value returned through the let
(define (sum-squares n)
  (let lp ((i 0) (acc 0.0))
    (if (< i n)
        (let ((x (* acc 0.0)))
          (let ((y (+ x 1.5)))
            (lp (+ i 1) (+ acc (* y y)))))
        (let ((r (* acc 1.0))) r))))
(write (sum-squares 4))
(newline)

;; signed zeros, infinities and NaNs pass through unchanged
(define (neg-zero n)
  (let lp ((i 0) (x -0.0))
    (if (< i n) (lp (+ i 1) (* x 1.0)) x)))
(define (overflow n)
  (let lp ((i 0) (x -2.0))
    (if (< i n) (lp (+ i 1) (* x (- 0.0 x))) x)))
(define (not-a-number n)
  (let lp ((i 0) (x 2.0))
    (if (< i n)
        (let ((y (* x x)))
          (lp (+ i 1) (+ y (- y (* x x)))))
        x)))
(write (list (eqv? -0.0 (neg-zero 3)) (overflow 20)
             (let ((v (not-a-number 2000))) (= v v))))
(newline)

;; captured by a closure, or used other than in float arithmetic,
;; a flonum stays boxed
(define (make-adders n)
  (let lp ((i 0) (acc 0.0) (res '()))
    (if (< i n)
        (lp (+ i 1) (+ acc 1.0) (cons (lambda (y) (+ acc y)) res))
        (map (lambda (f) (f 10.0)) res))))
(write (make-adders 3))
(newline)
(define (count-below x n)
  (let lp ((i 0) (x x))
    (if (< x 100.0) (lp (+ i 1) (* x 2.0)) i)))
(write (count-below 1.0 0))
(newline)
