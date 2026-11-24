
;; Lets, conditionals and sequences giving flonums are computed
;; unboxed natively, as float arithmetic operands and as the values of
;; unboxed variables.

;; two in one tree, in order
(define (diff n)
  (let lp ((i 0) (acc 0.0))
    (if (< i n)
        (lp (+ i 1) (+ acc (- (let ((a (* acc 2.0))) (+ a 3.0))
                              (let ((b (* acc 0.5))) (+ b 1.0)))))
        acc)))
(write (diff 3))
(newline)

;; conditionals, with fused and general tests
(define (cnds n)
  (let lp ((i 0) (acc 0.0))
    (if (< i n)
        (lp (+ i 1) (+ acc (if (< i 2) 1.0 (if (even? i) (* acc 0.5) 0.25))))
        acc)))
(write (cnds 6))
(newline)

;; a conditional as an unboxed loop param's new value, and a sequence
(define (steps n)
  (let ((count 0))
    (let lp ((i 0) (x 1.0))
      (if (< i n)
          (lp (+ i 1)
              (if (odd? i)
                  (begin (set! count (+ count 1)) (* x 3.0))
                  (let ((h (* x 0.5))) (if (> i 2) (+ h h) h))))
          (list count x)))))
(write (steps 6))
(newline)

;; the slow path when another operand isn't a flonum
(define (mix y n)
  (let lp ((i 0) (acc 1.0))
    (if (< i n)
        (lp (+ i 1) (+ acc (* (let ((x (* acc 2.0))) x) y)))
        acc)))
(write (list (mix 0 3) (mix 1/2 2) (mix 2 2)))
(newline)
(define (try thunk)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (e) (k 'error))
      thunk))))
(write (try (lambda () (mix 'x 2))))
(newline)
