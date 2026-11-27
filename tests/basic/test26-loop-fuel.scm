;; The core not is inlined natively, and self tail jumps use fuel
;; unless the loop provably terminates.

;; not as a value, in tests and in loop args
(define (alternate n)
  (let lp ((i 0) (flag #t) (k 0))
    (if (= i n) (list k flag) (lp (+ i 1) (not flag) (if flag (+ k 1) k)))))
(write (list (not #f) (not 0) (not '()) (alternate 5)))
(newline)

;; bounded loops: up and down to a fixnum bound, and down lists
(define (vsum v)
  (let lp ((i 0) (s 0))
    (if (< i (vector-length v)) (lp (+ i 1) (+ s (vector-ref v i))) s)))
(define (vsum-down v)
  (let lp ((i (- (vector-length v) 1)) (s 0))
    (if (not (< i 0)) (lp (- i 1) (+ s (vector-ref v i))) s)))
(define (vcount v x)
  (let lp ((i 0) (n (vector-length v)) (k 0))
    (cond ((>= i n) k)
          ((eq? x (vector-ref v i)) (lp (+ i 1) n (+ k 1)))
          (else (lp (+ i 1) n k)))))
(define (rev ls)
  (let lp ((ls ls) (res '()))
    (if (pair? ls) (lp (cdr ls) (cons (car ls) res)) res)))
(define (sum ls)
  (let lp ((ls ls) (acc 0))
    (if (null? ls) acc (lp (cdr ls) (+ acc (car ls))))))
(write (list (vsum #(1 2 3)) (vsum-down #(1 2 3)) (vcount #(a b a) 'a)
             (rev '(1 2 3)) (sum '(1 2 3))))
(newline)

;; a leaf loop not known to terminate still lets other threads run
(import (srfi 18))
(define done #f)
(define (spin)
  (let lp ((i 0)) (if (not done) (lp (+ i 1)) 'done)))
(define t (make-thread (lambda () (set! done #t))))
(thread-start! t)
(write (spin))
(newline)

;; a not of our own isn't the core not
(define (not x) 'mine)
(define (alt2 n)
  (let lp ((i 0) (f #t)) (if (= i n) f (lp (+ i 1) (not f)))))
(write (list (not #f) (alt2 2) (if (not #t) 'pass 'fail)))
(newline)
