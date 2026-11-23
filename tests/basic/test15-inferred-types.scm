
;; Checks the native backend elides based on inferred types must not
;; drop: each case goes wrong if a non-fixnum or non-vector is assumed.

(define max-fx 4611686018427387903)
(define min-fx (- -1 max-fx))

(define (try thunk)
  (call-with-current-continuation
   (lambda (k) (with-exception-handler (lambda (e) (k 'error)) thunk))))

;; quotient and remainder of bignums are bignums
(define (quotients big)
  (define (g x y) (if (> x 0) (+ x y) (g (+ x 1) y)))
  (list (g (quotient big 1) (quotient big 1))
        (g (remainder big (* big 2)) 1)))
(write (quotients (expt 10 20)))
(newline)

;; a non-strict bound at the fixnum limit doesn't prevent overflow
(define (up-to-limit)
  (let lp ((i (- max-fx 2)))
    (if (<= i max-fx) (lp (+ i 1)) (list i (+ i i)))))
(write (up-to-limit))
(newline)

(define (down-to-limit)
  (let lp ((i (+ min-fx 2)))
    (if (>= i min-fx) (lp (- i 1)) (list i (- i 1)))))
(write (down-to-limit))
(newline)

;; nor does a strict bound in the wrong direction
(define (wrong-way)
  (let lp ((i (+ min-fx 2)) (n 0))
    (if (< i 0) (if (< n 5) (lp (- i 1) (+ n 1)) (- i 1)) 'done)))
(write (wrong-way))
(newline)

;; the sum of two near-limit fixnums isn't a fixnum
(define (near-limit)
  (define (g x) (+ (+ x 1) (+ x 1)))
  (g max-fx))
(write (near-limit))
(newline)

;; small counters stay fixnums, with an = exit or a strict bound
(define (count-to n)
  (let lp ((i 0) (acc 0))
    (if (= i n) acc (lp (+ i 1) (+ acc i i)))))
(write (count-to 1000))
(newline)

(define (sum-vector v)
  (let lp ((i 0) (acc 0))
    (if (< i (vector-length v))
        (lp (+ i 1) (+ acc (vector-ref v i)))
        acc)))
(write (sum-vector (vector 1 2 3 (expt 10 20))))
(newline)

;; containers passed through loops keep their type, but only if
;; every caller passes that type
(define (typed-containers)
  (define (vlen v) (vector-length v))
  (define (copy v i res)
    (if (< i (vector-length v)) (copy v (+ i 1) (cons (vector-ref v i) res)) res))
  (define (chars s i res)
    (if (< i (string-length s)) (chars s (+ i 1) (cons (string-ref s i) res)) res))
  (define (bytes b i)
    (if (< i (bytevector-length b)) (begin (bytevector-u8-set! b i (* i 2)) (bytes b (+ i 1))) b))
  (list (vlen (make-vector 3 0)) (vlen (make-vector 2 0))
        (copy (vector 'a 'b 'c) 0 '())
        (chars "xyz" 0 '())
        (bytes (make-bytevector 4 0) 0)))
(write (typed-containers))
(newline)

(define (mixed-containers)
  (define (vlen v) (vector-length v))
  (list (vlen (make-vector 3 0)) (try (lambda () (vlen "abc")))))
(write (mixed-containers))
(newline)
