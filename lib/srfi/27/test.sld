
(define-library (srfi 27 test)
  (export run-tests)
  (import (scheme base)
          (scheme flonum)
          (scheme inexact)
          (scheme vector)
          (srfi 27)
          (chibi test))
  (begin
    (define (random-histogram bound n . o)
      (let* ((hist (make-vector (if (pair? o) (car o) (min 10 bound)) 0))
             (rs (make-random-source))
             (rand (random-source-make-integers rs)))
        (random-source-pseudo-randomize! rs 23 42)
        (do ((i 0 (+ i 1)))
            ((= i n) hist)
          (let* ((a (rand bound))
                 (b (quotient (* a (vector-length hist)) bound)))
            (vector-set! hist b (+ 1 (vector-ref hist b)))))))
    (define (loggamma x)
      (call-with-values (lambda () (flloggamma x))
        (lambda (res sign) res)))
    ;; continued fraction expansion, borrowed from (chibi math stats)
    (define (lower-incomplete-gamma s z)
      (let lp ((k 1) (x 1.0) (sum 1.0))
        (if (or (= k 1000) (< (/ x sum) 1e-14))
            (exp (+ (* s (log z))
                    (log sum)
                    (- z)
                    (- (loggamma (+ s 1.)))))
            (let* ((x2 (* x (/ z (+ s k))))
                   (sum2 (+ sum x2)))
              (lp (+ k 1) x2 sum2)))))
    (define (chi^2-cdf X^2 df)
      (min 1 (lower-incomplete-gamma (/ df 2) (/ X^2 2))))
    (define (histogram-uniform? hist . o)
      ;; ultra-conservative alpha to avoid test failures on false positives
      (let* ((alpha (if (pair? o) (car o) 1e-5))
             (n (vector-fold + 0 hist))
             (len (vector-length hist))
             (expected (/ n (inexact len)))
             (X^2 (vector-fold
                   (lambda (X^2 observed)
                     (+ X^2 (/ (square (- observed expected)) expected)))
                   0
                   hist))
             (p (- 1.0 (chi^2-cdf X^2 (- len 1)))))
        ;;(write `(hist: ,hist X^2: ,X^2 p: ,p)) (newline)
        (> p alpha)))
    (define (run-tests)
      (define (test-random rand n)
        (test-assert (<= 0 (rand n) (- n 1))))
      (test-begin "srfi-27: random")

      ;; sanity checks
      (test 0 (random-integer 1))
      (test-assert (<= 0 (random-integer 2) 1))
      (test-error (random-integer 0))
      (test-error (random-integer -1))

      (let ((rs (make-random-source)))
        ;; chosen by fair dice roll.  guaranteed to be random
        (random-source-pseudo-randomize! rs 4 4)
        (let ((rand (random-source-make-integers rs)))
          (do ((k 0 (+ k 5))
               (n 1 (* n 2)))
              ((> k 1024))
            (test-random rand n))
          (let* ((state (random-source-state-ref rs))
                 (x (rand 1000000)))
            ;; the next int won't be the same, but it will be after
            ;; resetting the state
            (test-not (= x (rand 1000000)))
            (random-source-state-set! rs state)
            ;; (test x (rand 1000000))  ;; actually impl defined
            )))

      ;; Distribution Checks.
      ;; Since we fall back on the libc rand, we can't test the exact
      ;; result even for a given seed, so we run some conservative
      ;; statistical tests.
      (test-assert
          (histogram-uniform? (random-histogram 2 1000)))      ; coin
      (test-assert
          (histogram-uniform? (random-histogram 6 10000)))     ; die
      (test-assert
          (histogram-uniform? (random-histogram 27 10000 27))) ; small prime
      ;; boundaries
      (test-assert
          (histogram-uniform? (random-histogram (expt 2 31) 10000)))
      (test-assert
          (histogram-uniform? (random-histogram (expt 2 32) 10000)))
      (test-assert
          (histogram-uniform? (random-histogram (- (expt 2 62) 1) 10000)))
      ;; bignums
      (test-assert
          (histogram-uniform? (random-histogram (expt 2 62) 10000)))
      (test-assert
          (histogram-uniform? (random-histogram (expt 2 63) 10000)))
      (test-assert
          (histogram-uniform? (random-histogram (expt 2 63) 10000 100)))
      (test-assert
          (histogram-uniform? (random-histogram (- (expt 2 64) 1) 10000)))
      (test-assert
          (histogram-uniform? (random-histogram (expt 2 64) 10000)))
      (test-assert
          (histogram-uniform? (random-histogram (+ (expt 2 64) 1) 10000)))
      (test-assert
          (histogram-uniform? (random-histogram (expt 2 65) 10000)))
      (test-assert
          (histogram-uniform? (random-histogram (expt 2 164) 10000)))

      (test-end))))
