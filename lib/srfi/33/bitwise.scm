;; bitwise.scm -- high-level bitwise functions
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (bitwise-not i) (- -1 i))

(define (bitwise-complement f) (lambda args (bitwise-not (apply f args))))

(define (make-nary proc2 default)
  (lambda args
    (if (null? args)
        default
        (let lp ((i (car args)) (ls (cdr args)))
          (if (null? ls)
              i
              (lp (proc2 i (car ls)) (cdr ls)))))))

(define bitwise-and  (make-nary bit-and  -1))
(define bitwise-ior  (make-nary bit-ior   0))
(define bitwise-xor  (make-nary bit-xor   0))

(define bitwise-eqv  (bitwise-complement (make-nary bit-xor -1)))
(define bitwise-nand (bitwise-complement (make-nary bit-and  0)))
(define bitwise-nor  (bitwise-complement (make-nary bit-ior -1)))

(define (bitwise-andc1 i j) (bit-and (bitwise-not i) j))
(define (bitwise-andc2 i j) (bit-and i (bitwise-not j)))
(define (bitwise-orc1 i j)  (bit-ior (bitwise-not i) j))
(define (bitwise-orc2 i j)  (bit-ior i (bitwise-not j)))

(define (any-bits-set? test-bits i)
  (not (zero? (bitwise-and test-bits i))))
(define (all-bits-set? test-bits i)
  (= test-bits (bitwise-and test-bits i)))

(define (first-set-bit i)
  (if (zero? i)
      -1
      (integer-length (- i (bit-and i (- i 1))))))

(define (mask len) (- (arithmetic-shift 1 len) 1))

(define (bitwise-merge mask n m)
  (bit-ior (bit-and mask n) (bit-and (bitwise-not mask) m)))

(define (extract-bit-field size position n)
  (bit-and (arithmetic-shift n (- position)) (mask size)))

(define (test-bit-field? size position n)
  (not (zero? (bit-and (arithmetic-shift n (- position)) (mask size)))))

(define (replace-bit-field size position newfield n)
  (bit-ior (bit-and n (bitwise-not (arithmetic-shift (mask size) position)))
           (arithmetic-shift newfield position)))

(define (clear-bit-field size position n)
  (replace-bit-field size position 0 n))

(define (copy-bit-field size position from to)
  (bitwise-merge (arithmetic-shift (mask size) position) to from))

