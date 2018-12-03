;;; Copyright (c) 2004-2018 by Alex Shinn.

;; Adapted from SRFI-56.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax

(define-syntax combine
  (syntax-rules ()
    ((combine) 0)
    ((combine b1) b1)
    ((combine b1 b2 b3 ...)
     (combine (+ (arithmetic-shift b1 8) b2) b3 ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reading floating point numbers

;; Inspired by Oleg's implementation from
;;   http://okmij.org/ftp/Scheme/reading-IEEE-floats.txt
;; but removes mutations and magic numbers and allows for manually
;; specifying the endianness.
;;
;; See also
;;   http://www.cs.auckland.ac.nz/~jham1/07.211/floats.html
;; and
;;   http://babbage.cs.qc.edu/courses/cs341/IEEE-754references.html
;; as references to IEEE 754.

(define (bytevector-ieee-single-native-ref bytevector k)
    (define (mantissa expn b2 b3 b4)
      (case expn   ; recognize special literal exponents
        ((255)
         ;;(if (zero? (combine b2 b3 b4)) +/0. 0/0.) ; XXXX for SRFI-70
         #f)
        ((0)       ; denormalized
         (inexact (* (expt 2.0 (- 1 (+ 127 23))) (combine b2 b3 b4))))
        (else
         (inexact
          (* (expt 2.0 (- expn (+ 127 23)))
             (combine (+ b2 128) b3 b4)))))) ; hidden bit
    (define (exponent b1 b2 b3 b4)
      (if (> b2 127)  ; 1st bit of b2 is low bit of expn
        (mantissa (+ (* 2 b1) 1) (- b2 128) b3 b4)
        (mantissa (* 2 b1) b2 b3 b4)))
    (define (sign b1 b2 b3 b4)
      (if (> b1 127)  ; 1st bit of b1 is sign
        (cond ((exponent (- b1 128) b2 b3 b4) => -) (else #f))
        (exponent b1 b2 b3 b4)))
    (let* ((b1 (bytevector-u8-ref bytevector (+ k 0)))
           (b2 (bytevector-u8-ref bytevector (+ k 1)))
           (b3 (bytevector-u8-ref bytevector (+ k 2)))
           (b4 (bytevector-u8-ref bytevector (+ k 3))))
      (if (eq? (native-endianness) 'big)
          (sign b1 b2 b3 b4)
          (sign b4 b3 b2 b1))))

(define (bytevector-ieee-double-native-ref bytevector k)
    (define (mantissa expn b2 b3 b4 b5 b6 b7 b8)
      (case expn   ; recognize special literal exponents
        ((255) #f) ; won't handle NaN and +/- Inf
        ((0)       ; denormalized
         (inexact (* (expt 2.0 (- 1 (+ 1023 52)))
                            (combine b2 b3 b4 b5 b6 b7 b8))))
        (else
         (inexact
          (* (expt 2.0 (- expn (+ 1023 52)))
             (combine (+ b2 16) b3 b4 b5 b6 b7 b8)))))) ; hidden bit
    (define (exponent b1 b2 b3 b4 b5 b6 b7 b8)
      (mantissa (bitwise-ior (arithmetic-shift b1 4)        ; 7 bits
                             (arithmetic-shift b2 -4))      ; + 4 bits
                (bitwise-and b2 #b1111)
                b3 b4 b5 b6 b7 b8))
    (define (sign b1 b2 b3 b4 b5 b6 b7 b8)
      (if (> b1 127)  ; 1st bit of b1 is sign
        (cond ((exponent (- b1 128) b2 b3 b4 b5 b6 b7 b8) => -)
              (else #f))
        (exponent b1 b2 b3 b4 b5 b6 b7 b8)))
    (let* ((b1 (bytevector-u8-ref bytevector (+ k 0)))
           (b2 (bytevector-u8-ref bytevector (+ k 1)))
           (b3 (bytevector-u8-ref bytevector (+ k 2)))
           (b4 (bytevector-u8-ref bytevector (+ k 3)))
           (b5 (bytevector-u8-ref bytevector (+ k 4)))
           (b6 (bytevector-u8-ref bytevector (+ k 5)))
           (b7 (bytevector-u8-ref bytevector (+ k 6)))
           (b8 (bytevector-u8-ref bytevector (+ k 7))))
      (if (eq? (native-endianness) 'big)
          (sign b1 b2 b3 b4 b5 b6 b7 b8)
          (sign b8 b7 b6 b5 b4 b3 b2 b1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writing floating point numbers

;; Underflow rounds down to zero as in IEEE-754, and overflow gets
;; written as +/- Infinity.

;; Break a real number down to a normalized mantissa and exponent.
;; Default base=2, mant-size=23 (52), exp-size=8 (11) for IEEE singles
;; (doubles).
;;
;; Note: This should never be used in practice, since it can be
;; implemented much faster in C.  See decode-float in ChezScheme or
;; Gauche.
(define (call-with-mantissa&exponent num base mant-size exp-size proc)
  (cond
   ((negative? num)
    (call-with-mantissa&exponent (- num) base mant-size exp-size proc))
    ((zero? num) (proc 0 0))
    (else
     (let* ((bot (expt base mant-size))
            (top (* base bot)))
       (let loop ((n (inexact num)) (e 0))
         (cond
          ((>= n top)
           (loop (/ n base) (+ e 1)))
          ((< n bot)
           (loop (* n base) (- e 1)))
          (else
           (proc (exact (round n)) e))))))))

(define (bytevector-ieee-single-native-set! bytevector k num)
    (define (bytes)
      (call-with-mantissa&exponent num 2 23 8
        (lambda (f e)
          (let ((e0 (+ e 127 23)))
            (cond
              ((negative? e0)
               (let* ((f1 (exact (round (* f (expt 2 (- e0 1))))))
                      (b2 (bit-field f1 16 24))        ; mant:16-23
                      (b3 (bit-field f1 8 16))         ; mant:8-15
                      (b4 (bit-field f1 0 8)))         ; mant:0-7
                 (list (if (negative? num) 128 0) b2 b3 b4)))
              ((> e0 255) ; XXXX here we just write infinity
               (list (if (negative? num) 255 127) 128 0 0))
              (else
               (let* ((b0 (arithmetic-shift e0 -1))
                      (b1 (if (negative? num) (+ b0 128) b0)) ; sign + exp:1-7
                      (b2 (bitwise-ior
                           (if (odd? e0) 128 0)               ; exp:0
                           (bit-field f 16 23)))              ;   + mant:16-23
                      (b3 (bit-field f 8 16))                 ; mant:8-15
                      (b4 (bit-field f 0 8)))                 ; mant:0-7
                 (list b1 b2 b3 b4))))))))
      (let ((result (cond
                      ((zero? num) '(0 0 0 0))
                      ((eq? (native-endianness) 'big) (bytes))
                      (else (reverse (bytes))))))
       (bytevector-u8-set! bytevector (+ k 0) (list-ref result 0))
       (bytevector-u8-set! bytevector (+ k 1) (list-ref result 1))
       (bytevector-u8-set! bytevector (+ k 2) (list-ref result 2))
       (bytevector-u8-set! bytevector (+ k 3) (list-ref result 3))))

(define (bytevector-ieee-double-native-set! bytevector k num)
    (define (bytes)
      (call-with-mantissa&exponent num 2 52 11
        (lambda (f e)
          (let ((e0 (+ e 1023 52)))
            (cond
              ((negative? e0)
               (let* ((f1 (exact (round (* f (expt 2 (- e0 1))))))
                      (b2 (bit-field f1 48 52))
                      (b3 (bit-field f1 40 48))
                      (b4 (bit-field f1 32 40))
                      (b5 (bit-field f1 24 32))
                      (b6 (bit-field f1 16 24))
                      (b7 (bit-field f1 8 16))
                      (b8 (bit-field f1 0 8)))
                 (list (if (negative? num) 128 0) b2 b3 b4 b5 b6 b7 b8)))
              ((> e0 4095) ; infinity
               (list (if (negative? num) 255 127) 224 0 0 0 0 0 0))
              (else
               (let* ((b0 (bit-field e0 4 11))
                      (b1 (if (negative? num) (+ b0 128) b0))
                      (b2 (bitwise-ior (arithmetic-shift
                                        (bit-field e0 0 4)
                                        4)
                                       (bit-field f 48 52)))
                      (b3 (bit-field f 40 48))
                      (b4 (bit-field f 32 40))
                      (b5 (bit-field f 24 32))
                      (b6 (bit-field f 16 24))
                      (b7 (bit-field f 8 16))
                      (b8 (bit-field f 0 8)))
                 (list b1 b2 b3 b4 b5 b6 b7 b8))))))))
      (let ((result (cond
                      ((zero? num) '(0 0 0 0 0 0 0 0))
                      ((eq? (native-endianness) 'big) (bytes))
                      (else (reverse (bytes))))))
       (bytevector-u8-set! bytevector (+ k 0) (list-ref result 0))
       (bytevector-u8-set! bytevector (+ k 1) (list-ref result 1))
       (bytevector-u8-set! bytevector (+ k 2) (list-ref result 2))
       (bytevector-u8-set! bytevector (+ k 3) (list-ref result 3))
       (bytevector-u8-set! bytevector (+ k 4) (list-ref result 4))
       (bytevector-u8-set! bytevector (+ k 5) (list-ref result 5))
       (bytevector-u8-set! bytevector (+ k 6) (list-ref result 6))
       (bytevector-u8-set! bytevector (+ k 7) (list-ref result 7))))
