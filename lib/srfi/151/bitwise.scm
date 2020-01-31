;; bitwise.scm -- high-level bitwise functions
;; Copyright (c) 2009-2017 Alex Shinn.  All rights reserved.
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

(define (any-bit-set? test-bits i)
  (not (zero? (bitwise-and test-bits i))))
(define (every-bit-set? test-bits i)
  (= test-bits (bitwise-and test-bits i)))

(define (first-set-bit i)
  (if (zero? i)
      -1
      (- (integer-length (- i (bit-and i (- i 1)))) 1)))

(define (mask len)
  (- (arithmetic-shift 1 len) 1))

(define (range start end)
  (arithmetic-shift (mask (- end start)) start))

(define (bitwise-if mask m n)
  (bit-ior (bit-and mask m)
           (bit-and (bitwise-not mask) n)))

(define (bit-field n start end)
  (bit-and (arithmetic-shift n (- start)) (mask (- end start))))

(define (bit-field-any? n start end)
  (not (zero? (bit-and (arithmetic-shift n (- start))
                       (mask (- end start))))))

(define (bit-field-every? n start end)
  (let ((lo (mask (- end start))))
    (= (bit-and lo (arithmetic-shift n (- start)))
       lo)))

(define (copy-bit index i boolean)
  (bit-field-replace i (if boolean 1 0) index (+ index 1)))

(define (bit-swap i1 i2 i)
  (let ((b1 (bit-set? i1 i))
        (b2 (bit-set? i2 i)))
    (copy-bit i2 (copy-bit i1 i b2) b1)))

(define (bit-field-clear n start end)
  (bit-field-replace n 0 start end))

(define (bit-field-set n start end)
  (bit-ior n (range start end)))

(define (bit-field-replace dst src start end)
  (bit-field-replace-same dst (arithmetic-shift src start) start end))

(define (bit-field-replace-same dst src start end)
  (bitwise-if (range start end) src dst))

(define (bit-field-rotate n count start end)
  (let* ((width (- end start))
         (count (modulo count width))
         (mask (bitwise-not (arithmetic-shift -1 width)))
         (n^ (bitwise-and mask (arithmetic-shift n (- start)))))
    (bit-ior (arithmetic-shift
              (bit-ior (bit-and mask (arithmetic-shift n^ count))
                       (arithmetic-shift n^ (- count width)))
              start)
             (bit-and (bitwise-not (arithmetic-shift mask start)) n))))

(define (bit-reverse n len)
  (let lp ((n n) (i 1) (res 0))
    (if (> i len)
        res
        (lp (arithmetic-shift n -1)
            (+ i 1)
            (bit-ior (arithmetic-shift res 1)
                     (bit-and n 1))))))

(define (bit-field-reverse i start end)
  (bitwise-if (range start end)
              (arithmetic-shift (bit-reverse (bit-field i start end)
                                             (- end start))
                                start)
              i))

(define (vector->bits vec)
  (let ((len (vector-length vec)))
    (let lp ((i 0) (exp 1) (res 0))
      (cond
       ((= i len) res)
       ((vector-ref vec i) (lp (+ i 1) (* exp 2) (+ res exp)))
       (else (lp (+ i 1) (* exp 2) res))))))

(define (bits->vector n . o)
  (let* ((len (if (pair? o) (car o) (integer-length n)))
         (res (make-vector len #f)))
    (let lp ((n n) (i 0))
      (cond
       ((>= i len)
        res)
       (else
        (if (odd? n)
            (vector-set! res i #t))
        (lp (arithmetic-shift n -1) (+ i 1)))))))

(define (list->bits ls)
  (vector->bits (list->vector ls)))

(define (bits->list n . o)
  (vector->list (apply bits->vector n o)))

(define (bits . o) (list->bits o))

(define (bitwise-fold kons knil i)
  (let lp ((i i) (acc knil))
    (if (zero? i)
        acc
        (lp (arithmetic-shift i -1) (kons (odd? i) acc)))))

(define (bitwise-for-each proc i)
  (bitwise-fold (lambda (b acc) (proc b)) #f i))

(define (bitwise-unfold stop? mapper successor seed)
  (let lp ((state seed) (exp 1) (i 0))
    (if (stop? state)
        i
        (lp (successor state)
            (* exp 2)
            (if (mapper state) (+ i exp) i)))))

(define make-bitwise-generator
  (let ((eof (read-char (open-input-string ""))))
    (lambda (i)
      (lambda ()
        (let ((res (odd? i)))
          (set! i (arithmetic-shift i -1))
          res)))))
