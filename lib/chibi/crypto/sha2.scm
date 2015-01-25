;; sha2.scm -- SHA2 digest algorithms
;; Copyright (c) 2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; http://csrc.nist.gov/groups/STM/cavp/documents/shs/sha256-384-512.pdf
;; http://tools.ietf.org/html/rfc6234

;; Note 1: All variables are 32 bit unsigned integers and addition is
;;   calculated modulo 32
;; Note 2: For each round, there is one round constant k[i] and one entry
;;   in the message schedule array w[i], 0 ≤ i ≤ 63
;; Note 3: The compression function uses 8 working variables, a through h
;; Note 4: Big-endian convention is used when expressing the constants in
;;   this pseudocode, and when parsing message block data from bytes to
;;   words, for example, the first word of the input message "abc" after
;;   padding is #x61626380

;; On a 32-bit machine, these will involve bignum computations
;; resulting in poor performance.  Breaking this down into separate
;; 16-bit computations may help.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities.

;; We fake 32-bit arithmetic by ANDing out the low 32 bits.
(define (u32 n)
  (bitwise-and n #xFFFFFFFF))

;; 32-bit addition.
(define (u32+ a b)
  (u32 (+ a b)))

;; Extract bytes 0..3 of a big-endian 32-bit value.
(define (extract-byte n i)
  (bitwise-and #xFF (arithmetic-shift n (* i -8))))

;; Rotate right in 32 bits.
(define (bitwise-rot-u32 n k)
  (bitwise-ior
   (u32 (arithmetic-shift n (- 32 k)))
   (arithmetic-shift n (- k))))

(define hex integer->hex-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first 32 bits of the fractional parts of the square roots of
;; the first 8 primes 2..19:

(define sha-224-inits
  '#(#xc1059ed8 #x367cd507 #x3070dd17 #xf70e5939
     #xffc00b31 #x68581511 #x64f98fa7 #xbefa4fa4))

;; The second 32 bits of the fractional parts of the square roots of
;; the 9th through 16th primes 23..53.

(define sha-256-inits
  '#(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
     #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

;; First 32 bits of the fractional parts of the cube roots of the
;; first 64 primes 2..311:

(define k
  '#(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
     #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
     #xd807aa98 #x12835b01 #x243185be #x550c7dc3
     #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
     #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
     #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
     #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
     #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
     #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
     #x650a7354 #x766a0abb #x81c2c92e #x92722c85
     #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
     #xd192e819 #xd6990624 #xf40e3585 #x106aa070
     #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
     #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
     #x748f82ee #x78a5636f #x84c87814 #x8cc70208
     #x90befffa #xa4506ceb #xbef9a3f7  #xc67178f2))

(define (sha-224-256 src inits full?)
  (let ((in (cond ((string? src) (open-input-bytevector (string->utf8 src)))
                  ((bytevector? src) (open-input-bytevector src))
                  ((input-port? src) src)
                  (else (error "unknown digest source: " src))))
        (buf (make-bytevector 64 0))
        (w (make-vector 64 0)))
    (let chunk ((i 0)
                (pad #x80)
                (h0 (vector-ref inits 0))
                (h1 (vector-ref inits 1))
                (h2 (vector-ref inits 2))
                (h3 (vector-ref inits 3))
                (h4 (vector-ref inits 4))
                (h5 (vector-ref inits 5))
                (h6 (vector-ref inits 6))
                (h7 (vector-ref inits 7)))
      (let* ((n (read-bytevector! buf in))
             (n (if (eof-object? n) 0 n)))
        ;; Maybe pad.
        (cond
         ((< n 64)
          (let ((len (* 8 (+ i n))))
            (bytevector-u8-set! buf n pad)
            (do ((j (+ n 1) (+ j 1))) ((>= j 64))
              (bytevector-u8-set! buf j 0))
            (cond
             ((< n 56)
              (bytevector-u8-set! buf 63 (extract-byte len 0))
              (bytevector-u8-set! buf 62 (extract-byte len 1))
              (bytevector-u8-set! buf 61 (extract-byte len 2))
              (bytevector-u8-set! buf 60 (extract-byte len 3))
              (bytevector-u8-set! buf 59 (extract-byte len 4))
              (bytevector-u8-set! buf 58 (extract-byte len 5))
              (bytevector-u8-set! buf 57 (extract-byte len 6))
              (bytevector-u8-set! buf 56 (extract-byte len 7)))))))
        ;; Copy block i into the buffer.
        (do ((j 0 (+ j 1)))
            ((= j 16))
          (vector-set! w j (bytevector-u32-ref-be buf (* j 4))))
        ;; Extend the first 16 words into the remaining 48 words
        ;; w[16..63] of the message schedule array:
        (do ((j 16 (+ j 1)))
            ((= j 64))
          (let* ((w15 (vector-ref w (- j 15)))
                 (w2 (vector-ref w (- j 2)))
                 (s0 (bitwise-xor (bitwise-rot-u32 w15 7)
                                  (bitwise-rot-u32 w15 18)
                                  (arithmetic-shift w15 -3)))
                 (s1 (bitwise-xor (bitwise-rot-u32 w2 17)
                                  (bitwise-rot-u32 w2 19)
                                  (arithmetic-shift w2 -10))))
            (vector-set! w j (u32 (+ (vector-ref w (- j 16))
                                     s0
                                     (vector-ref w (- j 7))
                                     s1)))))
        ;; Compression function main loop:
        (let lp ((j 0)
                 (a h0) (b h1)
                 (c h2) (d h3)
                 (e h4) (f h5)
                 (g h6) (h h7))
          (cond
           ((= j 64)
            (let ((a (u32+ h0 a)) (b (u32+ h1 b))
                  (c (u32+ h2 c)) (d (u32+ h3 d))
                  (e (u32+ h4 e)) (f (u32+ h5 f))
                  (g (u32+ h6 g)) (h (u32+ h7 h)))
              (cond
               ((< n 64)
                (if (>= n 56)
                    (chunk (+ i n) 0 a b c d e f g h)
                    (string-append
                     (hex a) (hex b) (hex c) (hex d)
                     (hex e) (hex f) (hex g) (if full? (hex h) ""))))
               (else
                (chunk (+ i 64) pad a b c d e f g h)))))
           (else
            ;; Step - compute the two sigmas and recurse on the new a-h.
            (let* ((s1 (bitwise-xor (bitwise-rot-u32 e 6)
                                    (bitwise-rot-u32 e 11)
                                    (bitwise-rot-u32 e 25)))
                   (ch (bitwise-xor (bitwise-and e f)
                                    (bitwise-and (bitwise-not e) g)))
                   (temp1 (u32 (+ h s1 ch (vector-ref k j) (vector-ref w j))))
                   (s0 (bitwise-xor (bitwise-rot-u32 a 2)
                                    (bitwise-rot-u32 a 13)
                                    (bitwise-rot-u32 a 22)))
                   (maj (bitwise-xor (bitwise-and a b)
                                     (bitwise-and a c)
                                     (bitwise-and b c)))
                   (temp2 (u32+ s0 maj)))
              (lp (+ j 1)
                  (u32+ temp1 temp2) a b c
                  (u32+ d temp1) e f g)))))))))

(define (sha-224 src)
  (sha-224-256 src sha-224-inits #f))

(define (sha-256 src)
  (sha-224-256 src sha-256-inits #t))
