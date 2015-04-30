;; md5.scm -- pure R7RS md5 implementation (originally from hato)
;; Copyright (c) 2009-2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Break computations down into 16-bit words to keep everything in
;; fixnum even on 32-bit machines.

;; All values are in little-endian.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities.

(define (extract-byte n i)
  (bitwise-and #xFF (arithmetic-shift n (* i -8))))

;; integer->hex-string is big-endian, so we adjust here
(define (hex-byte n)
  (if (< n 16)
      (string-append "0" (number->string n 16))
      (number->string n 16)))

(define (hex n)
  (string-append (hex-byte (remainder n 256))
                 (hex-byte (quotient n 256))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3. MD5 Algorithm Description

;; We begin by supposing that we have a b-bit message as input, and that
;; we wish to find its message digest. Here b is an arbitrary
;; nonnegative integer; b may be zero, it need not be a multiple of
;; eight, and it may be arbitrarily large. We imagine the bits of the
;; message written down as follows:

;;           m_0 m_1 ... m_{b-1}

;; The following five steps are performed to compute the message digest
;; of the message.

;; 3.1 Step 1. Append Padding Bits

;; The message is "padded" (extended) so that its length (in bits) is
;; congruent to 448, modulo 512. That is, the message is extended so
;; that it is just 64 bits shy of being a multiple of 512 bits long.
;; Padding is always performed, even if the length of the message is
;; already congruent to 448, modulo 512.

;; Padding is performed as follows: a single "1" bit is appended to the
;; message, and then "0" bits are appended so that the length in bits of
;; the padded message becomes congruent to 448, modulo 512. In all, at
;; least one bit and at most 512 bits are appended.

;; 3.2 Step 2. Append Length

;; A 64-bit representation of b (the length of the message before the
;; padding bits were added) is appended to the result of the previous
;; step. In the unlikely event that b is greater than 2^64, then only
;; the low-order 64 bits of b are used. (These bits are appended as two
;; 32-bit words and appended low-order word first in accordance with the
;; previous conventions.)

;; At this point the resulting message (after padding with bits and with
;; b) has a length that is an exact multiple of 512 bits. Equivalently,
;; this message has a length that is an exact multiple of 16 (32-bit)
;; words. Let M[0 ... N-1] denote the words of the resulting message,
;; where N is a multiple of 16.

;; 3.3 Step 3. Initialize MD Buffer

;; A four-word buffer (A,B,C,D) is used to compute the message digest.
;; Here each of A, B, C, D is a 32-bit register. These registers are
;; initialized to the following values in hexadecimal, low-order bytes
;; first):

;;           word A: 01 23 45 67
;;           word B: 89 ab cd ef
;;           word C: fe dc ba 98
;;           word D: 76 54 32 10

;; 3.4 Step 4. Process Message in 16-Word Blocks

;; We first define four auxiliary functions that each take as input
;; three 32-bit words and produce as output one 32-bit word.

;;           F(X,Y,Z) = XY v not(X) Z
;;           G(X,Y,Z) = XZ v Y not(Z)
;;           H(X,Y,Z) = X xor Y xor Z
;;           I(X,Y,Z) = Y xor (X v not(Z))

;; In each bit position F acts as a conditional: if X then Y else Z.
;; The function F could have been defined using + instead of v since XY
;; and not(X)Z will never have 1's in the same bit position.) It is
;; interesting to note that if the bits of X, Y, and Z are independent
;; and unbiased, the each bit of F(X,Y,Z) will be independent and
;; unbiased.

;; The functions G, H, and I are similar to the function F, in that they
;; act in "bitwise parallel" to produce their output from the bits of X,
;; Y, and Z, in such a manner that if the corresponding bits of X, Y,
;; and Z are independent and unbiased, then each bit of G(X,Y,Z),
;; H(X,Y,Z), and I(X,Y,Z) will be independent and unbiased. Note that
;; the function H is the bit-wise "xor" or "parity" function of its
;; inputs.

;; This step uses a 64-element table T[1 ... 64] constructed from the
;; sine function. Let T[i] denote the i-th element of the table, which
;; is equal to the integer part of 4294967296 times abs(sin(i)), where i
;; is in radians. The elements of the table are given in the appendix.

;; (define T
;;   (do ((i 64 (- i 1))
;;        (ls '()
;;            (cons (u32 (exact (truncate (* 4294967296 (abs (sin i))))))
;;                  ls)))
;;       ((< i 0) (list->vector ls))))

(define T
  '#(0     0       #xd76a #xa478 #xe8c7 #xb756 #x2420 #x70db #xc1bd #xceee
     #xf57c #x0faf #x4787 #xc62a #xa830 #x4613 #xfd46 #x9501 #x6980 #x98d8
     #x8b44 #xf7af #xffff #x5bb1 #x895c #xd7be #x6b90 #x1122 #xfd98 #x7193
     #xa679 #x438e #x49b4 #x0821 #xf61e #x2562 #xc040 #xb340 #x265e #x5a51
     #xe9b6 #xc7aa #xd62f #x105d #x0244 #x1453 #xd8a1 #xe681 #xe7d3 #xfbc8
     #x21e1 #xcde6 #xc337 #x07d6 #xf4d5 #x0d87 #x455a #x14ed #xa9e3 #xe905
     #xfcef #xa3f8 #x676f #x02d9 #x8d2a #x4c8a #xfffa #x3942 #x8771 #xf681
     #x6d9d #x6122 #xfde5 #x380c #xa4be #xea44 #x4bde #xcfa9 #xf6bb #x4b60
     #xbebf #xbc70 #x289b #x7ec6 #xeaa1 #x27fa #xd4ef #x3085 #x0488 #x1d05
     #xd9d4 #xd039 #xe6db #x99e5 #x1fa2 #x7cf8 #xc4ac #x5665 #xf429 #x2244
     #x432a #xff97 #xab94 #x23a7 #xfc93 #xa039 #x655b #x59c3 #x8f0c #xcc92
     #xffef #xf47d #x8584 #x5dd1 #x6fa8 #x7e4f #xfe2c #xe6e0 #xa301 #x4314
     #x4e08 #x11a1 #xf753 #x7e82 #xbd3a #xf235 #x2ad7 #xd2bb #xeb86 #xd391))

;;> Returns the md5 checksum of \var{src} as a lowercase hex-string.
;;> \var{src} can be any of a string (interpreted as utf8), a
;;> bytevector, or a binary input port.

(define (md5 src)
  (let ((in (cond ((string? src) (open-input-bytevector (string->utf8 src)))
                  ((bytevector? src) (open-input-bytevector src))
                  ((input-port? src) src)
                  (else (error "unknown digest source: " src))))
        ;; 3.3 Step 3. Initialize MD Buffer
        (buf (make-bytevector 64 0))
        (vec (make-vector 32))
        (A1 #x6745) (A0 #x2301)
        (B1 #xefcd) (B0 #xab89)
        (C1 #x98ba) (C0 #xdcfe)
        (D1 #x1032) (D0 #x5476))
    ;; Process each 16-word block.
    (let lp ((i 0)
             (pad #x80))
      (let* ((n (read-bytevector! buf in))
             (n (if (eof-object? n) 0 n)))
        (cond
         ((< n 64)
          (let ((len (* 8 (+ i n))))
            ;; 3.1 Step 1. Append Padding Bits
            (bytevector-u8-set! buf n pad)
            (do ((j (+ n 1) (+ j 1))) ((>= j 64))
              (bytevector-u8-set! buf j 0))
            ;; 3.2 Step 2. Append Length
            (cond
             ((< n 56)
              (bytevector-u8-set! buf 56 (extract-byte len 0))
              (bytevector-u8-set! buf 57 (extract-byte len 1))
              (bytevector-u8-set! buf 58 (extract-byte len 2))
              (bytevector-u8-set! buf 59 (extract-byte len 3))
              (bytevector-u8-set! buf 60 (extract-byte len 4))
              (bytevector-u8-set! buf 61 (extract-byte len 5))
              (bytevector-u8-set! buf 62 (extract-byte len 6))
              (bytevector-u8-set! buf 63 (extract-byte len 7)))))))
        ;; 3.4 Step 4. Process Message in 16-Word Blocks
        ;;
        ;; Copy block i into X.
        (do ((j 0 (+ j 1)))
            ((= j 16))
          (vector-set! vec (* j 2) (bytevector-u16-ref-le buf (* j 4)))
          (vector-set! vec
                       (+ (* j 2) 1)
                       (bytevector-u16-ref-le buf (+ (* j 4) 2))))
        ;; Save A as AA, B as BB, C as CC, and D as DD.
        (let ((AA0 A0) (AA1 A1)
              (BB0 B0) (BB1 B1)
              (CC0 C0) (CC1 C1)
              (DD0 D0) (DD1 D1)
              (T1 0)   (T0 0))
          (letrec-syntax
              ((add
                (syntax-rules ()
                  ((add d1 d0 a1 a0 b1 b0)
                   (begin
                     (set! d0 (+ a0 b0))
                     (set! d1 (bitwise-and
                               (+ a1 b1 (arithmetic-shift d0 -16))
                               #xFFFF))
                     (set! d0 (bitwise-and d0 #xFFFF))))))
               (rot
                (syntax-rules ()
                  ((rot d1 d0 a1 a0 s)
                   (let ((tmp a1))
                     (set! d1 (bitwise-and
                               (bitwise-ior (arithmetic-shift a1 s)
                                            (arithmetic-shift a1 (- s 32))
                                            (arithmetic-shift a0 (- s 16)))
                               #xFFFF))
                     (set! d0 (bitwise-and
                               (bitwise-ior (arithmetic-shift a0 s)
                                            (arithmetic-shift a0 (- s 32))
                                            (arithmetic-shift tmp (- s 16)))
                               #xFFFF))))))
               (bit-not
                (syntax-rules ()
                  ((bit-not a) (- (expt 2 16) a 1))))
               (FF
                (syntax-rules ()
                  ((FF d1 d0 x1 x0 y1 y0 z1 z0)
                   (begin
                     (set! d1 (bitwise-ior (bitwise-and x1 y1)
                                           (bitwise-and (bit-not x1) z1)))
                     (set! d0 (bitwise-ior (bitwise-and x0 y0)
                                           (bitwise-and (bit-not x0) z0)))
                     ))))
               (GG
                (syntax-rules ()
                  ((GG d1 d0 x1 x0 y1 y0 z1 z0)
                   (begin
                     (set! d1 (bitwise-ior (bitwise-and x1 z1)
                                           (bitwise-and y1 (bit-not z1))))
                     (set! d0 (bitwise-ior (bitwise-and x0 z0)
                                           (bitwise-and y0 (bit-not z0))))
                     ))))
               (HH
                (syntax-rules ()
                  ((HH d1 d0 x1 x0 y1 y0 z1 z0)
                   (begin (set! d1 (bitwise-xor x1 y1 z1))
                          (set! d0 (bitwise-xor x0 y0 z0))))))
               (II
                (syntax-rules ()
                  ((II d1 d0 x1 x0 y1 y0 z1 z0)
                   (begin
                     (set! d1 (bitwise-xor y1 (bitwise-ior x1 (bit-not z1))))
                     (set! d0 (bitwise-xor y0 (bitwise-ior x0 (bit-not z0))))
                     ))))
               (R
                (syntax-rules ()
                  ((R op T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i)
                   (begin
                     (op T1 T0 b1 b0 c1 c0 d1 d0)
                     (add T1 T0 T1 T0
                          (vector-ref vec (+ (* k 2) 1))
                          (vector-ref vec (* k 2)))
                     (add T1 T0 T1 T0
                          (vector-ref T (* i 2))
                          (vector-ref T (+ (* i 2) 1)))
                     (add a1 a0 a1 a0 T1 T0)
                     (rot a1 a0 a1 a0 s)
                     (add a1 a0 a1 a0 b1 b0)))))
               (R1 (syntax-rules ()
                     ((R1 T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i)
                      (R FF T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i))))
               (R2 (syntax-rules ()
                     ((R2 T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i)
                      (R GG T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i))))
               (R3 (syntax-rules ()
                     ((R3 T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i)
                      (R HH T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i))))
               (R4 (syntax-rules ()
                     ((R4 T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i)
                      (R II T1 T0 a1 a0 b1 b0 c1 c0 d1 d0 vec k s i)))))
            ;; Round 1: Let [abcd k s i] denote the operation
            ;;   a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s)
            (R1 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 0 7 1)
            (R1 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 1 12 2)
            (R1 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 2 17 3)
            (R1 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 3 22 4)
            (R1 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 4 7 5)
            (R1 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 5 12 6)
            (R1 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 6 17 7)
            (R1 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 7 22 8)
            (R1 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 8 7 9)
            (R1 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 9 12 10)
            (R1 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 10 17 11)
            (R1 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 11 22 12)
            (R1 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 12 7 13)
            (R1 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 13 12 14)
            (R1 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 14 17 15)
            (R1 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 15 22 16)
            ;; Round 2: Let [abcd k s i] denote the operation
            ;;   a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s)
            (R2 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 1 5 17)
            (R2 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 6 9 18)
            (R2 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 11 14 19)
            (R2 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 0 20 20)
            (R2 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 5 5 21)
            (R2 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 10 9 22)
            (R2 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 15 14 23)
            (R2 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 4 20 24)
            (R2 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 9 5 25)
            (R2 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 14 9 26)
            (R2 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 3 14 27)
            (R2 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 8 20 28)
            (R2 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 13 5 29)
            (R2 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 2 9 30)
            (R2 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 7 14 31)
            (R2 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 12 20 32)
            ;; Round 3: Let [abcd k s i] denote the operation
            ;;   a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s)
            (R3 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 5 4 33)
            (R3 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 8 11 34)
            (R3 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 11 16 35)
            (R3 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 14 23 36)
            (R3 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 1 4 37)
            (R3 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 4 11 38)
            (R3 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 7 16 39)
            (R3 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 10 23 40)
            (R3 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 13 4 41)
            (R3 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 0 11 42)
            (R3 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 3 16 43)
            (R3 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 6 23 44)
            (R3 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 9 4 45)
            (R3 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 12 11 46)
            (R3 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 15 16 47)
            (R3 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 2 23 48)
            ;; Round 4: Let [abcd k s i] denote the operation
            ;;   a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s)
            (R4 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 0 6 49)
            (R4 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 7 10 50)
            (R4 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 14 15 51)
            (R4 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 5 21 52)
            (R4 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 12 6 53)
            (R4 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 3 10 54)
            (R4 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 10 15 55)
            (R4 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 1 21 56)
            (R4 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 8 6 57)
            (R4 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 15 10 58)
            (R4 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 6 15 59)
            (R4 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 13 21 60)
            (R4 T1 T0 A1 A0 B1 B0 C1 C0 D1 D0 vec 4 6 61)
            (R4 T1 T0 D1 D0 A1 A0 B1 B0 C1 C0 vec 11 10 62)
            (R4 T1 T0 C1 C0 D1 D0 A1 A0 B1 B0 vec 2 15 63)
            (R4 T1 T0 B1 B0 C1 C0 D1 D0 A1 A0 vec 9 21 64)
            ;; Then in increment each of the four registers by the
            ;; value it had before this block was started.
            (add A1 A0 A1 A0 AA1 AA0)
            (add B1 B0 B1 B0 BB1 BB0)
            (add C1 C0 C1 C0 CC1 CC0)
            (add D1 D0 D1 D0 DD1 DD0)
            (cond
             ((< n 64)
              ;; 3.5 Step 5. Output
              ;;
              ;;    The message digest produced as output is A, B, C,
              ;;    D. That is, we begin with the low-order byte of A,
              ;;    and end with the high-order byte of D.
              (if (>= n 56)
                  (lp (+ i n) 0)
                  (string-append
                   (hex A0) (hex A1)
                   (hex B0) (hex B1)
                   (hex C0) (hex C1)
                   (hex D0) (hex D1))))
             (else
              (lp (+ i 64) pad)))))))))

;; This completes the description of MD5. A reference implementation in
;; C is given in the appendix.
