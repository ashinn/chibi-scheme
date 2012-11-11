;; Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Procedure: base64-encode-string str
;;   Return a base64 encoded representation of string according to the
;;   official base64 standard as described in RFC3548.

;; Procedure: base64-decode-string str
;;   Return a base64 decoded representation of string, also interpreting
;;   the alternate 62 & 63 valued characters as described in RFC3548.
;;   Other out-of-band characters are silently stripped, and = signals
;;   the end of the encoded string.  No errors will be raised.

;; Procedure: base64-encode [port]
;; Procedure: base64-decode [port]
;;   Variations of the above which read and write to ports.

;; Procedure: base64-encode-header enc str [start-col max-col nl]
;;   Return a base64 encoded representation of string as above,
;;   wrapped in =?ENC?B?...?= as per RFC1522, split across multiple
;;   MIME-header lines as needed to keep each lines length less than
;;   MAX-COL.  The string is encoded as is, and the encoding ENC is
;;   just used for the prefix, i.e. you are responsible for ensuring
;;   STR is already encoded according to ENC.  The optional argument
;;   NL is the newline separator, defaulting to CRLF.

;; This API is compatible with the Gauche library rfc.base64.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string utils

(define (string-chop str n)
  (let ((len (string-length str)))
    (let lp ((i 0) (res '()))
      (let ((j (+ i n)))
        (if (>= j len)
            (reverse (cons (substring str i len) res))
            (lp j (cons (substring str i j) res)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants and tables

(define *default-max-col* 76)

(define *outside-char* 99) ; luft-balloons
(define *pad-char* 101)    ; dalmations

(define *base64-decode-table*
  (let ((res (make-vector #x100 *outside-char*)))
    (let lp ((i 0)) ; map letters
      (cond
       ((<= i 25)
        (vector-set! res (+ i 65) i)
        (vector-set! res (+ i 97) (+ i 26))
        (lp (+ i 1)))))
    (let lp ((i 0)) ; map numbers
      (cond
       ((<= i 9)
        (vector-set! res (+ i 48) (+ i 52))
        (lp (+ i 1)))))
    ;; extras (be liberal for different common base64 formats)
    (vector-set! res (char->integer #\+) 62)
    (vector-set! res (char->integer #\-) 62)
    (vector-set! res (char->integer #\/) 63)
    (vector-set! res (char->integer #\_) 63)
    (vector-set! res (char->integer #\~) 63)
    (vector-set! res (char->integer #\=) *pad-char*)
    res))

(define (base64-decode-char c)
  (vector-ref *base64-decode-table* (char->integer c)))

(define *base64-encode-table*
  (let ((res (make-vector 64)))
    (let lp ((i 0)) ; map letters
      (cond
       ((<= i 25)
        (vector-set! res i (integer->char (+ i 65)))
        (vector-set! res (+ i 26) (integer->char (+ i 97)))
        (lp (+ i 1)))))
    (let lp ((i 0)) ; map numbers
      (cond
       ((<= i 9)
        (vector-set! res (+ i 52) (integer->char (+ i 48)))
        (lp (+ i 1)))))
    (vector-set! res 62 #\+)
    (vector-set! res 63 #\/)
    res))

(define (enc i)
  (vector-ref *base64-encode-table* i))

;; try to match common boundaries
(define decode-src-length
  (lcm 76 78))

(define decode-dst-length
  (* 3 (arithmetic-shift (+ 3 decode-src-length) -2)))

(define encode-src-length
  (* 3 1024))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decoding

;; Create a result buffer with the maximum possible length for the
;; input, and pass it to the internal base64-decode-string! utility.
;; If the resulting length used is exact, we can return that buffer,
;; otherwise we return the appropriate substring.
(define (base64-decode-string src)
  (let* ((len (string-length src))
         (dst-len (* 3 (arithmetic-shift (+ 3 len) -2)))
         (dst (make-string dst-len)))
    (base64-decode-string!
     src 0 len dst
     (lambda (src-offset res-len b1 b2 b3)
       (let ((res-len (base64-decode-finish dst res-len b1 b2 b3)))
         (if (= res-len dst-len)
             dst
             (substring dst 0 res-len)))))))

;; This is a little funky.
;;
;;   We want to skip over "outside" characters (e.g. newlines inside
;;   base64-encoded data, as would be passed in mail clients and most
;;   large base64 data).  This would normally mean two nested loops -
;;   one for overall processing the input, and one for looping until
;;   we get to a valid character.  However, many Scheme compilers are
;;   really bad about optimizing nested loops of primitives, so we
;;   flatten this into a single loop, using conditionals to determine
;;   which character is currently being read.
(define (base64-decode-string! src start end dst kont)
  (let lp ((i start)
           (j 0)
           (b1 *outside-char*)
           (b2 *outside-char*)
           (b3 *outside-char*))
    (if (>= i end)
        (kont i j b1 b2 b3)
        (let ((c (base64-decode-char (string-ref src i))))
          (cond
           ((eqv? c *pad-char*)
            (kont i j b1 b2 b3))
           ((eqv? c *outside-char*)
            (lp (+ i 1) j b1 b2 b3))
           ((eqv? b1 *outside-char*)
            (lp (+ i 1) j c b2 b3))
           ((eqv? b2 *outside-char*)
            (lp (+ i 1) j b1 c b3))
           ((eqv? b3 *outside-char*)
            (lp (+ i 1) j b1 b2 c))
           (else
            (string-set! dst
                         j
                         (integer->char
                          (bitwise-ior (arithmetic-shift b1 2)
                                       (extract-bit-field 2 4 b2))))
            (string-set! dst
                         (+ j 1)
                         (integer->char
                          (bitwise-ior
                           (arithmetic-shift (extract-bit-field 4 0 b2) 4)
                           (extract-bit-field 4 2 b3))))
            (string-set! dst
                         (+ j 2)
                         (integer->char
                          (bitwise-ior
                           (arithmetic-shift (extract-bit-field 2 0 b3) 6)
                           c)))
            (lp (+ i 1) (+ j 3)
                *outside-char* *outside-char* *outside-char*)))))))

;; If requested, account for any "partial" results (i.e. trailing 2 or
;; 3 chars) by writing them into the destination (additional 1 or 2
;; bytes) and returning the adjusted offset for how much data we've
;; written.
(define (base64-decode-finish dst j b1 b2 b3)
  (cond
   ((eqv? b1 *outside-char*)
    j)
   ((eqv? b2 *outside-char*)
    (string-set! dst j (integer->char (arithmetic-shift b1 2)))
    (+ j 1))
   (else
    (string-set! dst
                 j
                 (integer->char
                  (bitwise-ior (arithmetic-shift b1 2)
                               (extract-bit-field 2 4 b2))))
    (cond
     ((eqv? b3 *outside-char*)
      (+ j 1))
     (else
      (string-set! dst
                   (+ j 1)
                   (integer->char
                    (bitwise-ior
                     (arithmetic-shift (extract-bit-field 4 0 b2) 4)
                     (extract-bit-field 4 2 b3))))
      (+ j 2))))))

;; General port decoder: work in single blocks at a time to avoid
;; allocating memory (crucial for Scheme implementations that don't
;; allow large strings).
(define (base64-decode . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (out (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (current-output-port))))
    (let ((src (make-string decode-src-length))
          (dst (make-string decode-dst-length)))
      (let lp ((offset 0))
        (let ((src-len (+ offset
                          (read-string! decode-src-length src in offset))))
          (cond
           ((= src-len decode-src-length)
            ;; read a full chunk: decode, write and loop
            (base64-decode-string!
             src 0 decode-src-length dst
             (lambda (src-offset dst-len b1 b2 b3)
               (cond
                ((and (< src-offset src-len)
                      (eqv? #\= (string-ref src src-offset)))
                 ;; done
                 (let ((dst-len (base64-decode-finish dst dst-len b1 b2 b3)))
                   (write-string dst out 0 dst-len)))
                ((eqv? b1 *outside-char*)
                 (write-string dst out 0 dst-len)
                 (lp 0))
                (else
                 (write-string dst out 0 dst-len)
                 ;; one to three chars left in buffer
                 (string-set! src 0 (enc b1))
                 (cond
                  ((eqv? b2 *outside-char*)
                   (lp 1))
                  (else
                   (string-set! src 1 (enc b2))
                   (cond
                    ((eqv? b3 *outside-char*)
                     (lp 2))
                    (else
                     (string-set! src 2 (enc b3))
                     (lp 3))))))))))
           (else
            ;; end of source - just decode and write once
            (base64-decode-string!
             src 0 src-len dst
             (lambda (src-offset dst-len b1 b2 b3)
               (let ((dst-len (base64-decode-finish dst dst-len b1 b2 b3)))
                 (write-string dst out 0 dst-len)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding

(define (base64-encode-string str)
  (let* ((len (string-length str))
         (quot (quotient len 3))
         (rem (- len (* quot 3)))
         (res-len (arithmetic-shift (+ quot (if (zero? rem) 0 1)) 2))
         (res (make-string res-len)))
    (base64-encode-string! str 0 len res)
    res))

(define (base64-encode-string! str start end res)
  (let* ((res-len (string-length res))
         (limit (- end 2)))
    (let lp ((i start) (j 0))
      (if (>= i limit)
          (case (- end i)
            ((1)
             (let ((b1 (char->integer (string-ref str i))))
               (string-set! res j (enc (arithmetic-shift b1 -2)))
               (string-set! res
                            (+ j 1)
                            (enc (arithmetic-shift (bitwise-and #b11 b1) 4)))
               (string-set! res (+ j 2) #\=)
               (string-set! res (+ j 3) #\=)))
            ((2)
             (let ((b1 (char->integer (string-ref str i)))
                   (b2 (char->integer (string-ref str (+ i 1)))))
               (string-set! res j (enc (arithmetic-shift b1 -2)))
               (string-set! res
                            (+ j 1)
                            (enc (bitwise-ior
                                  (arithmetic-shift (bitwise-and #b11 b1) 4)
                                  (extract-bit-field 4 4 b2))))
               (string-set! res
                            (+ j 2)
                            (enc (arithmetic-shift (extract-bit-field 4 0 b2)
                                                   2)))
               (string-set! res (+ j 3) #\=))))
          (let ((b1 (char->integer (string-ref str i)))
                (b2 (char->integer (string-ref str (+ i 1))))
                (b3 (char->integer (string-ref str (+ i 2)))))
            (string-set! res j (enc (arithmetic-shift b1 -2)))
            (string-set! res
                         (+ j 1)
                         (enc (bitwise-ior
                               (arithmetic-shift (bitwise-and #b11 b1) 4)
                               (extract-bit-field 4 4 b2))))
            (string-set! res
                         (+ j 2)
                         (enc (bitwise-ior
                               (arithmetic-shift (extract-bit-field 4 0 b2) 2)
                               (extract-bit-field 2 6 b3))))
            (string-set! res (+ j 3) (enc (bitwise-and #b111111 b3)))
            (lp (+ i 3) (+ j 4)))))))

(define (base64-encode . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (out (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (current-output-port))))
    (let ((src (make-string encode-src-length))
          (dst (make-string
                (arithmetic-shift (quotient encode-src-length 3) 2))))
      (let lp ()
        (let ((n (read-string! 2048 src in)))
          (base64-encode-string! src 0 n dst)
          (write-string dst out 0 (* 3 (quotient (+ n 3) 4)))
          (if (= n 2048)
              (lp)))))))

(define (base64-encode-header encoding str . o)
  (define (round4 i) (arithmetic-shift (arithmetic-shift i -2) 2))
  (let ((start-col (if (pair? o) (car o) 0))
        (max-col (if (and (pair? o) (pair? (cdr o)))
                     (car (cdr o))
                     *default-max-col*))
        (nl (if (and (pair? o) (pair? (cdr o)) (pair? (cdr (cdr o))))
                (car (cdr (cdr o)))
                "\r\n")))
    (let* ((prefix (string-append "=?" encoding "?B?"))
           (prefix-length (+ 2 (string-length prefix)))
           (effective-max-col (round4 (- max-col prefix-length)))
           (first-max-col (round4 (- effective-max-col start-col)))
           (str (base64-encode-string str))
           (len (string-length str)))
      (if (<= len first-max-col)
          (string-append prefix str "?=")
          (string-append
           (if (positive? first-max-col)
               (string-append
                prefix (substring str 0 first-max-col) "?=" nl "\t" prefix)
               "")
           (string-concatenate (string-chop (substring str first-max-col len)
                                            effective-max-col)
                               (string-append "?=" nl "\t" prefix))
           "?=")))))

