;; quoted-printable.scm -- RFC2045 implementation
;; Copyright (c) 2005-2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> RFC 2045 quoted printable encoding and decoding utilities.  This
;;> API is backwards compatible with the Gauche library
;;> rfc.quoted-printable.

;;> \schemeblock{
;;> (define (mime-encode-header header value charset)
;;>   (let ((prefix (string-append header ": "))
;;>         (str (ces-convert value "UTF8" charset)))
;;>     (string-append
;;>      prefix
;;>      (quoted-printable-encode-header charset str (string-length prefix)))))
;;> }

(define *default-max-col* 76)

;; Allow for RFC1522 quoting for headers by always escaping ? and _
(define (qp-encode bv start-col max-col separator)
  (define (hex i) (+ i (if (<= i 9) 48 55)))
  (let ((end (bytevector-length bv))
        (buf (make-bytevector max-col))
        (out (open-output-bytevector)))
    (let lp ((i 0) (col start-col))
      (cond
       ((= i end)
        (write-bytevector (bytevector-copy buf 0 col) out)
        (get-output-bytevector out))
       ((>= col (- max-col 3))
        (write-bytevector (bytevector-copy buf 0 col) out)
        (lp i 0))
       (else
        (let ((c (bytevector-u8-ref bv i)))
          (cond
           ((and (<= 33 c 126) (not (memq c '(61 63 95))))
            (bytevector-u8-set! buf col c)
            (lp (+ i 1) (+ col 1)))
           (else
            (bytevector-u8-set! buf col (char->integer #\=))
            (bytevector-u8-set! buf (+ col 1) (hex (arithmetic-shift c -4)))
            (bytevector-u8-set! buf (+ col 2) (hex (bitwise-and c #b1111)))
            (lp (+ i 1) (+ col 3))))))))))

;;> Return a quoted-printable encoded representation of the input
;;> according to the official standard as described in RFC2045.
;;>
;;> ? and _ are always encoded for compatibility with RFC1522
;;> encoding, and soft newlines are inserted as necessary to keep each
;;> lines length less than \var{max-col} (default 76).  The starting
;;> column may be overridden with \var{start-col} (default 0).

(define (quoted-printable-encode-string src . o)
  (if (string? src)
      (utf8->string
       (apply quoted-printable-encode-bytevector
              (string->utf8 src)
              o))
      (apply quoted-printable-encode-bytevector src o)))

(define (quoted-printable-encode-bytevector . o)
  (let* ((src (if (pair? o) (car o) (current-input-port)))
         (o (if (pair? o) (cdr o) '()))
         (start-col (if (pair? o) (car o) 0))
         (o (if (pair? o) (cdr o) '()))
         (max-col (if (pair? o) (car o) *default-max-col*))
         (o (if (pair? o) (cdr o) '()))
         (sep (if (pair? o) (car o) (string->utf8 "=\r\n"))))
    (qp-encode (if (bytevector? src) src (read-bytevector 1000000000 src))
               start-col max-col sep)))

;;> Variation of the above to read and write to ports.

(define (quoted-printable-encode . o)
  (write-string (apply quoted-printable-encode-string o)))

;;> Return a quoted-printable encoded representation of string as
;;> above, wrapped in =?ENC?Q?...?= as per RFC1522, split across
;;> multiple MIME-header lines as needed to keep each lines length
;;> less than \var{max-col}.  The string is encoded as is, and the
;;> encoding \var{enc} is just used for the prefix, i.e. you are
;;> responsible for ensuring \var{str} is already encoded according to
;;> \var{enc}.

(define (quoted-printable-encode-header encoding . o)
  (let* ((src (if (pair? o) (car o) (current-input-port)))
         (o (if (pair? o) (cdr o) '()))
         (start-col (if (pair? o) (car o) 0))
         (o (if (pair? o) (cdr o) '()))
         (max-col (if (pair? o) (car o) *default-max-col*))
         (o (if (pair? o) (cdr o) '()))
         (nl (if (pair? o) (car o) "\r\n")))
    (let* ((prefix (string-append "=?" encoding "?Q?"))
           (prefix-length (+ 2 (string-length prefix)))
           (separator (string->utf8 (string-append "?=" nl "\t" prefix)))
           (effective-max-col (- max-col prefix-length)))
      (bytevector-append
       (string->utf8 prefix)
       (qp-encode (if (string? src) src (port->string src))
                  start-col effective-max-col separator)
       (string->utf8 "?=")))))

;;> Return a quoted-printable decoded representation of \var{str}.  If
;;> \var{mime-header?} is specified and true, _ will be decoded as as
;;> space in accordance with RFC1522.  No errors will be raised on
;;> invalid input.

(define (quoted-printable-decode-string src . o)
  (if (string? src)
      (utf8->string
       (apply quoted-printable-decode-bytevector
              (string->utf8 src)
              o))
      (apply quoted-printable-decode-bytevector src o)))

(define (quoted-printable-decode-bytevector  . o)
  (define (hex? c)
    (or (char<=? #\0 (integer->char c) #\9)
        (char<=? #\A (integer->char c) #\F)))
  (define (unhex1 i)
    (if (>= i 65) (- i 55) (- i 48)))
  (define (unhex c1 c2)
    (+ (arithmetic-shift (unhex1 c1) 4) (unhex1 c2)))
  (let ((src (if (pair? o) (car o) (current-input-port)))
        (mime-header? (and (pair? o) (pair? (cdr o)) (car (cdr o)))))
    (let* ((bv (if (bytevector? src) src (read-bytevector 1000000000 src)))
           (end (bytevector-length bv))
           (out (open-output-bytevector)))
      (let lp ((i 0))
        (cond
         ((>= i end)
          (get-output-bytevector out))
         (else
          (let ((c (bytevector-u8-ref bv i)))
            (case c
              ((61)                    ; = escapes
               (cond
                ((< (+ i 2) end)
                 (let ((c2 (bytevector-u8-ref bv (+ i 1))))
                   (cond
                    ((eq? c2 10) (lp (+ i 2)))
                    ((eq? c2 13)
                     (lp (if (eq? 10 (bytevector-u8-ref bv (+ i 2)))
                             (+ i 3)
                             (+ i 2))))
                    ((hex? c2)
                     (let ((c3 (bytevector-u8-ref bv (+ i 2))))
                       (if (hex? c3) (write-u8 (unhex c2 c3) out))
                       (lp (+ i 3))))
                    (else (lp (+ i 3))))))))
              ((95)                    ; maybe translate _ to space
               (write-u8 (if mime-header? 32 c) out)
               (lp (+ i 1)))
              ((32 9)          ; strip trailing whitespace
               (let lp2 ((j (+ i 1)))
                 (cond
                  ((not (= j end))
                   (case (bytevector-u8-ref bv j)
                     ((32 9) (lp2 (+ j 1)))
                     ((10)
                      (lp (+ j 1)))
                     ((13)
                      (let ((k (+ j 1)))
                        (lp (if (and (< k end)
                                     (eq? 10 (bytevector-u8-ref bv k)))
                                (+ k 1) k))))
                     (else
                      (write-bytevector (bytevector-copy bv i j) out)
                      (lp j)))))))
              (else                     ; a literal char
               (write-u8 c out)
               (lp (+ i 1)))))))))))

;;> Variation of the above to read and write to ports.

(define (quoted-printable-decode . o)
  (write-string (apply quoted-printable-decode-string o)))
