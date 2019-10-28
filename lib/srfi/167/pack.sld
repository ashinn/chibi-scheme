;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; Based on FoundationDB Python bindings pack and unpack functions
;;
(define-library (srfi 167 pack)

  (export pack unpack *null*)

  (import (scheme base)
          (srfi 151)
          (srfi 1)
          (srfi 158))

  (begin

    (define *null* '(null))

    (define *null-code* #x00)
    ;; variable length
    (define *bytes-code* #x01)
    (define *string-code* #x02)
    (define *symbol-code* #x03)
    (define *nested-code* #x05)
    ;; integers
    (define *neg-int-start* #x0B)
    (define *int-zero-code* #x14)
    (define *pos-int-end* #x1D)
    ;; double
    (define *double-code* #x21)
    ;; true and false
    (define *false-code* #x26)
    (define *true-code* #x27)
    (define *escape-code* #xFF)

    ;; pack

    (define (struct:pack>Q integer)
      (let ((bytevector (make-bytevector 8 0)))
        (let loop ((index 0))
          (unless (= index 8)
            (bytevector-u8-set! bytevector
                                index (bitwise-and
                                       (arithmetic-shift integer (- (* (- 7 index) 8)))
                                       #xFF))
            (loop (+ index 1))))
        bytevector))

    (define (struct:unpack>Q bytevector)
      (let loop ((index 0)
                 (out 0))
        (if (= index 8)
            out
            (loop (+ index 1)
                  (+ out
                     (arithmetic-shift
                      (bytevector-u8-ref bytevector index)
                      (* (- 7 index) 8)))))))

    (define (%%pack-bytes bv accumulator)
      (let loop ((index 0))
        (unless (= index (bytevector-length bv))
          (let ((byte (bytevector-u8-ref bv index)))
            (if (zero? byte)
                (begin ;; escape null byte
                  (accumulator #x00)
                  (accumulator *escape-code*))
                (accumulator byte))
            (loop (+ index 1)))))
      (accumulator #x00))

    (define *bigish* (arithmetic-shift 1 (* 8 8)))

    (define *limits*
      (let ((limits (make-vector 9)))
        (let loop ((index 0))
          (unless (= index 9)
            (vector-set! limits index (- (arithmetic-shift 1 (* index 8)) 1))
            (loop (+ index 1))))
        limits))

    (define (bisect vector value)
      (let loop ((low 0)
                 (high (vector-length vector)))
        (if (>= low high)
            low
            (let ((middle (quotient (+ low high) 2)))
              (if (< (vector-ref vector middle) value)
                  (loop (+ middle 1) high)
                  (loop low middle))))))

    (define (%%pack-positive-integer integer accumulator)
      (if (< integer *bigish*)
          ;; small integer
          (let* ((length (integer-length integer))
                 (n (exact (ceiling (/ length 8))))
                 (bv (struct:pack>Q integer)))
            (accumulator (+ *int-zero-code* n))
            (let loop ((index (- (bytevector-length bv) n)))
              (unless (= index (bytevector-length bv))
                (accumulator (bytevector-u8-ref bv index))
                (loop (+ index 1)))))
          ;; big integer
          (let ((length (exact (floor (/ (+ (integer-length integer) 7) 8)))))
            (accumulator *pos-int-end*)
            (accumulator length)
            (let loop ((index (- length 1)))
              (unless (= index -1)
                (accumulator (bitwise-and (arithmetic-shift integer (- (* 8 index)))
                                          #xFF))
                (loop (- index 1)))))))

    (define (%%pack-negative-integer integer accumulator)
      (if (< (- integer) *bigish*)
          ;; small negative integer
          (let* ((n (bisect *limits* (- integer)))
                 (maxv (vector-ref *limits* n))
                 (bv (struct:pack>Q (+ maxv integer))))
            (accumulator (- *int-zero-code* n))
            (let loop ((index (- (bytevector-length bv) n)))
              (unless (= index (bytevector-length bv))
                (accumulator (bytevector-u8-ref bv index))
                (loop (+ index 1)))))
          ;; big negative integer
          (let* ((length (exact (ceiling (/ (+ (integer-length integer) 7) 8))))
                 (integer (+ integer (- (arithmetic-shift 1 (* length 8)) 1))))
            (accumulator *neg-int-start*)
            (accumulator (bitwise-xor length #xFF))
            (let loop ((index (- length 1)))
              (unless (= index -1)
                (accumulator (bitwise-and (arithmetic-shift integer (- (* 8 index)))
                                          #xFF))
                (loop (- index 1)))))))

    (define (%%pack accumulator)
      (lambda (value)
        (cond
         ((eq? value *null*) (accumulator *null-code*))
         ((eq? value #t) (accumulator *true-code*))
         ((eq? value #f) (accumulator *false-code*))
         ((bytevector? value) (accumulator *bytes-code*) (%%pack-bytes value accumulator))
         ((string? value) (accumulator *string-code*) (%%pack-bytes (string->utf8 value) accumulator))
         ((symbol? value)
          (accumulator *symbol-code*)
          (%%pack-bytes (string->utf8 (symbol->string value)) accumulator))
         ;; integer
         ((and (number? value) (exact? value) (< value 0)) (%%pack-negative-integer value accumulator))
         ((and (number? value) (exact? value) (= value 0)) (accumulator *int-zero-code*))
         ((and (number? value) (exact? value) (> value 0)) (%%pack-positive-integer value accumulator))
         ;;
         (else (error 'pack "unsupported data type" value)))))

    (define (%pack args accumulator)
      (for-each (%%pack accumulator) args))

    (define (pack . args)
      (let ((accumulator (bytevector-accumulator)))
        (%pack args accumulator)
        (accumulator (eof-object))))

    ;; unpack

    (define (list->bytevector list)
      (let ((vec (make-bytevector (length list) 0)))
        (let loop ((i 0) (list list))
          (if (null? list)
              vec
              (begin
                (bytevector-u8-set! vec i (car list))
                (loop (+ i 1) (cdr list)))))))

    (define (unpack-bytes bv position)
      (let loop ((position position)
                 (out '()))
        (if (zero? (bytevector-u8-ref bv position))
            (cond
             ;; end of bv
             ((= (+ position 1) (bytevector-length bv))
              (values (list->bytevector (reverse out)) (+ position 1)))
             ;; escaped null bytes
             ((= (bytevector-u8-ref bv (+ position 1)) *escape-code*)
              (loop (+ position 2) (cons #x00 out)))
             ;; end of string
             (else (values (list->bytevector (reverse out)) (+ position 1))))
            ;; just a byte
            (loop (+ position 1) (cons (bytevector-u8-ref bv position) out)))))

    (define (unpack-positive-integer bv code position)
      (let* ((n (- code 20))
             (sub (make-bytevector 8 0)))
        (let loop ((index 0))
          (unless (= index n)
            (bytevector-u8-set! sub (+ (- 8 n) index) (bytevector-u8-ref bv (+ position 1 index)))
            (loop (+ index 1))))
        (values (struct:unpack>Q sub) (+ position 1 n))))

    (define (unpack-negative-integer bv code position)
      (let* ((n (- 20 code))
             (maxv (vector-ref *limits* n))
             (sub (make-bytevector 8 0)))
        (let loop ((index 0))
          (unless (= index n)
            (bytevector-u8-set! sub (+ (- 8 n) index) (bytevector-u8-ref bv (+ position 1 index)))
            (loop (+ index 1))))
        (values (- (struct:unpack>Q sub) maxv) (+ position 1 n))))

    (define (unpack-bigish-positive-integer bv code position)
      (let ((length (bytevector-u8-ref bv (+ position 1))))
        (values (let loop ((range (iota length))
                           (out 0))
                  (if (null? range)
                      out
                      (loop (cdr range) (+ (arithmetic-shift out 8)
                                           (bytevector-u8-ref bv (+ position 2 (car range)))))))
                (+ position 2 length))))

    (define (unpack-bigish-negative-integer bv code position)
      (let ((length (bitwise-xor (bytevector-u8-ref bv (+ position 1)) #xFF)))
        (values (let loop ((range (iota length))
                           (out 0))
                  (if (null? range)
                      (+ (- out (arithmetic-shift 1 (* length 8))) 1)
                      (loop (cdr range) (+ (arithmetic-shift out 8)
                                           (bytevector-u8-ref bv (+ position 2 (car range)))))))
                (+ position 2 length))))

    (define (unpack bv)
      (let loop ((position 0)
                 (out '()))
        (if (= position (bytevector-length bv))
            (reverse out)
            (let ((code (bytevector-u8-ref bv position)))
              (cond
               ;; null, true, false and zero
               ((= code *null-code*) (loop (+ position 1) (cons *null* out)))
               ((= code *true-code*) (loop (+ position 1) (cons #t out)))
               ((= code *false-code*) (loop (+ position 1) (cons #f out)))
               ((= code *int-zero-code*) (loop (+ position 1) (cons 0 out)))
               ;; variable length
               ((= code *bytes-code*)
                (call-with-values (lambda () (unpack-bytes bv (+ position 1)))
                  (lambda (value position) (loop position (cons value out)))))
               ((= code *string-code*)
                (call-with-values (lambda () (unpack-bytes bv (+ position 1)))
                  (lambda (value position) (loop position (cons (utf8->string value) out)))))
               ((= code *symbol-code*)
                (call-with-values (lambda () (unpack-bytes bv (+ position 1)))
                  (lambda (value position) (loop position (cons (string->symbol (utf8->string value)) out)))))
               ;; integers
               ((and (> code *int-zero-code*) (< code *pos-int-end*))
                (call-with-values (lambda () (unpack-positive-integer bv code position))
                  (lambda (value position) (loop position (cons value out)))))
               ((and (> code *neg-int-start*) (< code *int-zero-code*))
                (call-with-values (lambda () (unpack-negative-integer bv code position))
                  (lambda (value position) (loop position (cons value out)))))
               ((= code *pos-int-end*)
                (call-with-values (lambda () (unpack-bigish-positive-integer bv code position))
                  (lambda (value position) (loop position (cons value out)))))
               ((= code *neg-int-start*)
                (call-with-values (lambda () (unpack-bigish-negative-integer bv code position))
                  (lambda (value position) (loop position (cons value out)))))
               ;; oops
               (else (error 'unpack "unsupported code" code)))))))

    ))
