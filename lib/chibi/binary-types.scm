
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define (read-u16/be in)
  (let* ((i (read-u8 in))
         (j (read-u8 in)))
    (if (eof-object? j)
        (error "end of input")
        (+ (arithmetic-shift i 8) j))))

(define (read-u16/le in)
  (let* ((i (read-u8 in))
         (j (read-u8 in)))
    (if (eof-object? j)
        (error "end of input")
        (+ (arithmetic-shift j 8) i))))

(define (assert-read-u8 in i)
  (let ((i2 (read-u8 in)))
    (if (not (eqv? i i2))
        (error "unmatched value, expected: " i " but got: " i2)
        i2)))

(define (assert-read-char in ch)
  (let ((ch2 (read-char in)))
    (if (not (eqv? ch ch2))
        (error "unmatched value, expected: " ch " but got: " ch2)
        ch2)))

(define (assert-read-string in s)
  (let ((s2 (read-string (string-length s) in)))
    (if (not (equal? s s2))
        (error "unmatched value, expected: " s " but got: " s2)
        s2)))

(define (assert-read-bytevector in bv)
  (let ((bv2 (read-bytevector (bytevector-length bv) in)))
    (if (not (equal? bv bv2))
        (error "unmatched value, expected: " bv " but got: " bv2)
        bv2)))

(define (assert-read-integer in len radix)
  (let* ((s (string-trim-both (read-string len in)
                              (lambda (ch) (or (eqv? ch #\space) (eqv? ch #\null)))))
         (n (if (equal? s "") 0 (string->number s radix))))
    (or n (error "invalid number syntax: " s))))

(define (read-padded-string in len pad)
  (string-trim-right (read-string len in) pad))

(define (read-literal val)
  (cond
   ((integer? val) (lambda (in) (assert-read-u8 in val)))
   ((char? val) (lambda (in) (assert-read-char in val)))
   ((string? val) (lambda (in) (assert-read-string in val)))
   ((bytevector? val) (lambda (in) (assert-read-bytevector in val)))
   (else (error "unknown binary literal: " val))))

(define (write-literal val)
  (cond
   ((integer? val) (lambda (x out) (write-u8 val out)))
   ((char? val) (lambda (x out) (write-char val out)))
   ((string? val) (lambda (x out) (write-string val out)))
   ((bytevector? val) (lambda (x out) (write-bytevector val out)))
   (else (error "unknown binary literal: " val))))

(define (write-padded-integer out n radix len left-pad-ch right-pad-ch)
  (let ((s (string-pad (number->string n radix) (- len 1) left-pad-ch)))
    (cond
     ((>= (string-length s) len)
      (error "number too large for width" n radix len))
     (else
      (write-string s out)
      (write-char right-pad-ch out)))))

(define (write-u16/be n out)
  (write-u8 (arithmetic-shift n -8) out)
  (write-u8 (bitwise-and n #xFF) out))

(define (write-u16/le n out)
  (write-u8 (bitwise-and n #xFF) out)
  (write-u8 (arithmetic-shift n -8) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax

(define-syntax define-auxiliary-syntax
  (syntax-rules ()
    ((define-auxiliary-syntax name)
     (define-syntax name
       (syntax-rules ()
         ((name . x)
          (syntax-error "invalid use of auxilliary syntax" (name . x))))))))

(define-auxiliary-syntax make:)
(define-auxiliary-syntax pred:)
(define-auxiliary-syntax read:)
(define-auxiliary-syntax write:)
(define-auxiliary-syntax block:)

(define-syntax syntax-let-optionals*
  (syntax-rules ()
    ((syntax-let-optionals* () type-args expr)
     expr)
    ((syntax-let-optionals* ((param default) . rest) (arg0 . args) expr)
     (let ((param arg0))
       (syntax-let-optionals* rest args expr)))
    ((syntax-let-optionals* ((param default) . rest) () expr)
     (let ((param default))
       (syntax-let-optionals* rest () expr)))
    ((syntax-let-optionals* (param . rest) (arg0 . args) expr)
     (let ((param arg0))
       (syntax-let-optionals* rest args expr)))
    ((syntax-let-optionals* (param . rest) () expr)
     (syntax-error "missing required parameter" param expr))))

(define-syntax define-binary-type
  (syntax-rules ()
    ((define-binary-type (name params ...) gen-pred gen-read gen-write)
     (define-syntax name
       (syntax-rules (pred: read: write:)
         ((name pred: type-args)
          (syntax-let-optionals* (params ...) type-args gen-pred))
         ((name read: type-args)
          (syntax-let-optionals* (params ...) type-args gen-read))
         ((name write: type-args)
          (syntax-let-optionals* (params ...) type-args gen-write)))))))

(define-binary-type (u8)
  (lambda (x) (and (exact-integer? x) (<= 0 x 255)))
  read-u8
  write-u8)

(define-binary-type (u16/le)
  (lambda (x) (and (exact-integer? x) (<= 0 x 65536)))
  read-u16/le
  write-u16/le)

(define-binary-type (u16/be)
  (lambda (x) (and (exact-integer? x) (<= 0 x 65536)))
  read-u16/be
  write-u16/be)

(define-binary-type (padded-string len (pad #\null))
  (lambda (x) (and (string? x) (<= (string-length x) len)))
  (lambda (in) (read-padded-string in len pad))
  (lambda (str out)
    (write-string (string-pad-right str len pad) out)))

(define-binary-type (fixed-string len)
  (lambda (x) (and (string? x) (= (string-length x) len)))
  (lambda (in)
    (read-string len in))
  (lambda (str out)
    (write-string str out)))

(define-binary-type (octal len)
  exact-integer?
  (lambda (in) (assert-read-integer in len 8))
  (lambda (n out)
    (write-padded-integer out n 8 len #\0 #\null)))

(define-binary-type (decimal len)
  exact-integer?
  (lambda (in) (assert-read-integer in len 10))
  (lambda (n out)
    (write-padded-integer out n 10 len #\0 #\null)))

(define-binary-type (hexadecimal len)
  exact-integer?
  (lambda (in) (assert-read-integer in len 16))
  (lambda (n out)
    (write-padded-integer out n 16 len #\0 #\null)))
