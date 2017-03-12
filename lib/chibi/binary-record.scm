
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

;; Record types with user-specified binary formats.
;; A work in progress, but sufficient for tar files.

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
  (let* ((s (string-trim (read-string len in)
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

(define (string-pad-left str len . o)
  (let ((diff (- len (string-length str)))
        (pad-ch (if (pair? o) (car o) #\space)))
    (if (positive? diff)
        (string-append (make-string diff pad-ch) str)
        str)))

(define (string-pad-right str len . o)
  (let ((diff (- len (string-length str)))
        (pad-ch (if (pair? o) (car o) #\space)))
    (if (positive? diff)
        (string-append str (make-string diff pad-ch))
        str)))

(define (write-padded-integer out n radix len left-pad-ch right-pad-ch)
  (let ((s (string-pad-left (number->string n radix) (- len 1) left-pad-ch)))
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

(define-syntax define-binary-type
  (syntax-rules ()
    ((define-binary-type name gen-pred gen-read gen-write)
     (define-syntax name
       (syntax-rules (predicate reader writer)
         ((name predicate args) (gen-pred args))
         ((name reader args) (gen-read args))
         ((name writer args) (gen-write args)))))))

(define-binary-type u8
  (lambda (args) (lambda (x) (and (exact-integer? x) (<= 0 x 255))))
  (lambda (args) read-u8)
  (lambda (args) write-u8))

(define-binary-type u16/le
  (lambda (args) (lambda (x) (and (exact-integer? x) (<= 0 x 65536))))
  (lambda (args) read-u16/le)
  (lambda (args) write-u16/le))

(define-binary-type u16/be
  (lambda (args) (lambda (x) (and (exact-integer? x) (<= 0 x 65536))))
  (lambda (args) read-u16/be)
  (lambda (args) write-u16/be))

(define-binary-type padded-string
  (lambda (args)
    (let ((len (car args)))
      (lambda (x) (and (string? x) (<= (string-length x) len)))))
  (lambda (args)
    (let ((len (car args))
          (pad (if (pair? (cdr args)) (cadr args) #\null)))
      (lambda (in) (read-padded-string in len pad))))
  (lambda (args)
    (let ((len (car args))
          (pad (if (pair? (cdr args)) (cadr args) #\null)))
      (lambda (str out)
        (write-string (string-pad-right str len pad) out)))))

(define-binary-type fixed-string
  (lambda (args)
    (let ((len (car args)))
      (lambda (x) (and (string? x) (= (string-length x) len)))))
  (lambda (args)
    (let ((len (car args)))
      (lambda (in)
        (read-string len in))))
  (lambda (args)
    (lambda (str out)
      (write-string str out))))

(define-binary-type octal
  (lambda (args) exact-integer?)
  (lambda (args)
    (let ((len (car args)))
      (lambda (in) (assert-read-integer in len 8))))
  (lambda (args)
    (let ((len (car args)))
      (lambda (n out)
        (write-padded-integer out n 8 len #\0 #\null)))))

(define-binary-type decimal
  (lambda (args) exact-integer?)
  (lambda (args)
    (let ((len (car args)))
      (lambda (in) (assert-read-integer in len 10))))
  (lambda (args)
    (let ((len (car args)))
      (lambda (n out)
        (write-padded-integer out n 10 len #\0 #\null)))))

(define-binary-type hexadecimal
  (lambda (args) exact-integer?)
  (lambda (args)
    (let ((len (car args)))
      (lambda (in) (assert-read-integer in len 16))))
  (lambda (args)
    (let ((len (car args)))
      (lambda (n out)
        (write-padded-integer out n 16 len #\0 #\null)))))

(define-syntax defrec
  (syntax-rules (make pred read write block)
    ((defrec () n m p r w
       ((field-tmp field-read field-read-expr field-write field-write-expr field-get) ...)
       ((field getter . s) ...)
       (defs ...))
     (begin
       (define-record-type n (m field ...) p
         (field getter . s) ...)
       (define r
         (let ((field-read field-read-expr) ...)
           (lambda (in)
             (let* ((field-tmp (field-read in)) ...)
               (m field ...)))))
       (define w
         (let ((field-write field-write-expr) ...)
           (lambda (x out)
             (field-write (field-get x) out) ...)))
       defs ...))
    ((defrec ((make x) . rest) n m p r w b f s)
     (defrec rest n x p r w b f s))
    ((defrec ((pred x) . rest) n m p r w b f s)
     (defrec rest n m x r w b f s))
    ((defrec ((read x) . rest) n m p r w b f s)
     (defrec rest n m p x w b f s))
    ((defrec ((write x) . rest) n m p r w b f s)
     (defrec rest n m p r x b f s))
    ((defrec ((block (field (type . args) getter setter) . fields) . rest) n m p r w
       (b ...) (f ...) (s ...))
     (defrec ((block . fields) . rest) n m p r w
       (b ...
          (field read-tmp (type reader 'args) write-tmp (type writer 'args) getter))
       (f ...
          (field getter tmp-setter))
       (s ...
          (define setter
            (let ((pred? (type predicate 'args)))
              (lambda (x val)
                (if (not (pred? val))
                    (error "invalid val for" 'field val))
                (tmp-setter x val)))))))
    ((defrec ((block (field (type . args) getter) . fields) . rest) n m p r w
       (b ...) (f ...) s)
     (defrec ((block . fields) . rest) n m p r w
       (b ...
          (field read-tmp (type reader 'args) write-tmp (type writer 'args) getter))
       (f ...
          (field getter))
       s))
    ((defrec ((block (field . x)) . rest) n m p r w b f s)
     (syntax-error "invalid field in block" (field . x)))
    ((defrec ((block data . fields) . rest) n m p r w (b ...) f s)
     (defrec ((block . fields) . rest) n m p r w
       (b ...
          (tmp-data read-tmp (read-literal 'data) write-tmp (write-literal 'data) (lambda (x) x)))
       f
       s))
    ((defrec ((block) . rest) n m p r w b f s)
     (defrec rest n m p r w b f s))
    ))

(define-syntax define-binary-record-type
  (syntax-rules ()
    ((define-binary-record-type name x ...)
     (defrec (x ...) name hidden-make hidden-pred hidden-read hidden-write
       () () ()))))
