
;;> \section{Additional accessors}

;;> Retrieve a 16-bit unsigned integer value from the given bytevector
;;> \var{bv} at offset \var{i}, in little-endian order.

(define (bytevector-u16-ref-le bv i)
  (+ (bytevector-u8-ref bv i)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 1)) 8)))

;;> Retrieve a 16-bit unsigned integer value from the given bytevector
;;> \var{bv} at offset \var{i}, in big-endian order.

(define (bytevector-u16-ref-be bv i)
  (+ (arithmetic-shift (bytevector-u8-ref bv i) 8)
     (bytevector-u8-ref bv (+ i 1))))

;;> Retrieve a 32-bit unsigned integer value from the given bytevector
;;> \var{bv} at offset \var{i}, in little-endian order.

(define (bytevector-u32-ref-le bv i)
  (+ (bytevector-u8-ref bv i)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 1)) 8)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 2)) 16)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 3)) 24)))

;;> Retrieve a 32-bit unsigned integer value from the given bytevector
;;> \var{bv} at offset \var{i}, in big-endian order.

(define (bytevector-u32-ref-be bv i)
  (+ (arithmetic-shift (bytevector-u8-ref bv i) 24)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 1)) 16)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 2)) 8)
     (bytevector-u8-ref bv (+ i 3))))

;;> \section{Integer conversion}

;;> Convert an unsigned integer \var{n} to a bytevector representing
;;> the base-256 big-endian form (the zero index holds the MSB).

(define (integer->bytevector n)
  (cond
   ((zero? n)
    (make-bytevector 1 0))
   ((negative? n)
    (error "can't convert a negative integer to bytevector" n))
   (else
    (let lp ((n n) (res '()))
      (if (zero? n)
          (let* ((len (length res))
                 (bv (make-bytevector len 0)))
            (do ((i 0 (+ i 1))
                 (ls res (cdr ls)))
                ((= i len) bv)
              (bytevector-u8-set! bv i (car ls))))
          (lp (quotient n 256) (cons (remainder n 256) res)))))))

;;> The inverse of \scheme{integer->bytevector}.  Convert a bytevector
;;> representing the base-256 big-endian form (the zero index holds
;;> the MSB) to the corresponding unsigned integer.

(define (bytevector->integer bv)
  (let ((len (bytevector-length bv)))
    (let lp ((i 0) (n 0))
      (if (>= i len)
          n
          (lp (+ i 1)
              (+ (arithmetic-shift n 8)
                 (bytevector-u8-ref bv i)))))))

;;> Utility to pad a bytevector with zeros.  Padding is added to the
;;> left so as not to change the big-endian value.

(define (bytevector-pad-left bv len)
  (let ((diff (- len (bytevector-length bv))))
    (if (positive? diff)
        (bytevector-append bv (make-bytevector diff 0))
        bv)))

;;> \section{Hex string conversion}

;;> Big-endian conversion, guaranteed padded to even length.

(define (integer->hex-string n)
  (let* ((res (number->string n 16))
         (len (string-length res)))
    (if (even? len)
        res
        (string-append "0" res))))

(define (hex-string->integer str)
  (string->number str 16))

(define (bytevector->hex-string bv)
  (let ((out (open-output-string))
        (len (bytevector-length bv)))
    (let lp ((i 0))
      (cond
       ((>= i len)
        (get-output-string out))
       (else
        (write-string (integer->hex-string (bytevector-u8-ref bv i)) out)
        (lp (+ i 1)))))))

(define (hex-string->bytevector str)
  (integer->bytevector (hex-string->integer str)))
