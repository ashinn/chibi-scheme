
;; Additional accessors

(define (bytevector-u16-ref-le str i)
  (+ (bytevector-u8-ref str i)
     (arithmetic-shift (bytevector-u8-ref str (+ i 1)) 8)))

(define (bytevector-u16-ref-be str i)
  (+ (arithmetic-shift (bytevector-u8-ref str i) 8)
     (bytevector-u8-ref str (+ i 1))))

(define (bytevector-u32-ref-le str i)
  (+ (bytevector-u8-ref str i)
     (arithmetic-shift (bytevector-u8-ref str (+ i 1)) 8)
     (arithmetic-shift (bytevector-u8-ref str (+ i 2)) 16)
     (arithmetic-shift (bytevector-u8-ref str (+ i 3)) 24)))

(define (bytevector-u32-ref-be str i)
  (+ (arithmetic-shift (bytevector-u8-ref str i) 24)
     (arithmetic-shift (bytevector-u8-ref str (+ i 1)) 16)
     (arithmetic-shift (bytevector-u8-ref str (+ i 2)) 8)
     (bytevector-u8-ref str (+ i 3))))

;; Integer conversion

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

(define (bytevector->integer bv)
  (let ((len (bytevector-length bv)))
    (let lp ((i 0) (n 0))
      (if (>= i len)
          n
          (lp (+ i 1)
              (+ (arithmetic-shift n 8)
                 (bytevector-u8-ref bv i)))))))

;; Hex string conversion (big-endian, guaranteed padded to even length)

(define (integer->hex-string n)
  (let* ((res (number->string n 16))
         (len (string-length res)))
    (if (even? len)
        res
        (string-append "0" res))))

(define (hex-string->integer str)
  (string->number str 16))

(define (bytevector->hex-string bv)
  (integer->hex-string (bytevector->integer bv)))

(define (hex-string->bytevector str)
  (integer->bytevector (hex-string->integer str)))
