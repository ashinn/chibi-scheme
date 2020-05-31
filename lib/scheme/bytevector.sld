
(define-library (scheme bytevector)
  (import (rename (scheme base)
                  (bytevector-copy! %bytevector-copy!))
          (scheme bitwise)
          (only (chibi)
                er-macro-transformer
                strip-syntactic-closures))
  (export
   endianness native-endianness bytevector? make-bytevector
   bytevector-length bytevector=? bytevector-fill! bytevector-copy!
   bytevector-u8-ref bytevector-s8-ref
   bytevector-u8-set! bytevector-s8-set!
   bytevector->u8-list u8-list->bytevector
   bytevector-uint-ref bytevector-sint-ref
   bytevector-uint-set! bytevector-sint-set!
   bytevector->uint-list uint-list->bytevector
   bytevector->sint-list sint-list->bytevector
   bytevector-u16-ref bytevector-s16-ref
   bytevector-u16-set! bytevector-s16-set!
   bytevector-u16-native-ref bytevector-s16-native-ref
   bytevector-u16-native-set! bytevector-s16-native-set!
   bytevector-u32-ref bytevector-s32-ref
   bytevector-u32-set! bytevector-s32-set!
   bytevector-u32-native-ref bytevector-s32-native-ref
   bytevector-u32-native-set! bytevector-s32-native-set!
   bytevector-u64-ref bytevector-s64-ref
   bytevector-u64-set! bytevector-s64-set!
   bytevector-u64-native-ref bytevector-s64-native-ref
   bytevector-u64-native-set! bytevector-s64-native-set!
   bytevector-ieee-single-native-ref
   bytevector-ieee-single-ref
   bytevector-ieee-double-native-ref
   bytevector-ieee-double-ref
   bytevector-ieee-single-native-set!
   bytevector-ieee-single-set!
   bytevector-ieee-double-native-set!
   bytevector-ieee-double-set!
   string->utf8
   string->utf16
   string->utf32
   utf8->string
   utf16->string
   utf32->string
   )
  (cond-expand
   (big-endian (begin (define (native-endianness) 'big)))
   (else (begin (define (native-endianness) 'little))))
  (begin
    (define-syntax endianness
      (er-macro-transformer
       (lambda (expr rename compare)
         (if (not (and (pair? (cdr expr))
                       (null? (cddr expr))
                       (memq (strip-syntactic-closures (cadr expr))
                             '(big little))))
             (error "endianness must be 'big or 'little" expr))
         `(,(rename 'quote) ,(cadr expr)))))
    (define (bytevector=? a b)
      (if (not (and (bytevector? a) (bytevector? b)))
          (error "bytevector expected" a b))
      (equal? a b))
    (define (bytevector-fill! bv elt)
      (do ((i (- (bytevector-length bv) 1) (- i 1)))
          ((< i 0))
        (bytevector-u8-set! bv i elt)))
    (define (bytevector-copy! from start to . o)
      (let* ((at (if (pair? o) (car o) 0))
             (len (if (and (pair? o) (pair? (cdr o)))
                      (cadr o)
                      (- (bytevector-length to) at)))
             (end (+ start len)))
        (%bytevector-copy! to at from start end)))
    (define (bytevector->u8-list bv)
      (do ((i (- (bytevector-length bv) 1) (- i 1))
           (res '() (cons (bytevector-u8-ref bv i) res)))
          ((< i 0) res)))
    (define (u8-list->bytevector ls)
      (let* ((len (length ls))
             (res (make-bytevector len)))
        (do ((ls ls (cdr ls))
             (i 0 (+ i 1)))
            ((null? ls) res)
          (bytevector-u8-set! res i (car ls))))))
  (include-shared "bytevector")
  (begin
    (define (string->utf16 str . o)
      (%string->utf16 str (if (pair? o) (car o) (endianness big))))
    (define (string->utf32 str . o)
      (%string->utf32 str (if (pair? o) (car o) (endianness big))))
    (define (utf16->string bv . o)
      (let ((endianness (if (pair? o) (car o) (endianness big)))
            (endianness-mandatory? (and (pair? o) (pair? (cdr o)) (cadr o))))
        (%utf16->string bv endianness endianness-mandatory?)))
    (define (utf32->string bv . o)
      (let ((endianness (if (pair? o) (car o) (endianness big)))
            (endianness-mandatory? (and (pair? o) (pair? (cdr o)) (cadr o))))
        (%utf32->string bv endianness endianness-mandatory?)))
    (define (bytevector-uint-ref bv k endianness size)
      (unless (positive? size) (error "size must be positive" size))
      (if (eq? endianness 'big)
          (do ((i 0 (+ i 1))
               (res 0 (+ (* res 256) (bytevector-u8-ref bv (+ k i)))))
              ((>= i size) res))
          (do ((i (- size 1) (- i 1))
               (res 0 (+ (* res 256) (bytevector-u8-ref bv (+ k i)))))
              ((< i 0) res))))
    (define (bytevector-sint-ref bv k endianness size)
      (unless (positive? size) (error "size must be positive" size))
      (let ((n (bytevector-uint-ref bv k endianness size))
            (mask (expt 2 (- (* 8 size) 1))))
        (- (bitwise-and n (bitwise-not mask))
           (bitwise-and n mask))))
    (define (bytevector-uint-set! bv k n endianness size)
      (unless (positive? size) (error "size must be positive" size))
      (if (eq? endianness 'big)
          (do ((i (- size 1) (- i 1))
               (n n (arithmetic-shift n -8)))
              ((< i 0))
            (bytevector-u8-set! bv (+ k i) (bitwise-and n #xFF)))
          (do ((i 0 (+ i 1))
               (n n (arithmetic-shift n -8)))
              ((>= i size))
            (bytevector-u8-set! bv (+ k i) (bitwise-and n #xFF)))))
    (define (bytevector-sint-set! bv k n endianness size)
      (bytevector-uint-set! bv k (+ (expt 2 (* 8 size)) n) endianness size))
    (define (bytevector->uint-list bv endianness size)
      (unless (positive? size) (error "size must be positive" size))
      (unless (zero? (modulo (bytevector-length bv) size))
        (error "size must divide length" (bytevector-length bv) size))
      (do ((i 0 (+ i size))
           (res '() (cons (bytevector-uint-ref bv i endianness size) res)))
          ((> (+ i size) (bytevector-length bv)) (reverse res))))
    (define (bytevector->sint-list bv endianness size)
      (unless (positive? size) (error "size must be positive" size))
      (unless (zero? (modulo (bytevector-length bv) size))
        (error "size must divide length" (bytevector-length bv) size))
      (do ((i 0 (+ i size))
           (res '() (cons (bytevector-sint-ref bv i endianness size) res)))
          ((> (+ i size) (bytevector-length bv)) (reverse res))))
    (define (uint-list->bytevector ls endianness size)
      (unless (positive? size) (error "size must be positive" size))
      (let ((res (make-bytevector (* (length ls) size) 0))
            (limit (expt 2 (* size 8))))
        (do ((ls ls (cdr ls))
             (i 0 (+ i size)))
            ((null? ls) res)
          (unless (<= 0 (car ls) limit)
            (error "out of range" (car ls) limit))
          (bytevector-uint-set! res i (car ls) endianness size))))
    (define (sint-list->bytevector ls endianness size)
      (unless (positive? size) (error "size must be positive" size))
      (let* ((res (make-bytevector (* (length ls) size) 0))
             (lo (- (expt 2 (- (* size 8) 1))))
             (hi (- -1 lo)))
        (do ((ls ls (cdr ls))
             (i 0 (+ i size)))
            ((null? ls) res)
          (unless (<= lo (car ls) hi)
            (error "out of range" (car ls) lo hi))
          (bytevector-sint-set! res i (car ls) endianness size))))
    ))
