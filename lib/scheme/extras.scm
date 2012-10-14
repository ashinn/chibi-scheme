
(define (read-sexps file . o)
  (let ((in (open-input-file file)))
    (if (and (pair? o) (car o))
        (set-port-fold-case! in #t))
    (let lp ((res '()))
      (let ((x (read in)))
        (if (eof-object? x) res (lp (cons x res)))))))

(define-syntax include
  (er-macro-transformer
   (lambda (expr rename compare)
     (let lp ((files (cdr expr)) (res '()))
       (cond
        ((null? files) (cons (rename 'begin) (reverse res)))
        (else (lp (cdr files) (append (read-sexps (car files)) res))))))))

(define-syntax include-ci
  (er-macro-transformer
   (lambda (expr rename compare)
     (let lp ((files (cdr expr)) (res '()))
       (cond
        ((null? files) (cons (rename 'begin) (reverse res)))
        (else (lp (cdr files) (append (read-sexps (car files) #t) res))))))))

(define (features) *features*)

(define exact inexact->exact)
(define inexact exact->inexact)

(define (boolean=? x y) (eq? x y))
(define (symbol=? x y) (eq? x y))

(define call/cc call-with-current-continuation)

;; Adapted from Bawden's algorithm.
(define (rationalize x e)
  (define (sr x y return)
    (let ((fx (inexact->exact (floor x))) (fy (inexact->exact (floor y))))
      (cond
       ((>= fx x)
        (return fx 1))
       ((= fx fy)
        (sr (/ (- y fy)) (/ (- x fx)) (lambda (n d) (return (+ d (* fx n)) n))))
       (else
        (return (+ fx 1) 1)))))
  (if (exact? x)
      (let ((return (if (negative? x) (lambda (num den) (/ (- num) den)) /))
            (x (abs x))
            (e (abs e)))
        (sr (- x e) (+ x e) return))
      x))

(define (square x) (* x x))

(define flush-output-port flush-output)

(define (close-port port)
  ((if (input-port? port) close-input-port close-output-port) port))

(define (u8-ready? port) (char-ready? port))

(define (call-with-port port proc)
  (let ((res (proc port)))
    (close-port port)
    res))

(define (read-bytevector n . o)
  (if (zero? n)
      #u8()
      (let ((in (if (pair? o) (car o) (current-input-port)))
            (res (make-bytevector n)))
        (let lp ((i 0))
          (if (>= i n)
              res
              (let ((x (read-u8 in)))
                (cond ((eof-object? x)
                       (if (zero? i) x (subbytes res 0 i)))
                      (else
                       (bytevector-u8-set! res i x)
                       (lp (+ i 1))))))))))

(define (read-bytevector! vec start end . o)
  (if (>= start end)
      0
      (let* ((res (read-bytevector
                   (- end start)
                   (if (pair? o) (car o) (current-input-port))))
             (len (bytevector-length res)))
        (cond
         ((zero? len)
          (read-char (open-input-string "")))
         (else
          (do ((i 0 (+ i 1)))
              ((>= i len) len)
            (bytevector-u8-set! vec (+ i start) (bytevector-u8-ref res i))))))))

(define (write-bytevector vec . o)
  (let* ((out (if (pair? o) (car o) (current-output-port)))
         (o (if (pair? o) (cdr o) '()))
         (start (if (pair? o) (car o) 0))
         (o (if (pair? o) (cdr o) '()))
         (end (if (pair? o) (car o) (bytevector-length vec))))
    (do ((i start (+ i 1)))
        ((>= i end))
      (write-u8 (bytevector-u8-ref vec i) out))))

(define (make-list n . o)
  (let ((init (and (pair? o) (car o))))
    (let lp ((i 0) (res '()))
      (if (>= i n) res (lp (+ i 1) (cons init res))))))

(define (list-copy ls)
  (reverse (reverse ls)))

(define (list-set! ls k x)
  (cond ((null? ls) (error "invalid list index"))
        ((zero? k) (set-car! ls x))
        (else (list-set! (cdr ls) (- k 1) x))))

(define (vector-map proc vec . lov)
  (if (null? lov)
      (let lp ((i (vector-length vec)) (res '()))
        (if (zero? i)
            (list->vector res)
            (lp (- i 1) (cons (proc (vector-ref vec (- i 1))) res))))
      (list->vector (apply map proc (map vector->list (cons vec lov))))))

(define (vector-for-each proc vec . lov)
  (if (null? lov)
      (let ((len (vector-length vec)))
        (let lp ((i 0))
          (cond ((< i len)
                 (proc (vector-ref vec i))
                 (lp (+ i 1))))))
      (apply for-each proc (map vector->list (cons vec lov)))))

(define (vector-copy vec)
  (let* ((len (vector-length vec))
         (res (make-vector len)))
    (do ((i 0 (+ i 1))) ((>= i len) res)
      (vector-set! res i (vector-ref vec i)))))

(define (vector-copy! to at from . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length from))))
    (do ((i at (+ i 1)) (j start (+ i 1)))
        ((>= j end))
      (vector-set! to i (vector-ref from j)))))

(define (vector->string vec)
  (list->string (vector->list vec)))

(define (string->vector vec)
  (list->vector (string->list vec)))

(define (bytevector-copy! to at from . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (bytevector-length from))))
    (do ((i at (+ i 1)) (j start (+ i 1)))
        ((>= j end))
      (bytevector-u8-set! to i (bytevector-u8-ref from j)))))

(define (bytevector-copy vec . o)
  (if (null? o)
      (subbytes vec 0)
      (apply subbytes vec o)))

;; Never use this!
(define (string-copy! to at from . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length from))))
    (do ((i at (+ i 1)) (j start (+ i 1)))
        ((>= j end))
      (string-set! to i (string-ref from j)))))

(define truncate-quotient quotient)
(define truncate-remainder remainder)
(define (truncate/ n m)
  (values (truncate-quotient n m) (truncate-remainder n m)))

(cond-expand
 (ratios
  (define (floor-quotient n m)
    (floor (/ n m))))
 (else
  (define (floor-quotient n m)
    (let ((res (floor (/ n m))))
      (if (and (exact? n) (exact? m))
          (exact res)
          res)))))
(define (floor-remainder n m)
  (- n (* m (floor-quotient n m))))
(define (floor/ n m)
  (values (floor-quotient n m) (floor-remainder n m)))
