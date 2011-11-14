
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
      ""
      (let ((res (read-string n (if (pair? o) (car o) (current-input-port)))))
        (if (equal? res "")
            (read-char (open-input-string res))
            (string->utf8 res)))))

(define (read-bytevector! vec start end . o)
  (if (>= start end)
      0
      (let* ((res (read-bytevector!
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
  (apply write-string (utf8->string vec) (bytevector-length vec) o))

(define (write-partial-bytevector vec start end . o)
  (apply write-bytevector (bytevector-copy-partial vec start end) o))

(define (make-list n . o)
  (let ((init (and (pair? o) (car o))))
    (let lp ((i 0) (res '()))
      (if (>= i n) res (lp (+ i 1) (cons init res))))))

(define (list-copy ls)
  (reverse (reverse ls)))

(define (list-set! ls k x)
  (cond ((null? ls) (error "invalid list index"))
        ((zero? k) (set-car! ls x))
        (else (list-set! ls (- k 1) x))))

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

(define (vector->string vec)
  (list->string (vector->list vec)))

(define (string->vector vec)
  (list->vector (string->list vec)))

(define (string-map proc . los)
  (list->string (apply map proc (map string->list los))))

(define (string-for-each proc str . los)
  (if (null? los)
      (let ((len (string-length str)))
        (let lp ((i 0))
          (if (< i len) (begin (proc (string-ref str i)) (lp (+ i 1))))))
      (apply string-map (lambda (ch) (proc ch) ch) str los)))

(define (bytevector-copy bv)
  (let ((res (make-bytevector (bytevector-length bv))))
    (bytevector-copy! bv res)
    res))

(define (bytevector-copy! from to)
  (bytevector-copy-partial! from 0 (bytevector-length from) to 0))

(define (bytevector-copy-partial bv start end)
  (let ((res (make-bytevector (- end start))))
    (bytevector-copy-partial! bv start end res 0)
    res))

(define (bytevector-copy-partial! from start end to at)
  (do ((i start (+ i 1)))
      ((= i end))
    (bytevector-u8-set! to (+ (- i start) at) (bytevector-u8-ref from i))))
