
(define call/cc call-with-current-continuation)

(define flush-output-port flush-output)

(define (close-port port)
  ((if (input-port? port) close-input-port close-output-port) port))

(define (call-with-port port proc)
  (let ((res (proc port)))
    (close-port port)
    res))

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
            (lp (- i 1) (cons (proc (vector-ref vec i)) res))))
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
