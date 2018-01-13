
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitive layer adapted from SRFI 41 reference impl
;; TODO: rewrite this in terms or R7RS lazy primitives

(define-record-type Stream
  (make-stream box)
  stream?
  (box stream-promise stream-promise!))

(define-record-type Stream-Pare
  (make-stream-pare kar kdr)
  stream-pare?
  (kar stream-kar)
  (kdr stream-kdr))

(define-syntax stream-lazy
  (syntax-rules ()
    ((lazy expr)
     (make-stream (cons 'lazy (lambda () expr))))))

(define (stream-eager expr)
  (make-stream (cons 'eager expr)))

(define-syntax stream-delay
  (syntax-rules ()
    ((stream-delay expr)
     (stream-lazy (stream-eager expr)))))

(define (stream-force promise)
  (let ((content (stream-promise promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* ((cdr content)))
                      (content  (stream-promise promise)))
                 (if (not (eqv? (car content) 'eager))
                     (begin (set-car! content (car (stream-promise promise*)))
                            (set-cdr! content (cdr (stream-promise promise*)))
                            (stream-promise! promise* content)))
                 (stream-force promise))))))

(define stream-null (stream-delay (cons 'stream 'null)))

(define (stream-pair? obj)
  (and (stream? obj) (stream-pare? (stream-force obj))))

(define (stream-null? obj)
  (and (stream? obj)
       (eqv? (stream-force obj)
             (stream-force stream-null))))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons obj strm)
     (stream-delay (make-stream-pare (stream-delay obj) (stream-lazy strm))))))

(define (stream-car strm)
  (cond ((not (stream? strm)) (error 'stream-car "non-stream"))
        ((stream-null? strm) (error 'stream-car "null stream"))
        (else (stream-force (stream-kar (stream-force strm))))))

(define (stream-cdr strm)
  (cond ((not (stream? strm)) (error 'stream-cdr "non-stream"))
        ((stream-null? strm) (error 'stream-cdr "null stream"))
        (else (stream-kdr (stream-force strm)))))

(define-syntax stream-lambda
  (syntax-rules ()
    ((stream-lambda formals body0 body1 ...)
     (lambda formals (stream-lazy (let () body0 body1 ...))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; derived

(define-syntax assert
  (syntax-rules ()
    ((assert expr ...)
     (begin (unless expr (error "assertion failed" 'expr)) ...))))

(define-syntax define-stream
  (syntax-rules ()
    ((define-stream (name . args) e0 e1 ...)
     (define name
       (stream-lambda args e0 e1 ...)))))

(define-syntax stream-let
  (syntax-rules ()
    ((stream-let lp ((name val) ...) e0 e1 ...)
     ((letrec ((lp (stream-lambda (name ...) e0 e1 ...))) lp)
      val ...))))

(define-syntax stream
  (syntax-rules ()
    ((stream) stream-null)
    ((stream x y ...) (stream-cons x (stream y ...)))))

(define (list->stream ls)
  (assert (list? ls))
  (let lp ((ls (reverse ls)) (res stream-null))
    (if (null? ls)
        res
        (lp (cdr ls) (stream-cons (car ls) res)))))

(define (port->stream in)
  (assert (input-port? in))
  (let lp ()
    (let ((ch (read-char in)))
      (if (eof-object? ch)
          stream-null
          (stream-cons ch (lp))))))

(define (stream->list x . o)
  (let ((n (if (pair? o) x +inf.0)))
    (assert (not (negative? n)))
    (let lp ((i 0)
             (strm (if (pair? o) (car o) x))
             (res '()))
      (if (or (>= i n) (stream-null? strm))
          (reverse res)
          (lp (+ i 1)
              (stream-cdr strm)
              (cons (stream-car strm) res))))))

(define (stream-concat strms)
  (assert (stream? strms))
  (if (stream-null? strms)
      stream-null
      (let lp ((strm (stream-car strms))
               (strms (stream-cdr strms)))
        (assert (stream? strm))
        (cond
         ((stream-null? strm)
          (if (stream-null? strms)
              stream-null
              (lp (stream-car strms) (stream-cdr strms))))
         (else
          (stream-cons (stream-car strm)
                       (lp (stream-cdr strm) strms)))))))

(define (stream-append . strms)
  (stream-concat (list->stream strms)))

(define (stream-from n . o)
  (let ((step (if (pair? o) (car o) 1)))
    (assert (number? n) (number? step))
    (let lp ((n n))
      (stream-cons n (lp (+ n step))))))

(define (stream-range first past . o)
  (let ((step (if (pair? o) (car o) (if (< first past) 1 -1))))
    (assert (number? first) (number? past) (number? step))
    (if (positive? step)
        (stream-let lp ((n first))
          (if (< n past)
              (stream-cons n (lp (+ n step)))
              stream-null))
        (stream-let lp ((n first))
          (if (> n past)
              (stream-cons n (lp (+ n step)))
              stream-null)))))

(define (stream-constant . o)
  (let lp ((ls o))
    (if (null? ls)
        (lp o)
        (stream-cons (car ls) (lp (cdr ls))))))

(define (stream-ref strm k)
  (assert (stream? strm) (integer? k) (not (negative? k)))
  (if (positive? k)
      (stream-ref (stream-cdr strm) (- k 1))
      (stream-car strm)))

(define (stream-length strm)
  (assert (stream? strm))
  (let lp ((strm strm) (len 0))
    (if (stream-null? strm)
        len
        (lp (stream-cdr strm) (+ len 1)))))

(define (stream-drop k strm)
  (assert (integer? k) (not (negative? k)) (stream? strm))
  (stream-let drop ((k k) (strm strm))
    (if (or (zero? k) (stream-null? strm))
        strm
        (drop (- k 1) (stream-cdr strm)))))

(define (stream-drop-while pred? strm)
  (assert (procedure? pred?) (stream? strm))
  (stream-let drop-while ((strm strm))
    (if (or (stream-null? strm) (not (pred? (stream-car strm))))
        strm
        (drop-while (stream-cdr strm)))))

(define (stream-filter pred? strm)
  (assert (procedure? pred?) (stream? strm))
  (stream-let filter ((strm strm))
    (cond ((stream-null? strm) stream-null)
          ((pred? (stream-car strm))
           (stream-cons (stream-car strm) (filter (stream-cdr strm))))
          (else not (filter (stream-cdr strm))))))

(define (stream-for-each proc strm)
  (assert (procedure? proc) (stream? strm))
  (when (stream-pair? strm)
    (proc (stream-car strm))
    (stream-for-each proc (stream-cdr strm))))

(define (stream-fold kons knil strm)
  (assert (procedure? kons) (stream? strm))
  (let fold ((acc knil) (strm strm))
    (if (stream-null? strm)
        acc
        (fold (kons (stream-car strm) acc) (stream-cdr strm)))))

(define (stream-scan proc base strm)
  (assert (procedure? proc) (stream? strm))
  (stream-let scan ((acc base) (strm strm))
    (if (stream-null? strm)
        (stream acc)
        (stream-cons acc
                     (scan (proc acc (stream-car strm))
                           (stream-cdr strm))))))

(define (stream-map proc strm . o)
  (assert (procedure? proc) (stream? strm))
  (if (pair? o)
      (stream-let lp ((strms (cons strm o)))
        (if (any stream-null? strms)
            stream-null
            (stream-cons (apply proc (map stream-car strms))
                         (lp (map stream-cdr strms)))))
      (stream-let lp ((strm strm))
        (if (stream-null? strm)
            stream-null
            (stream-cons (proc (stream-car strm))
                         (lp (stream-cdr strm)))))))

(define (stream-iterate proc base)
  (assert (procedure? proc))
  (stream-let iterate ((base base))
    (stream-cons base (iterate (proc base)))))

(define (stream-take k strm)
  (assert (integer? k) (not (negative? k)) (stream? strm))
  (stream-let take ((k k) (strm strm))
    (if (and (positive? k) (stream-pair? strm))
        (stream-cons (stream-car strm) (take (- k 1) (stream-cdr strm)))
        stream-null)))

(define (stream-take-while pred strm)
  (assert (procedure? pred) (stream? strm))
  (stream-let take-while ((strm strm))
    (if (and (stream-pair? strm) (pred (stream-car strm)))
        (stream-cons (stream-car strm) (take-while (stream-cdr strm)))
        stream-null)))

(define-syntax stream-of
  (syntax-rules ()
    ((stream-of expr . clauses)
     (stream-of/aux expr stream-null . clauses))))

(define-syntax stream-of/aux
  (syntax-rules (in is)
    ((stream-of/aux expr tail)
     (stream-cons expr tail))
    ((stream-of/aux expr tail (var in s) . rest)
     (stream-let lp ((strm s))
       (if (stream-null? strm)
           tail
           (let ((var (stream-car strm)))
             (stream-of/aux expr (lp (stream-cdr strm)) . rest)))))
    ((stream-of/aux expr tail (var is e) . rest)
     (let ((var e))
       (stream-of/aux expr tail . rest)))
    ((stream-of/aux expr tail pred . rest)
     (if pred (stream-of/aux expr tail . rest) tail))))

(define (stream-reverse strm)
  (list->stream (reverse (stream->list strm))))

(define (stream-unfold mapper pred gen base)
  (assert (procedure? mapper) (procedure? pred) (procedure? gen))
  (stream-let unfold ((base base))
    (if (pred base)
        (stream-cons (mapper base) (unfold (gen base)))
        stream-null)))

(define (stream-unfolds proc seed)
  (assert (procedure? proc))
  (let ((strm (stream-let lp ((seed seed))
                (call-with-values
                    (lambda () (proc seed))
                  (lambda ls
                    (stream-cons (cdr ls)
                                 (lp (car ls))))))))
    (apply values
           (map (lambda (i)
                  (stream-let lp ((strm strm))
                    (let ((x (list-ref (stream-car strm) i)))
                      (cond
                       ((null? x) stream-null)
                       ((pair? x) (stream-cons (car x) (lp (stream-cdr strm))))
                       (else (lp (stream-cdr strm)))))))
                (iota (length (stream-car strm)))))))

(define (stream-zip strm . o)
  (assert (stream? strm) (every stream? o))
  (stream-let lp ((strms (cons strm o)))
    (if (every stream-pair? strms)
        (stream-cons (map stream-car strms)
                     (lp (map stream-cdr strms)))
        stream-null)))

(define-syntax stream-match
  (syntax-rules ()
    ((stream-match expr clause ...)
     (let ((strm expr))
       (assert (stream? strm))
       (stream-match-next strm clause ...)))))

(define-syntax stream-match-next
  (syntax-rules ()
    ((stream-match-next strm)
     (error "no pattern matched"))
    ((stream-match-next strm clause . clauses)
     (let ((fail (lambda () (stream-match-next strm . clauses))))
       (stream-match-one strm clause (fail))))))

(define-syntax stream-match-one
  (syntax-rules (_)
    ((stream-match-one strm (() . body) fail)
     (if (stream-null? strm)
         (stream-match-body fail . body)
         fail))
    ((stream-match-one strm (_ . body) fail)
     (stream-match-body fail . body))
    ((stream-match-one strm ((a . b) . body) fail)
     (if (stream-pair? strm)
         (stream-match-one
          (stream-car strm)
          (a
           (stream-match-one (stream-cdr strm) (b . body) fail))
          fail)
         fail))
    ((stream-match-one strm (a . body) fail)
     (let ((a strm))
       (stream-match-body fail . body)))))

(define-syntax stream-match-body
  (syntax-rules ()
    ((stream-match-body fail fender expr)
     (if fender expr fail))
    ((stream-match-body fail expr)
     expr)))

;; Local variables:
;; eval: (put 'stream-let 'scheme-indent-function 2)
;; End:
