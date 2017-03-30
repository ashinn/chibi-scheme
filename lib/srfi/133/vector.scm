
(define (vector-unfold! f vec start end . o)
  (let lp ((i start) (seeds o))
    (if (< i end)
        (call-with-values (lambda () (apply f i seeds))
          (lambda (x . seeds)
            (vector-set! vec i x)
            (lp (+ i 1) seeds))))))

(define (vector-unfold-right! f vec start end . o)
  (let lp ((i (- end 1)) (seeds o))
    (if (>= i start)
        (call-with-values (lambda () (apply f i seeds))
          (lambda (x . seeds)
            (vector-set! vec i x)
            (lp (- i 1) seeds))))))

(define (vector-unfold f len . o)
  (let ((res (make-vector len)))
    (apply vector-unfold! f res 0 len o)
    res))

(define (vector-unfold-right f len . o)
  (let ((res (make-vector len)))
    (apply vector-unfold-right! f res 0 len o)
    res))

(define (vector-reverse-copy vec . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec)))
         (len (- end start)))
    (vector-unfold-right (lambda (i) (vector-ref vec (- end i 1))) len)))

(define (vector-concatenate ls)
  (apply vector-append ls))

(define (vector-append-subvectors . o)
  (let lp ((ls o) (vecs '()))
    (if (null? ls)
        (vector-concatenate (reverse vecs))
        (lp (cdr (cddr ls))
            (cons (vector-copy (car ls) (cadr ls) (car (cddr ls))) vecs)))))

(define (vector-empty? vec)
  (zero? (vector-length vec)))

(define (vector= eq . o)
  (cond
   ((null? o) #t)
   ((null? (cdr o)) #t)
   (else
    (and (let* ((v1 (car o))
                (v2 (cadr o))
                (len (vector-length v1)))
           (and (= len (vector-length v2))
                (let lp ((i 0))
                  (or (>= i len)
                      (and (eq (vector-ref v1 i) (vector-ref v2 i))
                           (lp (+ i 1)))))))
         (apply vector= eq (cdr o))))))

(define (vector-fold kons knil vec1 . o)
  (let ((len (vector-length vec1)))
    (if (null? o)
        (let lp ((i 0) (acc knil))
          (if (>= i len) acc (lp (+ i 1) (kons acc (vector-ref vec1 i)))))
        (let lp ((i 0) (acc knil))
          (if (>= i len)
              acc
              (lp (+ i 1)
                  (apply kons acc (vector-ref vec1 i)
                         (map (lambda (v) (vector-ref v i)) o))))))))

(define (vector-fold-right kons knil vec1 . o)
  (let ((len (vector-length vec1)))
    (if (null? o)
        (let lp ((i (- len 1)) (acc knil))
          (if (negative? i) acc (lp (- i 1) (kons acc (vector-ref vec1 i)))))
        (let lp ((i (- len 1)) (acc knil))
          (if (negative? i)
              acc
              (lp (- i 1)
                  (apply kons acc (vector-ref vec1 i)
                         (map (lambda (v) (vector-ref v i)) o))))))))

(define (vector-map! proc vec1 . o)
  (let ((len (vector-length vec1)))
    (if (null? o)
        (let lp ((i 0))
          (cond
           ((>= i len) vec1)
           (else (vector-set! vec1 i (proc (vector-ref vec1 i))) (lp (+ i 1)))))
        (let lp ((i 0))
          (cond
           ((>= i len) vec1)
           (else
            (let ((x (apply proc (vector-ref vec1 i)
                            (map (lambda (v) (vector-ref v i)) o))))
              (vector-set! vec1 i x)
              (lp (+ i 1)))))))))

(define (vector-count pred? vec1 . o)
  (apply vector-fold
         (lambda (count . x) (+ count (if (apply pred? x) 1 0)))
         0
         vec1 o))

(define (vector-cumulate f knil vec)
  (let* ((len (vector-length vec))
         (res (make-vector len)))
    (let lp ((i 0) (acc knil))
      (if (>= i len)
          res
          (let ((acc (f acc (vector-ref vec i))))
            (vector-set! res i acc)
            (lp (+ i 1) acc))))))

(define (vector-index pred? vec1 . o)
  (let ((len (apply min (vector-length vec1) (map vector-length o))))
    (let lp ((i 0))
      (and (< i len)
           (if (apply pred? (vector-ref vec1 i)
                      (map (lambda (v) (vector-ref v i)) o))
               i
               (lp (+ i 1)))))))

(define (vector-index-right pred? vec1 . o)
  (let ((len (vector-length vec1)))
    (let lp ((i (- len 1)))
      (and (>= i 0)
           (if (apply pred? (vector-ref vec1 i)
                      (map (lambda (v) (vector-ref v i)) o))
               i
               (lp (- i 1)))))))

(define (complement f)
  (lambda args (not (apply f args))))

(define (vector-skip pred? vec1 . o)
  (apply vector-index (complement pred?) vec1 o))

(define (vector-skip-right pred? vec1 . o)
  (apply vector-index-right (complement pred?) vec1 o))

(define (vector-binary-search vec value cmp)
  (let lp ((lo 0) (hi (- (vector-length vec) 1)))
    (and (<= lo hi)
         (let* ((mid (quotient (+ lo hi) 2))
                (x (vector-ref vec mid))
                (y (cmp value x)))
           (cond
            ((< y 0) (lp lo (- mid 1)))
            ((> y 0) (lp (+ mid 1) hi))
            (else mid))))))

(define (vector-any pred? vec1 . o)
  (let ((len (apply min (vector-length vec1) (map vector-length o))))
    (let lp ((i 0))
      (and (< i len)
           (or (apply pred? (vector-ref vec1 i)
                      (map (lambda (v) (vector-ref v i)) o))
               (lp (+ i 1)))))))

(define (vector-every pred? vec1 . o)
  (let ((len (apply min (vector-length vec1) (map vector-length o))))
    (let lp ((i 0))
      (let ((x (apply pred? (vector-ref vec1 i)
                      (map (lambda (v) (vector-ref v i)) o))))
        (if (= i (- len 1))
            x
            (and x (lp (+ i 1))))))))

(define (vector-swap! vec i j)
  (let ((tmp (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (vector-reverse! vec . o)
  (let lp ((left (if (pair? o) (car o) 0))
           (right (- (if (and (pair? o) (pair? (cdr o)))
                         (cadr o)
                         (vector-length vec))
                     1)))
    (cond
     ((>= left right) (if #f #f))
     (else
      (vector-swap! vec left right)
      (lp (+ left 1) (- right 1))))))

(define (vector-reverse-copy! to at from . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (vector-length from))))
    (vector-copy! to at from start end)
    (vector-reverse! to at (+ at (- end start)))))

(define (reverse-vector->list vec . o)
  (reverse (apply vector->list vec o)))

(define (reverse-list->vector ls)
  (list->vector (reverse ls)))

(define (vector-partition pred? vec)
  (let* ((len (vector-length vec))
         (res (make-vector len)))
    (let lp ((i 0) (left 0) (right (- len 1)))
      (cond
       ((= i len)
        (if (< left len)
            (vector-reverse! res left))
        (values res left))
       (else
        (let ((x (vector-ref vec i)))
          (cond
           ((pred? x)
            (vector-set! res left x)
            (lp (+ i 1) (+ left 1) right))
           (else
            (vector-set! res right x)
            (lp (+ i 1) left (- right 1))))))))))
