
(define (list-sorted? < ls) (sorted? ls <))
(define (vector-sorted? < vec) (sorted? vec <))

(define (list-sort < ls) (sort ls <))
(define (list-stable-sort < ls) (sort ls <))

(define (vector-sort < vec . o)
  (if (pair? o) (sort (apply vector-copy vec o) <) (sort vec <)))
(define vector-stable-sort vector-sort)

(define (list-sort! < ls) (sort! ls <))
(define (list-stable-sort! < ls) (sort! ls <))

(define (vector-sort! < vec . o)
  (if (pair? o)
      (let ((v (vector-sort! < (apply vector-copy vec o))))
        (vector-copy! vec (car o) v)
        vec)
      (sort! vec <)))
(define vector-stable-sort! vector-sort!)

(define (list-merge < ls1 ls2) (merge ls1 ls2 <))
(define (list-merge! < ls1 ls2) (merge! ls1 ls2 <))

(define (vector-merge less vec1 vec2 . o)
  (if (pair? o)
      (if (pair? (cdr o))
          (vector-merge less
                        (vector-copy vec1 (car o) (cadr o))
                        (apply vector-copy vec2 (cddr o)))
          (vector-merge less (vector-copy vec1 (car o)) vec2))
      (let* ((e1 (vector-length vec1))
             (e2 (vector-length vec2))
             (res (make-vector (+ e1 e2))))
        (let lp ((i 0) (i1 0) (i2 0))
          (cond
           ((and (>= i1 e1) (>= i2 e2)) res)
           ((or (>= i1 e1)
                (and (< i2 e2)
                     (less (vector-ref vec2 i2) (vector-ref vec1 i1))))
            (vector-set! res i (vector-ref vec2 i2))
            (lp (+ i 1) i1 (+ i2 1)))
           (else
            (vector-set! res i (vector-ref vec1 i1))
            (lp (+ i 1) (+ i1 1) i2)))))))

(define (vector-merge! < to vec1 vec2 . o)
  (let ((start (if (pair? o) (car o) 0))
        (o (if (pair? o) (cdr o) '())))
    (let ((res (apply vector-merge < vec1 vec2 o)))
      (vector-copy! to start res))))

(define (list-delete-neighbor-dups eq ls)
  (let lp ((ls ls) (res '()))
    (cond ((null? ls) (reverse res))
          ((and (pair? res) (eq (car res) (car ls))) (lp (cdr ls) res))
          (else (lp (cdr ls) (cons (car ls) res))))))

(define (list-delete-neighbor-dups! eq ls)
  (if (pair? ls)
      (let lp ((ls (cdr ls)) (start ls))
        (cond ((null? ls) (set-cdr! start '()))
              ((eq (car start) (car ls)) (lp (cdr ls) start))
              (else (set-cdr! start ls) (lp (cdr ls) ls)))))
  ls)

(define (vector-delete-neighbor-dups eq vec . o)
  (if (zero? (vector-length vec))
      vec
      (let ((ls (if (and (pair? o) (pair? (cdr o)))
                    (vector->list vec (car o) (cadr o))
                    (apply vector->list vec o))))
        (list->vector (list-delete-neighbor-dups eq ls)))))

(define (vector-delete-neighbor-dups! eq vec . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec))))
    (cond
     ((<= end start) start)
     (else
      (let lp ((i (+ start 1))
               (fill (+ start 1)))
        (cond
         ((>= i end) fill)
         ((eq (vector-ref vec (- i 1)) (vector-ref vec i)) (lp (+ i 1) fill))
         (else
          (if (> i fill)
              (vector-set! vec fill (vector-ref vec i)))
          (lp (+ i 1) (+ fill 1)))))))))

;; Median of 3 (good in practice, use median-of-medians for guaranteed
;; linear time).
(define (choose-pivot < vec left right)
  (let* ((mid (quotient (+ left right) 2))
         (a (vector-ref vec left))
         (b (vector-ref vec mid))
         (c (vector-ref vec right)))
    (if (< a b)
        (if (< b c) mid (if (< a c) right left))
        (if (< a c) left (if (< b c) right mid)))))

;; Partitions around elt and returns the resulting median index.
(define (vector-partition! < vec left right pivot)
  (define (swap! i j)
    (let ((tmp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j tmp)))
  (let ((elt (vector-ref vec pivot)))
    (swap! pivot right)
    (let lp ((i left)
             (j left))
      (cond
       ((= i right)
        (swap! i j)
        j)
       ((< (vector-ref vec i) elt)
        (swap! i j)
        (lp (+ i 1) (+ j 1)))
       (else
        (lp (+ i 1) j))))))

;; Permutes vec in-place to move the k smallest elements as ordered by
;; < to the beginning of the vector (unsorted).  Returns the nth smallest.
(define (vector-select! < vec k . o)
  (let* ((left (if (pair? o) (car o) 0))
         (k (+ k left)))
    (if (not (<= 0 k (vector-length vec)))
        (error "k out of range" vec k))
    (let select ((left left)
                 (right (- (if (and (pair? o) (pair? (cdr o)))
                               (cadr o)
                               (vector-length vec))
                           1)))
      (if (>= left right)
          (vector-ref vec left)
          (let* ((pivot (choose-pivot < vec left right))
                 (pivot-index (vector-partition! < vec left right pivot)))
            (cond
             ((= k pivot-index)
              (vector-ref vec k))
             ((< k pivot-index)
              (select left (- pivot-index 1)))
             (else
              (select (+ pivot-index 1) right))))))))

(define (vector-separate! < vec k . o)
  (apply vector-select! < vec k o)
  (if #f #f))

(define (vector-find-median! < vec knil . o)
  (vector-sort! < vec)  ; required by SRFI 132
  (let* ((len (vector-length vec))
         (mid (quotient len 2))
         (mean (if (pair? o) (car o) (lambda (a b) (/ (+ a b) 2)))))
    (cond
     ((zero? len) knil)
     ((odd? len) (vector-ref vec mid))
     (else (mean (vector-ref vec (- mid 1)) (vector-ref vec mid))))))

(define (vector-find-median < vec knil . o)
  (let* ((vec (vector-copy vec))
         (len (vector-length vec))
         (mid (quotient len 2))
         (mean (if (pair? o) (car o) (lambda (a b) (/ (+ a b) 2)))))
    (cond
     ((zero? len) knil)
     (else
      (vector-separate! < vec mid)
      (cond
       ((odd? len) (vector-ref vec mid))
       (else (mean (vector-ref vec (- mid 1)) (vector-ref vec mid))))))))
