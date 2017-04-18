
(define (opt-hash eq-fn o)
  (if (pair? o)
      (car o)
      (if (eq? eq? eq-fn) hash-by-identity hash)))

(define (make-hash-table x . o)
  (if (comparator? x)
      (%make-hash-table (comparator-equality-predicate x)
                        (comparator-hash-function x))
      (%make-hash-table x (opt-hash x o))))

(define (hash-table comparator . o)
  (let ((ht (make-hash-table comparator)))
    (let lp ((ls o))
      (when (pair? ls)
        (hash-table-set! ht (car ls) (cadr ls))
        (lp (cddr ls))))
    ht))

(define (hash-table-copy ht . o)
  (cond
   ((and (pair? o) (car o))
    (%hash-table-copy ht))
   ((hash-table-mutable? ht)
    (let ((res (%hash-table-copy ht)))
      (make-immutable! res)
      res))
   (else
    ht)))

(define (hash-table-set! ht . o)
  (let lp ((ls o))
    (when (pair? ls)
      (%hash-table-set! ht (car ls) (cadr ls))
      (lp (cddr ls)))))

(define (hash-table-fold a b c)
  (if (hash-table? a)
      (%hash-table-fold a b c)
      (%hash-table-fold c a b)))

(define (hash-table-unfold stop? mapper successor seed comparator . o)
  (let ((ht (make-hash-table comparator)))
    (let lp ((acc seed))
      (if (stop? acc)
          ht
          (call-with-values (lambda () (mapper acc))
            (lambda (key value)
              (hash-table-set! ht key value)
              (lp (successor acc))))))))

(define (alist->hash-table alist x . o)
  (if (comparator? x)
      (%alist->hash-table alist
                          (comparator-equality-predicate x)
                          (comparator-hash-function x))
      (%alist->hash-table alist x (opt-hash x o))))

(define hash-table-contains? hash-table-exists?)

(define (hash-table-empty? ht)
  (zero? (hash-table-size ht)))

(define (hash-table-mutable? ht)
  (not (immutable? ht)))

(define missing-key (list 'missing-key))

(define (hash-table=? value-cmp ht1 ht2)
  (and (= (hash-table-size ht1)
          (hash-table-size ht2))
       (let lp ((ls (hash-table-keys ht1)))
         (or (null? ls)
             (let ((v1 (hash-table-ref/default ht1 (car ls) missing-key))
                   (v2 (hash-table-ref/default ht2 (car ls) missing-key)))
               (and (not (eq? missing-key v1))
                    (not (eq? missing-key v2))
                    ((comparator-equality-predicate value-cmp) v1 v2)
                    (lp (cdr ls))))))))

(define (hash-table-intern! ht key failure)
  (hash-table-ref ht key (lambda ()
                           (let ((res (failure)))
                             (hash-table-set! ht key res)
                             res))))

(define (hash-table-delete! ht . keys)
  (for-each (lambda (key) (%hash-table-delete! ht key)) keys))

(define (hash-table-pop! ht)
  (let* ((key (car (hash-table-keys ht)))
         (value (hash-table-ref ht key)))
    (hash-table-delete! ht key)
    (values key value)))

(define (hash-table-clear! ht)
  (for-each
   (lambda (key) (hash-table-delete! ht key))
   (hash-table-keys ht)))

(define (hash-table-entries ht)
  (values (hash-table-keys ht) (hash-table-values ht)))

(define (hash-table-find proc ht failure)
  (call-with-current-continuation
   (lambda (return)
     (hash-table-for-each
      (lambda (key value)
        (let ((res (proc key value)))
          (if res (return res))))
      ht)
     (failure))))

(define (hash-table-count proc ht)
  (let ((count 0))
    (hash-table-for-each
     (lambda (key value)
       (if (proc key value)
           (set! count (+ count 1))))
     ht)
    count))

(define (hash-table-map proc cmp ht)
  (let ((ht2 (make-hash-table cmp)))
    (hash-table-for-each
     (lambda (key value) (hash-table-set! ht2 key (proc value)))
     ht)
    ht2))

(define (hash-table-map! proc ht)
  (for-each
   (lambda (key value) (hash-table-set! ht key (proc key value)))
   (hash-table-keys ht)
   (hash-table-values ht)))

(define (hash-table-for-each proc ht)
  (hash-table-walk ht proc))

(define (hash-table-map->list proc ht)
  (map (lambda (cell) (proc (car cell) (cdr cell))) (hash-table->alist ht)))

(define (hash-table-prune! proc ht)
  (for-each
   (lambda (key value)
     (if (proc key value)
         (hash-table-delete! ht key)))
   (hash-table-keys ht)
   (hash-table-values ht)))

(define (hash-table-empty-copy ht)
  (make-hash-table (hash-table-equivalence-function ht)
                   (hash-table-hash-function ht)))

(define hash-table-union! hash-table-merge!)

(define (hash-table-intersection! ht1 ht2)
  (for-each
   (lambda (key)
     (if (not (hash-table-contains? ht2 key))
         (hash-table-delete! ht1 key)))
   (hash-table-keys ht1))
  ht1)

(define (hash-table-difference! ht1 ht2)
  (for-each
   (lambda (key)
     (if (hash-table-contains? ht2 key)
         (hash-table-delete! ht1 key)))
   (hash-table-keys ht1))
  ht1)

(define (hash-table-xor! ht1 ht2)
  (let* ((tmp (hash-table-copy ht1 #t))
         (intersection (hash-table-intersection! tmp ht2)))
    (hash-table-difference! (hash-table-union! ht1 ht2)
                            intersection)
    ht1))
