;; interface.scm -- hash-table interface
;; Copyright (c) 2009-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; the non-exported hash-table-cell is the heart of the implemenation

(define (make-hash-table . o)
  (let* ((eq-fn (if (pair? o) (car o) equal?))
         (hash-fn (if (and (pair? o) (pair? (cdr o)))
                      (car (cdr o))
                      (if (eq? eq? eq-fn) hash-by-identity hash))))
    (cond
     ((not (procedure? eq-fn))
      (error "make-hash-table: bad equivalence function" eq-fn))
     ((not (procedure? hash-fn))
      (error "make-hash-table: bad hash function" hash-fn))
     (else
      (%make-hash-table
       (make-vector 23 '())
       0
       (if (eq? hash-fn hash-by-identity) 1 (if (eq? hash-fn hash) 2 hash-fn))
       (if (eq? eq-fn eq?) 1 (if (eq? eq-fn equal?) 2 eq-fn)))))))

(define (hash-table-hash-function table)
  (let ((f (%hash-table-hash-function table)))
    (case f ((1) hash-by-identity) ((2) hash) (else f))))

(define (hash-table-equivalence-function table)
  (let ((f (%hash-table-equivalence-function table)))
    (case f ((1) eq?) ((2) equal?) (else f))))

(define-syntax assert-hash-table
  (syntax-rules ()
    ((assert-hash-table from obj)
     (if (not (hash-table? obj))
         (error (string-append from ": not a Hash-Table") obj)))))

(define (hash-table-ref table key . o)
  (assert-hash-table "hash-table-ref" table)
  (let ((cell (hash-table-cell table key #f)))
    (cond (cell (cdr cell))
          ((pair? o) ((car o)))
          (else (error "hash-table-ref: key not found" key)))))

(define (hash-table-ref/default table key default)
  (assert-hash-table "hash-table-ref/default" table)
  (let ((cell (hash-table-cell table key #f)))
    (if cell (cdr cell) default)))

(define (hash-table-set! table key value)
  (assert-hash-table "hash-table-set!" table)
  (let ((cell (hash-table-cell table key #t)))
    (set-cdr! cell value)))

(define (hash-table-exists? table key)
  (assert-hash-table "hash-table-exists?" table)
  (and (hash-table-cell table key #f) #t))

(define hash-table-update!
  (let ((not-found (cons 'not-found '())))
    (lambda (table key func . o)
      (assert-hash-table "hash-table-update!" table)
      (let ((cell (hash-table-cell table key not-found)))
        (set-cdr! cell (if (eq? not-found (cdr cell))
                           (if (pair? o)
                               (func ((car o)))
                               (error "hash-table-update!: key not found" key))
                           (func (cdr cell))))))))

(define hash-table-update!/default
  (let ((not-found (cons 'not-found '())))
    (lambda (table key func default)
      (assert-hash-table "hash-table-update!/default" table)
      (let ((cell (hash-table-cell table key not-found)))
        (set-cdr! cell (func (if (eq? not-found (cdr cell)) default (cdr cell))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hash-table-fold table kons knil)
  (assert-hash-table "hash-table-fold" table)
  (let ((vec (hash-table-buckets table)))
    (let lp1 ((i (- (vector-length vec) 1)) (acc knil))
      (if (< i 0)
          acc
          (let lp2 ((ls (vector-ref vec i)) (acc acc))
            (if (null? ls)
                (lp1 (- i 1) acc)
                (lp2 (cdr ls) (kons (car (car ls)) (cdr (car ls)) acc))))))))

(define (hash-table-walk table proc)
  (hash-table-fold table (lambda (k v a) (proc k v)) #f)
  (if #f #f))

(define (hash-table->alist table)
  (hash-table-fold table (lambda (k v a) (cons (cons k v) a)) '()))

(define (hash-table-keys table)
  (hash-table-fold table (lambda (k v a) (cons k a)) '()))

(define (hash-table-values table)
  (hash-table-fold table (lambda (k v a) (cons v a)) '()))

(define (alist->hash-table ls . o)
  (let ((res (apply make-hash-table o)))
    (for-each (lambda (x) (hash-table-set! res (car x) (cdr x))) ls)
    res))

(define (hash-table-merge! a b)
  (hash-table-walk b (lambda (k v) (hash-table-set! a k v)))
  a)

(define (hash-table-copy table)
  (assert-hash-table "hash-table-copy" table)
  (let ((res (make-hash-table (hash-table-equivalence-function table))))
    (hash-table-merge! res table)
    res))
