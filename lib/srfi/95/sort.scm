;; sort.scm -- SRFI-95 sorting utilities
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (copy seq)
  (if (vector? seq)
      (let* ((len (vector-length seq))
             (res (make-vector len)))
        (do ((i (- len 1) (- i 1)))
            ((< i 0) res)
          (vector-set! res i (vector-ref seq i))))
      (map (lambda (x) x) seq)))

(define (sort seq . o)
  (let ((less (and (pair? o) (car o)))
        (key (and (pair? o) (pair? (cdr o)) (car (cdr o)))))
    (sort! (copy seq) less key)))

(define (sorted? seq less . o)
  (let ((key (if (pair? o) (car o) (lambda (x) x))))
    (cond
     ((vector? seq)
      (let ((len (- (vector-length seq) 1)))
        (let lp ((i 0))
          (cond
           ((>= i len) #t)
           ((less (key (vector-ref seq (+ i 1)))
                  (key (vector-ref seq i)))
            #f)
           (else (lp (+ i 1)))))))
     ((null? seq)
      #t)
     ((pair? seq)
      (let lp ((ls1 seq) (ls2 (cdr seq)))
        (cond ((null? ls2) #t)
              ((less (key (car ls2)) (key (car ls1))) #f)
              (else (lp ls2 (cdr ls2))))))
     (else
      (error "sorted?: not a list or vector" seq)))))

(define (merge! ls1 ls2 less . o)
  (let ((key (if (pair? o) (car o) (lambda (x) x))))
    (define (lp prev ls1 ls2 a b less key)
      (cond
       ((less a b)
        (if (null? (cdr ls1))
            (set-cdr! ls1 ls2)
            (lp ls1 (cdr ls1) ls2 (key (car (cdr ls1))) b less key)))
       (else
        (set-cdr! prev ls2)
        (if (null? (cdr ls2))
            (set-cdr! ls2 ls1)
            (lp ls2 (cdr ls2) ls1 (key (car (cdr ls2))) a less key)))))
    (cond
     ((null? ls1) ls2)
     ((null? ls2) ls1)
     (else
      (let ((a (key (car ls1)))
            (b (key (car ls2))))
        (cond
         ((less a b)
          (if (null? (cdr ls1))
              (set-cdr! ls1 ls2)
              (lp ls1 (cdr ls1) ls2 (key (car (cdr ls1))) b less key))
          ls1)
         (else
          (if (null? (cdr ls2))
              (set-cdr! ls2 ls1)
              (lp ls2 (cdr ls2) ls1 (key (car (cdr ls2))) a less key))
          ls2)))))))

(define (merge ls1 ls2 less . o)
  (let ((key (if (pair? o) (car o) (lambda (x) x))))
    (merge! (copy ls1) (copy ls2) less key)))
