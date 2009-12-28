;; search.scm -- list searching and splitting
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (find pred ls)
  (cond ((find-tail pred ls) => car) (else #f)))

(define (find-tail pred ls)
  (and (pair? ls) (if (pred (car ls)) ls (find-tail pred (cdr ls)))))

(define (take-while pred ls)
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pred (car ls)))
        (lp (cdr ls) (cons (car ls) res))
        (reverse! res))))

(define take-while! take-while)

(define (drop-while pred ls)
  (or (find-tail (lambda (x) (not (pred x))) ls) '()))

(define (span pred ls)
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pred (car ls)))
        (lp (cdr ls) (cons (car ls) res))
        (values (reverse! res) ls))))

(define span! span)

(define (break pred ls) (span (lambda (x) (not (pred x))) ls))

(define break! break)

(define (any pred ls . lists)
  (if (null? lists)
      (let lp ((ls ls)) (and (pair? ls) (if (pred (car ls)) (car ls) (lp (cdr ls)))))
      (let lp ((lists (cons ls lists)))
        (and (every pair? lists)
             (let ((args (map car lists)))
               (if (apply pred args) args (lp (map cdr lists))))))))

(define (every pred ls . lists)
  (if (null? lists)
      (let lp ((ls ls)) (if (pair? ls) (and (pred (car ls)) (lp (cdr ls))) #t))
      (not (apply any (lambda (x) (not (pred x))) ls lists))))

(define (list-index pred ls . lists)
  (if (null? lists)
      (let lp ((ls ls) (n 0))
        (and (pair? ls) (if (pred (car ls)) n (lp (cdr ls) (+ n 1)))))
      (let lp ((lists (cons ls lists)) (n 0))
        (and (every pair? lists)
             (if (apply pred (map car lists)) n (lp (map cdr lists) (+ n 1)))
             ))))
