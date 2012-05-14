;; lset.scm -- list set library
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (lset<= eq . sets)
  (if (null? sets)
      #t
      (let lp1 ((set1 (car sets)) (sets (cdr sets)))
        (if (null? sets)
            #t
            (let ((set2 (car sets)))
              (let lp2 ((ls set1))
                (if (pair? ls)
                    (and (member (car set1) set2 eq) (lp2 (cdr ls)))
                    (lp1 set2 (cdr sets)))))))))

(define (lset= eq . sets)
  (and (apply lset<= eq sets) (apply lset<= eq (reverse sets))))

(define (lset-adjoin eq set . elts)
  (lset-union2 eq set elts))

(define (lset-union2 eq a b)
  (if (null? b)
      a
      (lset-union2 eq (if (member (car b) a eq) a (cons (car b) a)) (cdr b))))

(define (lset-union eq . sets)
  (reduce (lambda (a b) (lset-union2 eq b a)) '() sets))

(define (lset-intersection eq . sets)
  (reduce (lambda (a b) (filter (lambda (x) (member x a eq)) b)) '() sets))

(define (lset-diff2 eq a b)
  (remove (lambda (x) (member x a eq)) b))

(define (lset-difference eq . sets)
  (reduce (lambda (a b) (lset-diff2 eq a b)) '() sets))

(define (lset-xor eq . sets)
  (reduce (lambda (a b) (append (lset-diff2 eq a b) (lset-diff2 eq b a)))
          '()
          sets))

(define (lset-diff+intersection eq . sets)
  (values (apply lset-difference eq sets) (apply lset-intersection eq sets)))

(define lset-diff+intersection! lset-diff+intersection)
(define lset-xor! lset-xor)
(define lset-difference! lset-difference)
(define lset-intersection! lset-intersection)
(define lset-union! lset-union)
