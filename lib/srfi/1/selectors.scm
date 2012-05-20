;; selectors.scm -- extended list selectors
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define first car)
(define second cadr)
(define (third ls)   (car (cdr (cdr ls))))
(define (fourth ls)  (car (cdr (cdr (cdr ls)))))
(define (fifth ls)   (car (cdr (cdr (cdr (cdr ls))))))
(define (sixth ls)   (car (cdr (cdr (cdr (cdr (cdr ls)))))))
(define (seventh ls) (car (cdr (cdr (cdr (cdr (cdr (cdr ls))))))))
(define (eighth ls)  (car (cdr (cdr (cdr (cdr (cdr (cdr ls))))))))
(define (ninth ls)   (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr ls)))))))))
(define (tenth ls)   (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr ls))))))))))

(define (car+cdr x) (values (car x) (cdr x)))

(define (take ls i)
  (let lp ((i i) (ls ls) (res '()))
    (if (<= i 0)
        (reverse! res)
        (lp (- i 1) (cdr ls) (cons (car ls) res)))))

(define (take! ls i)
  (if (<= i 0)
      '()
      (let ((tail (list-tail ls (- i 1))))
        (set-cdr! tail '())
        ls)))

(define (drop ls i)
  (if (<= i 0) ls (drop (cdr ls) (- i 1))))

(define (take-right ls i)
  (drop ls (- (length+ ls) i)))

(define (drop-right ls i)
  (take ls (- (length+ ls) i)))

(define (drop-right! ls i)
  (take! ls (- (length+ ls) i)))

(define (split-at ls i)
  (let lp ((i i) (ls ls) (res '()))
    (if (<= i 0)
        (values (reverse! res) ls)
        (lp (- i 1) (cdr ls) (cons (car ls) res)))))

(define (split-at! ls i)
  (if (<= i 0)
      (values '() ls)
      (let* ((tail (list-tail ls (- i 1)))
             (right (cdr tail)))
        (set-cdr! tail '())
        (values ls right))))

(define (last ls) (if (null? (cdr ls)) (car ls) (last (cdr ls))))
(define (last-pair ls) (if (null? (cdr ls)) ls (last-pair (cdr ls))))

