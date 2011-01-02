;; predicates.scm -- list prediates
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (proper-list? x)
  (cond ((null? x) #t)
        ((pair? x) (proper-list? (cdr x)))
        (else #f)))

(define (circular-list? x)
  (and (pair? x) (pair? (cdr x))
       (let race ((hare (cdr x)) (tortoise x))
         (or (eq? hare tortoise)
             (and (pair? hare) (pair? (cdr hare))
                  (race (cddr hare) (cdr tortoise)))))))

(define (dotted-list? x)
  (not (proper-list? x)))

(define (not-pair? x) (not (pair? x)))

(define (null-list? x) (null? x)) ; no error

(define (list= eq . lists)
  (let lp1 ((lists lists))
    (or (null? lists)
        (null? (cdr lists))
        (let lp2 ((ls1 (car lists)) (ls2 (cadr lists)))
          (if (null? ls1)
              (and (null? ls2)
                   (lp1 (cdr lists)))
              (and (eq (car ls1) (car ls2))
                   (lp2 (cdr ls1) (cdr ls2))))))))

(define (length+ x)
  (if (not (pair? x))
      0
      (let lp ((hare (cdr x)) (tortoise x) (res 1))
        (and (not (eq? hare tortoise))
             (if (pair? hare)
                 (if (not (pair? (cdr hare)))
                     (+ res 1)
                     (lp (cddr hare) (cdr tortoise) (+ res 2)))
                 res)))))
