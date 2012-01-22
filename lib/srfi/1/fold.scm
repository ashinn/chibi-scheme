;; fold.scm -- list fold/reduce utilities
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (fold kons knil ls . lists)
  (if (null? lists)
      (let lp ((ls ls) (acc knil))
        (if (pair? ls) (lp (cdr ls) (kons (car ls) acc)) acc))
      (let lp ((lists (cons ls lists)) (acc knil))
        (if (every pair? lists)
            (lp (map cdr lists) (apply kons (map-onto car lists (list acc))))
            acc))))

(define (fold-right kons knil ls . lists)
  (if (null? lists)
      (let lp ((ls ls))
        (if (pair? ls) (kons (car ls) (lp (cdr ls))) knil))
      (let lp ((lists (cons ls lists)))
        (if (every pair? lists)
            (apply kons (map-onto car lists (list (lp (map cdr lists)))))
            knil))))

(define (pair-fold kons knil ls . lists)
  (if (null? lists)
      (let lp ((ls ls) (acc knil))
        (if (pair? ls) (lp (cdr ls) (kons ls acc)) acc))
      (let lp ((lists (cons ls lists)) (acc knil))
        (if (every pair? lists)
            (lp (map cdr lists) (apply kons (append lists (list acc))))
            acc))))

(define (pair-fold-right kons knil ls . lists)
  (if (null? lists)
      (let lp ((ls ls))
        (if (pair? ls) (kons ls (lp (cdr ls))) knil))
      (let lp ((lists (cons ls lists)))
        (if (every pair? lists)
            (apply kons (append lists (list (lp (map cdr lists)))))
            knil))))

(define (reduce f identity ls)
  (if (null? ls) identity (fold f (car ls) (cdr ls))))

(define (reduce-right f identity ls)
  (if (null? ls) identity (fold-right f (car ls) (cdr ls))))

(define (unfold p f g seed . o)
  (let lp ((seed seed))
    (if (p seed)
        (if (pair? o) ((car o) seed) '())
        (cons (f seed) (lp (g seed))))))

(define (unfold-right p f g seed . o)
  (let lp ((seed seed) (res (if (pair? o) (car o) '())))
    (if (p seed) res (lp (g seed) (cons (f seed) res)))))

(define (append-map-helper append f ls lists)
  (if (null? lists)
      (if (null? ls)
          '()
          (let lp ((ls (reverse ls)) (res '()))
            (if (null? ls) res (lp (cdr ls) (append (f (car ls)) res)))))
      (if (and (pair? ls) (every pair? lists))
          (let lp ((lists (cons ls lists)))
            (let ((vals (apply f (map car lists)))
                  (cdrs (map cdr lists)))
              (if (every pair? cdrs) (append vals (lp cdrs)) vals)))
          '())))

(define (append-map f ls . lists)
  (append-map-helper append f ls lists))

(define (append-map! f ls . lists)
  (append-map-helper append! f ls lists))

(define map! map)
(define map-in-order map)

(define (pair-for-each f ls . lists)
  (apply pair-fold (lambda (x _) (f x)) #f ls lists))

(define (filter-map f ls . lists)
  (if (null? lists)
      (let lp ((ls ls) (res '()))
        (if (pair? ls)
            (let ((x (f (car ls)))) (lp (cdr ls) (if x (cons x res) res)))
            (reverse! res)))
      (filter (lambda (x) x) (apply map f ls lists))))

(define (take-up-to-reverse from to init)
  (if (eq? from to)
      init
      (take-up-to-reverse (cdr from) to (cons (car from) init))))

(define (remove pred ls)
  (let lp ((ls ls) (rev '()))
    (let ((tail (find-tail pred ls)))
      (if tail
          (lp (cdr tail) (take-up-to-reverse ls tail rev))
          (if (pair? rev) (append-reverse! rev ls) ls)))))

(define (filter pred ls) (remove (lambda (x) (not (pred x))) ls))

(define (partition pred ls)
  (let lp ((ls ls) (good '()) (bad '()))
    (cond ((null? ls) (values (reverse! good) (reverse! bad)))
          ((pred (car ls)) (lp (cdr ls) (cons (car ls) good) bad))
          (else (lp (cdr ls) good (cons (car ls) bad))))))

(define filter! filter)
(define remove! remove)
(define partition! partition)

