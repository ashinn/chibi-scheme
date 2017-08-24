
(define-library (srfi 1 immutable)
  (export
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caar cadr cdar cddr car cdr
   pair? list?
   list-ref length apply map for-each member memv memq assoc assv assq
   cons list xcons cons* make-list list-tabulate list-copy circular-list iota
   proper-list? circular-list? dotted-list? not-pair? null-list? list=
   first second third fourth fifth sixth seventh eighth ninth tenth
   car+cdr take drop take-right drop-right split-at
   last last-pair length+ concatenate append-reverse append reverse
   zip unzip1 unzip2 unzip3 unzip4 unzip5 count
   fold unfold pair-fold reduce fold-right unfold-right
   pair-fold-right reduce-right
   append-map pair-for-each filter-map map-in-order
   filter partition remove find find-tail any every
   list-index take-while drop-while span break
   delete delete-duplicates
   alist-cons alist-copy alist-delete
   lset<= lset= lset-adjoin lset-union lset-intersection
   lset-difference lset-xor lset-diff+intersection)
  (import (rename (chibi)
                  (cons mcons)
                  (list mlist)
                  (reverse mreverse)
                  (append mappend)
                  (map mmap))
          (scheme cxr)
          (only (chibi ast) make-immutable!))
  (begin
    (define (cons a b)
      (let ((res (mcons a b)))
        (make-immutable! res)
        res))
    (define (list . args)
      (let lp ((ls args))
        (cond
         ((pair? ls)
          (make-immutable! ls)
          (lp (cdr ls)))))
      args)
    (define (reverse ls)
      (let lp ((ls ls) (res '()))
        (if (pair? ls)
            (lp (cdr ls) (cons (car ls) res))
            res)))
    (define (append2 ls1 ls2)
      (let lp ((ls1 (reverse ls1)) (ls2 ls2))
        (if (pair? ls1)
            (lp (cdr ls1) (cons (car ls1) ls2))
            ls2)))
    (define (append . o)
      (let lp ((lol (reverse o)) (res '()))
        (if (pair? lol)
            (lp (cdr lol) (append2 (car lol) res))
            res)))
    (define (map proc ls . lol)
      (define (map1 proc ls res)
        (if (pair? ls)
            (map1 proc (cdr ls) (cons (proc (car ls)) res))
            (reverse res)))
      (define (mapn proc lol res)
        (if (every pair? lol)
            (mapn proc
                  (map1 cdr lol '())
                  (cons (apply proc (map1 car lol '())) res))
            (reverse res)))
      (if (null? lol)
          (map1 proc ls '())
          (mapn proc (cons ls lol) '()))))
  (include "predicates.scm"
           "selectors.scm"
           "search.scm"
           "misc.scm"
           "constructors.scm"
           "fold.scm"
           "deletion.scm"
           "alists.scm"
           "lset.scm"))
