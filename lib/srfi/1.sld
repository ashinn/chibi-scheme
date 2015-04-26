
(define-library (srfi 1)
  (export
   xcons cons* make-list list-tabulate list-copy circular-list iota
   proper-list? circular-list? dotted-list? not-pair? null-list? list=
   first second third fourth fifth sixth seventh eighth ninth tenth
   car+cdr take drop take-right drop-right take! drop-right! split-at split-at!
   last last-pair length+ concatenate append! concatenate! reverse!
   append-reverse append-reverse!
   zip unzip1 unzip2 unzip3 unzip4 unzip5 count
   fold unfold pair-fold reduce fold-right unfold-right
   pair-fold-right reduce-right
   append-map append-map! map! pair-for-each filter-map map-in-order
   filter partition remove filter! partition! remove! find find-tail any every
   list-index take-while drop-while take-while! span break span! break!
   delete delete-duplicates delete! delete-duplicates!
   alist-cons alist-copy alist-delete alist-delete!
   lset<= lset= lset-adjoin lset-union lset-union! lset-intersection
   lset-intersection! lset-difference lset-difference! lset-xor lset-xor!
   lset-diff+intersection lset-diff+intersection!)
  (cond-expand
   (chibi
    (import (chibi)))
   (else
    (import (scheme base))
    (begin
      (define reverse! reverse)
      (define (find-tail pred ls)
        (and (pair? ls) (if (pred (car ls)) ls (find-tail pred (cdr ls)))))
      (define (find pred ls)
        (cond ((find-tail pred ls) => car) (else #f)))
      (define (any pred ls . lol)
        (define (any1 pred ls)
          (if (pair? (cdr ls))
              ((lambda (x) (if x x (any1 pred (cdr ls)))) (pred (car ls)))
              (pred (car ls))))
        (define (anyn pred lol)
          (if (every pair? lol)
              ((lambda (x) (if x x (anyn pred (map cdr lol))))
               (apply pred (map car lol)))
              #f))
        (if (null? lol)
            (if (pair? ls) (any1 pred ls) #f)
            (anyn pred (cons ls lol))))
      (define (every pred ls . lol)
        (define (every1 pred ls)
          (if (null? (cdr ls))
              (pred (car ls))
              (if (pred (car ls)) (every1 pred (cdr ls)) #f)))
        (if (null? lol)
            (if (pair? ls) (every1 pred ls) #t)
            (not (apply any (lambda xs (not (apply pred xs))) ls lol))))
      )))
  (include "1/predicates.scm"
           "1/selectors.scm"
           "1/search.scm"
           "1/misc.scm"
           "1/constructors.scm"
           "1/fold.scm"
           "1/deletion.scm"
           "1/alists.scm"
           "1/lset.scm"))
