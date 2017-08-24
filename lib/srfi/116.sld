
(define-library (srfi 116)
  (export
   ;; Syntax
   iq

   ;; Constructors
   ipair ilist
   xipair ipair* make-ilist ilist-tabulate  
   ilist-copy iiota

   ;; Predicates
   ipair? proper-ilist? ilist? dotted-ilist? 
   not-ipair? null-ilist?
   ilist=

   ;; Selectors
   icar icdr icaar icadr icdar icddr
   icaaaar icaaadr icaadar icaaddr
   icadaar icadadr icaddar icadddr
   icdaaar icdaadr icdadar icdaddr
   icddaar icddadr icdddar icddddr
   icaaar icaadr icadar icaddr
   icdaar icdadr icddar icdddr
   ilist-ref
   ifirst isecond ithird ifourth ififth isixth iseventh ieighth ininth itenth
   icar+icdr
   itake       idrop ilist-tail
   itake-right idrop-right
   isplit-at   
   ilast last-ipair

   ;; Miscellaneous: length, append, concatenate, reverse, zip & count
   ilength 
   iappend  iconcatenate  ireverse  iappend-reverse
   izip iunzip1 iunzip2 iunzip3 iunzip4 iunzip5
   icount

   ;; Fold, unfold & map
   imap        ifor-each
   ifold       iunfold        ipair-fold       ireduce 
   ifold-right iunfold-right  ipair-fold-right ireduce-right 
   iappend-map ipair-for-each ifilter-map      imap-in-order

   ;; Filtering & partitioning
   ifilter  ipartition  iremove

   ;; Searching
   imember imemq imemv
   ifind         ifind-tail 
   iany ievery
   ilist-index
   itake-while   idrop-while
   ispan ibreak

   ;; Deleting
   idelete       idelete-duplicates 

   ;; Immutable association lists
   iassoc iassq iassv
   ialist-cons  ialist-delete

   ;; Replacement
   replace-icar replace-icdr

   ;; Conversion
   pair->ipair  ipair->pair
   list->ilist  ilist->list
   tree->itree  itree->tree
   gtree->itree gtree->tree

   ;; Procedure application
   iapply

   ;; Comparators
   ipair-comparator        ilist-comparator
   make-ilist-comparator   make-improper-ilist-comparator
   make-icar-comparator    make-icdr-comparator)

  (import (scheme base)
          (rename (prefix (srfi 1 immutable) i)
                  (imake-list make-ilist)
                  (icar+cdr icar+icdr)
                  (ilast-pair last-ipair)
                  (icons ipair)
                  (ixcons xipair)
                  (icons* ipair*)
                  (inull-list? null-ilist?)
                  (idotted-list? dotted-ilist?)
                  (iproper-list? proper-ilist?))
          (rename (srfi 128)
                  (make-pair-comparator make-ipair-comparator)
                  (make-list-comparator make-ilist-comparator)))

  (begin
    (define-syntax iq
      (syntax-rules ()
        ((iq x ...)
         (ilist 'x ...))))
    (define (pair->ipair x)
      (ipair (car x) (cdr x)))
    (define (ipair->pair x)
      (cons (car x) (cdr x)))
    (define (not-ipair? x)
      (not (ipair? x)))
    (define (replace-icar x obj)
      (ipair obj (cdr x)))
    (define (replace-icdr x obj)
      (ipair (car x) obj))
    (define ilist-tail idrop)
    (define list->ilist ilist-copy)
    (define ilist->list list-copy)
    (define (tree->itree x)
      (if (pair? x)
          (ipair (tree->itree (car x)) (tree->itree (cdr x)))
          x))
    (define (itree->tree x)
      (if (ipair? x)
          (cons (itree->tree (car x)) (itree->tree (cdr x)))
          x))
    (define gtree->itree tree->itree)
    (define gtree->tree itree->tree)

    (define ipair-comparator (make-default-comparator))
    (define ilist-comparator (make-default-comparator))
    (define make-improper-ilist-comparator make-ilist-comparator)
    (define (make-field-comparator comparator pred field)
      (make-comparator
       (lambda (x)
         (and (pred x)
              ((comparator-type-test-predicate comparator) (field x))))
       (lambda (x y)
         ((comparator-equality-predicate comparator) (field x) (field y)))
       (lambda (x y)
         ((comparator-ordering-predicate comparator) (field x) (field y)))
       (lambda (x)
         ((comparator-hash-function comparator) (field x)))))
    (define (make-icar-comparator comparator)
      (make-field-comparator comparator ipair? icar))
    (define (make-icdr-comparator comparator)
      (make-field-comparator comparator ipair? icdr))
    ))
