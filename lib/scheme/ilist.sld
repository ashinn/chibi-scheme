
(define-library (scheme ilist)
  (import (srfi 116))
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
   make-icar-comparator    make-icdr-comparator))
