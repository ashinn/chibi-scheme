;; base.scm - base integer set operations
;; Copyright (c) 2004-2012 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; An integer set (iset) is a set of exact integers optimized for
;; minimal space usage and fast membership lookup.  General set
;; operations are provided based on the character set operations found
;; in SRFI-14.
;;
;; Creating isets:
;;
;; (make-iset)     ; an empty integer set
;; (make-iset n)   ; a set of the single integer N
;; (make-iset n m) ; a set of the range of all integers from N-M inclusive
;;
;; The following procedures are provided as direct analogs of the
;; SRFI-14 procedures, accepting and returning isets and integers in
;; place of char-sets and characters:
;;
;; Creating isets:
;;
;; (iset-copy is)            ; a new copy of IS
;; (iset n ...)              ; an iset containing the elements N...
;; (list->iset ls [base-is]) ; an iset containing all the integers in
;;                           ; list LS, union BASE-IS if provided
;; (list->iset! ls base-is)  ; same as above, allowed but not required to
;;                           ; modify base-is
;;
;; Querying isets:
;;
;; (iset-size is)          ; return the # of elements in IS
;; (iset-contains? is n)   ; test N for membership in IS
;; (iset->list is)         ; returns a list of all integers in IS
;;
;; Predicates:
;;
;; (iset? obj)     ; #t iff obj is an integer set
;; (iset= is ...)  ; #t iff all arguments are equivalent integer sets
;; (iset<= is ...) ; #t iff the arguments are monotonically increasing sets
;; (iset>= is ...) ; #t iff the arguments are monotonically decreasing sets
;;
;; Cursors:
;;
;; (iset-cursor iset)
;; (iset-ref iset cursor)
;; (iset-cursor-next iset cursor)
;; (end-of-iset? iset)
;;
;; Set operations:
;;
;; (iset-adjoin is n ...)         ; char-set-adjoin
;; (iset-delete is n ...)         ; char-set-delete
;;
;; (iset-adjoin! is n ...)        ; char-set-adjoin!
;; (iset-delete! is n ...)        ; char-set-delete!
;;
;; (iset-union is1 ...)                  ; char-set-union
;; (iset-intersection is1 ...)           ; char-set-intersection
;; (iset-difference is1 is2 ...)         ; char-set-difference
;;
;; (iset-union! is1 ...)                 ; char-set-union!
;; (iset-intersection! is1 ...)          ; char-set-intersection!
;; (iset-difference! is1 is2 ...)        ; char-set-difference!

(define-library (chibi iset)
  (import (chibi iset base)
          (chibi iset iterators)
          (chibi iset constructors))
  (export
   %make-iset make-iset iset? iset-contains? Integer-Set
   iset iset-copy list->iset list->iset! iset-map
   iset-adjoin iset-adjoin! iset-delete iset-delete!
   iset-union iset-union! iset-intersection iset-intersection!
   iset-difference iset-difference!
   iset-empty? iset-fold iset-fold-node iset-for-each iset-for-each-node
   iset-map iset->list iset-size iset= iset<= iset>=
   iset-cursor iset-cursor? iset-cursor-next iset-ref end-of-iset?))
