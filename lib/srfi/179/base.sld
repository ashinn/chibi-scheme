
;;> The base array definitions of SRFI 179, plus some extra internal
;;> bindings.

(define-library (srfi 179 base)
  (import (scheme base)
          (scheme list)
          (scheme vector)
          (chibi assert))
  (export
   ;; Miscellaneous Functions
   translation? permutation?
   ;; Intervals
   make-interval interval? interval-dimension interval-lb interval-ub
   interval-lower-bound interval-upper-bound interval-lower-bounds->list
   interval-upper-bounds->list interval-lower-bounds->vector
   interval-upper-bounds->vector interval= interval-volume
   interval-subset? interval-contains-multi-index? interval-projections
   interval-for-each interval-dilate interval-intersect
   interval-translate interval-permute interval-rotate
   interval-scale interval-cartesian-product
   ;; Indexing
   indexer->coeffs coeffs->indexer default-indexer default-coeffs
   invert-default-index interval-cursor interval-cursor-next!
   interval-cursor-next interval-cursor-get interval-fold
   ;; Storage Classes
   make-storage-class storage-class? storage-class-getter
   storage-class-setter storage-class-checker storage-class-maker
   storage-class-copier storage-class-length storage-class-default
   generic-storage-class
   ;; Arrays
   make-array array? array-domain array-getter array-dimension
   mutable-array? array-setter specialized-array-default-safe?
   specialized-array-default-mutable? make-specialized-array
   specialized-array? array-storage-class array-indexer array-body
   array-safe? array-coeffs array-adjacent? array-elements-in-order?
   specialized-array-share array-ref array-set!
   %make-specialized %array-setter-set!
   specialized-getter specialized-setter
   )
  (include "base.scm"))
