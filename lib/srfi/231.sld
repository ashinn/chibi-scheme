(define-library (srfi 231)
  (import (scheme base)
          (scheme list)
          (scheme vector)
          (scheme sort)
          (srfi 160 base)
          (srfi 231 base)
          (chibi assert))
  (export
   ;; Miscellaneous Functions
   translation? permutation?
   ;; Indexes
   index-rotate index-first index-last index-swap
   ;; Intervals
   make-interval interval? interval-dimension interval-lower-bound
   interval-upper-bound interval-lower-bounds->list
   interval-upper-bounds->list interval-lower-bounds->vector
   interval-upper-bounds->vector interval= interval-volume
   interval-subset? interval-contains-multi-index? interval-projections
   interval-fold-left interval-fold-right
   interval-for-each interval-dilate interval-intersect
   interval-translate interval-permute
   interval-scale interval-cartesian-product
   interval-width interval-widths
   interval-empty?
   ;; Storage Classes
   make-storage-class storage-class? storage-class-getter
   storage-class-setter storage-class-checker storage-class-maker
   storage-class-copier storage-class-length storage-class-default
   generic-storage-class s8-storage-class s16-storage-class
   s32-storage-class s64-storage-class u1-storage-class
   u8-storage-class u16-storage-class u32-storage-class
   u64-storage-class f8-storage-class f16-storage-class
   f32-storage-class f64-storage-class
   c64-storage-class c128-storage-class
   char-storage-class
   storage-class-data? storage-class-data->body
   ;; Arrays
   make-array array? array-domain array-getter array-dimension
   array-empty?
   mutable-array? array-setter specialized-array-default-safe?
   specialized-array-default-mutable? make-specialized-array
   make-specialized-array-from-data
   specialized-array? array-storage-class array-indexer array-body
   array-safe? array-packed? specialized-array-share
   array-copy array-curry array-extract array-tile array-translate
   array-permute array-reverse array-sample
   array-outer-product array-map array-for-each array-fold-left
   array-fold-right array-reduce array-any array-every
   array-inner-product array-stack array-append array-block
   array->list list->array array->vector vector->array
   array->list* list*->array array->vector* vector*->array
   array-assign! array-ref array-set! array-decurry
   specialized-array-reshape
   array-copy! array-stack! array-decurry! array-append! array-block!
   array-freeze!
   )
  (include "231/transforms.scm")
  (cond-expand
   ((and chibi (library (srfi 160 mini)))
    (import (srfi 160 mini))
    (begin
      (define-storage-class f8-storage-class
        f8vector-ref f8vector-set! f8? make-f8vector f8vector-length 0)
      (define-storage-class f16-storage-class
        f16vector-ref f16vector-set! f16? make-f16vector f16vector-length 0)))
   (else
    (begin
      (define f8-storage-class f32-storage-class)
      (define f16-storage-class f32-storage-class))))
  )
