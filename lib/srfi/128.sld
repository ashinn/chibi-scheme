(define-library (srfi 128)
  (import (scheme base) (scheme char)
          (srfi 27) (srfi 33) (srfi 69) (srfi 95) (srfi 98)
          (only (chibi) fixnum? er-macro-transformer))
  (export
   ;; Predicates:
   comparator? comparator-ordered? comparator-hashable?
   ;; Constructors:
   make-comparator make-pair-comparator make-list-comparator
   make-vector-comparator make-eq-comparator make-eqv-comparator
   make-equal-comparator
   ;; Standard hash functions:
   boolean-hash char-hash char-ci-hash string-hash string-ci-hash
   symbol-hash number-hash
   ;; Bounds and salt:
   hash-bound hash-salt
   ;; Default comparators:
   make-default-comparator default-hash comparator-register-default!
   ;; Accessors and invokers:
   comparator-type-test-predicate comparator-equality-predicate
   comparator-ordering-predicate comparator-hash-function
   comparator-test-type comparator-check-type comparator-hash
   ;; Comparison predicates:
   =? <? >? <=? >=?
   ;;Syntax:
   comparator-if<=>)
  (include "128/comparators.scm"))
