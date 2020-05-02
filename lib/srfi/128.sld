(define-library (srfi 128)
  (import (scheme base) (scheme char)
          (srfi 27) (srfi 69) (srfi 95) (srfi 98) (srfi 151)
          (only (chibi) fixnum? er-macro-transformer)
          (only (chibi ast) opcode? procedure? procedure-arity procedure-variadic?))
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
   comparator-if<=>
   ;;SRFI 162:
   comparator-max comparator-min
   comparator-max-in-list comparator-min-in-list
   default-comparator boolean-comparator real-comparator
   char-comparator char-ci-comparator
   string-comparator string-ci-comparator
   list-comparator vector-comparator
   eq-comparator eqv-comparator equal-comparator)
  (include "128/comparators.scm")
  (include "128/162-impl.scm")
  )
