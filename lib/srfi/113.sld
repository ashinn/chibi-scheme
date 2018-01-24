
(define-library (srfi 113)
  (import (scheme base) (srfi 1) (srfi 125) (srfi 128))
  (export
   ;;;;;;;;;;;;;; Sets
   ;; Constructors:
   set set-contains? set-unfold
   ;; Predicates:
   set? set-empty? set-disjoint?
   ;; Accessors:
   set-member set-element-comparator
   ;; Updaters:
   set-adjoin set-adjoin! set-replace set-replace!
   set-delete set-delete! set-delete-all set-delete-all!
   set-search!
   ;; The whole set:
   set-size set-find set-count set-any? set-every?
   ;; Mapping and folding:
   set-map set-for-each set-fold set-filter set-filter!
   set-remove set-remove! set-partition set-partition!
   ;; Copying and conversion:
   set-copy set->list list->set list->set!
   ;; Subsets:
   set=? set<? set>? set<=? set>=?
   ;; Set theory operations:
   set-union set-intersection set-difference set-xor
   set-union! set-intersection! set-difference! set-xor!
   ;; Comparators:
   (rename the-set-comparator set-comparator)

   ;;;;;;;;;;;;;; Bags
   ;; Constructors:
   bag bag-contains? bag-unfold
   ;; Predicates:
   bag? bag-empty? bag-disjoint?
   ;; Accessors:
   bag-member bag-element-comparator
   ;; Updaters:
   bag-adjoin bag-adjoin! bag-replace bag-replace!
   bag-delete bag-delete! bag-delete-all bag-delete-all!
   bag-search!
   ;; The whole bag:
   bag-size bag-find bag-count bag-any? bag-every?
   ;; Mapping and folding:
   bag-map bag-for-each bag-fold bag-filter bag-filter!
   bag-remove bag-remove! bag-partition bag-partition!
   ;; Copying and conversion:
   bag-copy bag->list list->bag list->bag!
   ;; Subbags:
   bag=? bag<? bag>? bag<=? bag>=?
   ;; Bag theory operations:
   bag-union bag-intersection bag-difference bag-xor
   bag-union! bag-intersection! bag-difference! bag-xor!
   ;; Comparators:
   (rename the-bag-comparator bag-comparator)

   ;; Additional bag procedures:
   bag-unique-size
   bag-sum bag-sum! bag-product bag-product! bag-element-count
   bag-for-each-unique bag-fold-unique bag-increment! bag-decrement!
   bag->set set->bag set->bag!
   bag->alist alist->bag
   )
  ;;(include-shared "69/hash")
  (include "113/sets.scm"
           "113/bags.scm"))
