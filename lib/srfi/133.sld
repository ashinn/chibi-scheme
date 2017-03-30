(define-library (srfi 133)
  (import (scheme base))
  (export
   ;; Constructors
   make-vector vector 
   vector-unfold vector-unfold-right 
   vector-copy vector-reverse-copy 
   vector-append vector-concatenate vector-append-subvectors 
   ;; Predicates 
   vector? 
   vector-empty? 
   vector= 
   ;; Selectors 
   vector-ref 
   vector-length 
   ;; Iteration 
   vector-fold vector-fold-right 
   vector-map vector-map! 
   vector-for-each vector-count 
   vector-cumulate 
   ;; Searching 
   vector-index vector-index-right 
   vector-skip vector-skip-right 
   vector-binary-search 
   vector-any vector-every 
   vector-partition 
   ;; Mutators 
   vector-set! vector-swap! 
   vector-fill! vector-reverse! 
   vector-copy! vector-reverse-copy! 
   vector-unfold! vector-unfold-right! 
   ;; Conversion 
   vector->list reverse-vector->list 
   list->vector reverse-list->vector 
   vector->string string->vector)
  (include "133/vector.scm"))
