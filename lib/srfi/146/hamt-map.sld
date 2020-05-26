(define-library (srfi 146 hamt-map)
  (import
   (scheme base)
   (scheme case-lambda)
   (only (srfi 1) fold)
   (srfi 16)
   (srfi 146 hamt)
   (srfi 146 hamt-misc))
  (export
   make-phm phm?
   phm->alist
   phm/add-alist phm/add-alist!
   phm/contains?
   phm/count
   phm/empty?
   phm/for-each
   phm/get
   phm/immutable
   phm/keys
   phm/mutable phm/mutable?
   phm/put
   phm/put!
   phm/remove phm/remove!
   phm/replace phm/replace!

   ;; This is only needed by tests:
   phm/data)
  (include "hamt-map.scm"))
