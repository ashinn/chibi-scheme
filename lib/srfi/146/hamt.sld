(define-library (srfi 146 hamt)
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 1) find-tail)
          (srfi 16)
          (only (srfi 143) fx-width)
          (srfi 151)
          (srfi 146 hamt-misc)
          (srfi 146 vector-edit))
  (export fragment->mask
          hamt->list
          hamt-fetch
          hamt-null
          hamt-null?
          hamt/count
          hamt/empty?
          hamt/for-each
          hamt/immutable
          hamt/mutable
          hamt/mutable?
          hamt/payload?
          hamt/put
          hamt/put!
          hamt/replace
          hamt/replace!
          hash-array-mapped-trie?
          make-hamt

          ;; These are only needed by tests:
          collision?
          hamt-bucket-size
          hamt-hash-size
          hamt/root
          leaf-stride
          narrow/array
          narrow/leaves
          narrow?
          next-set-bit
          wide/array
          wide/children
          wide?)
  (include "hamt.scm"))
