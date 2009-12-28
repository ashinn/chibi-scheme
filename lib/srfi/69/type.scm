;; types.scm -- the hash-table record type
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-record-type hash-table
  (%make-hash-table buckets size hash-fn eq-fn)
  hash-table?
  (buckets hash-table-buckets hash-table-buckets-set!)
  (size hash-table-size hash-table-size-set!)
  (hash-fn %hash-table-hash-function)
  (eq-fn %hash-table-equivalence-function))

