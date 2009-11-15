
(define-record-type hash-table
  (%make-hash-table buckets size hash-fn eq-fn)
  hash-table?
  (buckets hash-table-buckets hash-table-buckets-set!)
  (size hash-table-size hash-table-size-set!)
  (hash-fn %hash-table-hash-function)
  (eq-fn %hash-table-equivalence-function))

