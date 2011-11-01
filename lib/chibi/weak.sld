
;;> Library for weak data structures.

(define-library (chibi weak)
  (export make-ephemeron ephemeron? ephemeron-broken?
          ephemeron-key ephemeron-value
          make-weak-vector weak-vector? weak-vector-length
          weak-vector-ref weak-vector-set!)
  (include-shared "weak"))
