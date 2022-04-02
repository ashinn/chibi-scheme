(define-library (srfi 213)
  (import (chibi)
          (chibi ast)
          (scheme case-lambda)
          (scheme comparator)
          (scheme hash-table))
  (export define-property
          capture-lookup)
  (include "213.scm"))
