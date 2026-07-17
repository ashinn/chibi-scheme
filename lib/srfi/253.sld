(define-library (srfi 253)
  (import (scheme base)
          (scheme case-lambda)
          (scheme list)
          (chibi assert)
          (chibi ast))
  (export check-impl?
          check-arg values-checked
          check-case
          lambda-checked define-checked
          case-lambda-checked
          define-record-type-checked)
  (include "253/impl.scm"))
