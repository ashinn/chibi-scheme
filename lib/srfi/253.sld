(define-library (srfi 253)
  (import (scheme base)
          (scheme case-lambda)
          (scheme list)
          (chibi assert))
  (export check-arg values-checked
          check-case
          lambda-checked define-checked
          case-lambda-checked
          define-record-type-checked)
  (include "253/impl.scm"))
