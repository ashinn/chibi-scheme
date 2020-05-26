(define-library (srfi 146 vector-edit-test)
  (import (scheme base) (chibi test) (srfi 146 vector-edit))
  (export run-vector-edit-tests)
  (include "vector-edit-test.scm"))
