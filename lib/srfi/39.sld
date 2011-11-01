
(define-library (srfi 39)
  (export make-parameter parameterize)
  (import (scheme))
  (include-shared "39/param")
  (include "39/syntax.scm"))
