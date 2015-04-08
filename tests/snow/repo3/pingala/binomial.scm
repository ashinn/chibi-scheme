(define-library (pingala binomial)
  (export binomial)
  (import (scheme base) (pingala factorial))
  (include "binomial-impl.scm"))
