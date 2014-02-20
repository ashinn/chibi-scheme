
(define-library (chibi optimize)
  (import (chibi) (chibi ast) (chibi match) (srfi 1))
  (export register-lambda-optimization!
          replace-references
          fold-every join-seq dotted-tail)
  (include "optimize.scm"))
