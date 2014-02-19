
(define-library (chibi optimize rest)
  (export optimize-rest rest-parameter-cdrs num-parameters local-ref)
  (import (chibi) (srfi 1) (chibi ast) (chibi match) (chibi optimize))
  (include-shared "rest")
  (include "rest.scm"))
