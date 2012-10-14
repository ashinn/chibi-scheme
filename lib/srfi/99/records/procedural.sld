
(define-library (srfi 99 records procedural)
  (export make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator)
  (import (chibi) (chibi ast) (srfi 99 records inspection))
  (include "procedural.scm"))
