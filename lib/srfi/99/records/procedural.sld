
(define-library (srfi 99 records procedural)
  (export make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator)
  (import (chibi)
          (chibi ast)
          (only (srfi 1) iota)
          (srfi 99 records inspection))
  (include "procedural.scm"))
