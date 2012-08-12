
(define-library (srfi 38)
  (import (scheme) (srfi 69) (chibi ast))
  (export write-with-shared-structure write/ss
          read-with-shared-structure read/ss)
  (include "38.scm"))
