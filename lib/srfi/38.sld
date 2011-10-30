
(define-library (srfi 38)
  (import (scheme) (chibi ast))
  (export write-with-shared-structure write/ss
          read-with-shared-structure read/ss)
  (include "38.scm"))
