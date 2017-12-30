
(define-library (chibi show pretty)
  (export pretty pretty-shared pretty-simply
          joined/shares try-fitted
          )
  (import (scheme base) (scheme write) (chibi show) (chibi show base)
          (srfi 1) (srfi 69) (chibi string))
  (include "pretty.scm"))
