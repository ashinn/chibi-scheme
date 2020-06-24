
(define-library (srfi 166 columnar)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (srfi 1)
          (srfi 117)
          (srfi 130)
          (srfi 166 base)
          (chibi optional))
  (export
   columnar tabular wrapped wrapped/list wrapped/char
   justified from-file line-numbers)
  (include "column.scm"))
