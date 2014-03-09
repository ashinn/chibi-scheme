
(define-library (chibi binary-record)
  (import (scheme base)
          (srfi 1) (srfi 9)
          (chibi io) (chibi string)
          (only (chibi) identifier? er-macro-transformer))
  (export define-binary-record-type)
  (include "binary-record.scm"))
