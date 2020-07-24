
(define-library (chibi trace)
  (export trace untrace untrace-all trace-cell untrace-cell)
  (import (chibi) (chibi ast) (srfi 38) (srfi 39) (srfi 69))
  (include "trace.scm"))
