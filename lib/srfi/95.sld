
(define-library (srfi 95)
  (export sorted? merge merge! sort sort! object-cmp)
  (import (chibi))
  (include-shared "95/qsort")
  (include "95/sort.scm"))
