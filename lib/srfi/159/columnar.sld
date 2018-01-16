
(define-library (srfi 159 columnar)
  (import (chibi show column))
  (export
   call-with-output-generator call-with-output-generators
   string->line-generator
   tabular columnar show-columns wrapped wrapped/list wrapped/char
   justified line-numbers from-file))
