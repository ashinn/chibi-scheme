
(define-library (chibi loop)
  (export loop in-list in-lists in-port in-file up-from down-from
          listing listing-reverse appending appending-reverse
          summing multiplying in-string in-string-reverse
          in-vector in-vector-reverse)
  (import (chibi))
  (include "loop/loop.scm"))

