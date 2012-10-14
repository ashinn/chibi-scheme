
(define-library (srfi 39)
  (export make-parameter parameterize)
  (import (chibi))
  (include-shared "39/param")
  (cond-expand
   (threads
    (include "39/syntax.scm"))
   (else
    (include "39/syntax-no-threads.scm"))))
