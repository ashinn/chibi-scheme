
(define-library (chibi text search)
  (import (scheme base)
          (chibi regexp)
          (chibi text base)
          (chibi text types)
          (chibi text utf8)
          (srfi 130))
  (cond-expand
   (chibi
    (import (only (chibi) string-cursor-offset)))
   (else
    ;; assume cursors are indexes
    (begin
      (define (string-cursor-offset sc) sc))))
  (export text-search!)
  (include "search.scm"))
