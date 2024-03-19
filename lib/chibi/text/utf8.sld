
(define-library (chibi text utf8)
  (export string->utf8! utf8->string! utf8-ref utf8-next utf8-prev)
  (cond-expand
   ((and chibi (not portable))
    (import (only (chibi io)
                  string->utf8! utf8->string! utf8-ref utf8-next utf8-prev)))
   (else
    (import (scheme base) (scheme bitwise))
    (include "utf8.scm")
    (begin
      (define utf8->string! utf8->string)
      (define string->utf8! string->utf8)))))
