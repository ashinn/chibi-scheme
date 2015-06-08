
(define-library (chibi regexp pcre)
  (export pcre->sre pcre->regexp)
  (import (scheme base) (scheme char) (scheme cxr)
          (srfi 1)
          (chibi string) (chibi regexp))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (include "pcre.scm"))
