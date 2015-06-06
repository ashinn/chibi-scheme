
(define-library (chibi regexp pcre)
  (export pcre->sre pcre->regexp)
  (import (scheme base) (scheme char) (scheme cxr)
          (srfi 1) (srfi 33)
          (chibi string) (chibi regexp))
  (include "pcre.scm"))
