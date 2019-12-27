(define-library (chibi apropos)
  (export apropos apropos-list)
  (import (scheme base) (chibi) (chibi string) (srfi 1) (srfi 95))
  (include "apropos.scm"))
