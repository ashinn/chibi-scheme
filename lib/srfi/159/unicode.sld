
(define-library (srfi 159 unicode)
  (import (scheme base)
          (scheme char)
          (chibi show base)
          (srfi 130)
          (srfi 151))
  (export as-unicode unicode-terminal-width)
  (include "../166/width.scm" "../166/unicode.scm"))
