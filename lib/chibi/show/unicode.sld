
(define-library (chibi show unicode)
  (import (scheme base) (chibi show base) (srfi 151))
  (export as-unicode unicode-terminal-width)
  (include "unicode.scm"))
