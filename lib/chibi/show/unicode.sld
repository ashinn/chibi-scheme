
(define-library (chibi show unicode)
  (import (scheme base)
          (scheme char)
          (chibi show base)
          (srfi 130)
          (srfi 151))
  (export as-unicode
          unicode-terminal-width unicode-terminal-width/wide
          upcased downcased)
  (include "width.scm" "unicode.scm"))
