
(define-library (srfi 166 unicode)
  (import (scheme base) (srfi 130) (srfi 151) (srfi 166 base))
  (export as-unicode unicode-terminal-width unicode-terminal-width/wide)
  (include "../../chibi/show/width.scm"
           "../../chibi/show/unicode.scm"))
