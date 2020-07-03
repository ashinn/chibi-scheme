
(define-library (srfi 166 unicode)
  (import (scheme base) (scheme write)
          (scheme char)
          (srfi 130)
          (srfi 151)
          (srfi 166 base))
  (export terminal-aware
          string-terminal-width string-terminal-width/wide
          substring-terminal-width substring-terminal-width/wide
          upcased downcased)
  (include "width.scm"
           "unicode.scm"))
