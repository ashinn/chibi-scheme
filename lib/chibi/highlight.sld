
(define-library (chibi highlight)
  (export highlight highlight-detect-language highlighter-for highlight-style
          highlight-scheme highlight-c highlight-assembly)
  (import (scheme) (srfi 1) (chibi io))
  (include "highlight.scm"))
