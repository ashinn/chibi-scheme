
(define-library (chibi sxml)
  (export sxml-display-as-html sxml-display-as-text sxml-strip)
  (import (scheme base) (scheme write))
  (include "sxml.scm"))
