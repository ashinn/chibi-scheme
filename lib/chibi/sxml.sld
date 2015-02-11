
(define-library (chibi sxml)
  (export sxml->xml sxml-display-as-html sxml-display-as-text sxml-strip
          html-escape)
  (import (scheme base) (scheme write))
  (include "sxml.scm"))
