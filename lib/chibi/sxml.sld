
;;> Utilities to convert sxml to xml or plain text.

(define-library (chibi sxml)
  (export sxml->xml sxml-display-as-html sxml-display-as-text sxml-strip
          html-escape html-tag->string)
  (import (scheme base) (scheme write))
  (include "sxml.scm"))
