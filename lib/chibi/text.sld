
(define-library (chibi text)
  (import (scheme base)
          (chibi text base)
          (chibi text search)
          (chibi text types)
          (chibi text utf8))
  (export
   make-text text? text-empty?
   text-beginning-of-line? text-end-of-line?
   text-char-length text-utf8-length text-piece-length
   string->text text->string text->utf8
   text-append text-append!
   text-concatenate text-concatenate!
   text-ref text-copy text-current-column
   text-insert! text-delete!
   text-mark text-mark!
   text-forward-char! text-backward-char!
   text-forward-word! text-backward-word!
   text-beginning-of-line? text-end-of-line?
   text-beginning-of-line! text-end-of-line!
   text-forward-line! text-backward-line!
   text-search!
   mark-text mark-offset mark-copy
   mark-anchor! mark-release!
   ))
