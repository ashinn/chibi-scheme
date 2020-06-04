
(define-library (chibi diff)
  (import (scheme base) (srfi 1) (chibi optional) (chibi term ansi))
  (export lcs lcs-with-positions
          diff write-diff diff->string
          write-edits edits->string edits->string/color
          write-line-diffs
          write-line-diffs/color
          write-char-diffs
          write-char-diffs/color)
  (cond-expand
   (chibi (import (only (chibi io) port->list)))
   (else
    (begin
      (define (port->list reader port)
        (let lp ((res '()))
          (let ((x (reader port)))
            (if (eof-object? x)
                (reverse res)
                (lp (cons x res)))))))))
  (include "diff.scm"))
