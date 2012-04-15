
(define-library (chibi term edit-line)
  (export edit-line edit-line-repl make-history history-insert!
          history-commit! history->list list->history buffer->string
          buffer-make-completer)
  (import (scheme) (chibi stty) (srfi 9))
  (include "edit-line.scm"))
