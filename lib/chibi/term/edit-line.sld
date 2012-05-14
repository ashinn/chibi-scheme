
(define-library (chibi term edit-line)
  (export make-line-editor edit-line edit-line-repl
          make-history history-insert!
          history-commit! history->list list->history buffer->string
          make-buffer buffer-make-completer buffer-row buffer-col)
  (import (scheme) (chibi stty) (srfi 9))
  (include "edit-line.scm"))
