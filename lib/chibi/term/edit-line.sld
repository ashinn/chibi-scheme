
(define-library (chibi term edit-line)
  (export make-line-editor edit-line edit-line-repl
          make-history history-insert! history-reset!
          history-commit! history->list list->history buffer->string
          make-buffer buffer-make-completer
          buffer-clear buffer-refresh buffer-draw
          buffer-row buffer-col)
  (import (chibi) (chibi stty) (srfi 9))
  (include "edit-line.scm"))
