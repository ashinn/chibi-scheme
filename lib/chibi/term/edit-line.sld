
(define-library (chibi term edit-line)
  (export make-line-editor edit-line edit-line-repl
          make-history history-insert! history-reset!
          history-commit! history->list list->history buffer->string
          make-buffer buffer-make-completer
          buffer-clear buffer-refresh buffer-draw
          buffer-row buffer-col
          make-keymap make-standard-keymap)
  (import (scheme base) (scheme char) (scheme write))
  (cond-expand
   ((library (srfi 33))
    (import (srfi 33)))
   (else
    (import (srfi 60))))
  (cond-expand
   (chibi
    (import (chibi stty)))
   (chicken
    (import stty))
   (else
    (define (with-stty spec thunk)
      (thunk))))
  (cond-expand
   (chibi
    (import (only (chibi) protect print-exception)
            (chibi ast)))
   (else
    (begin
      (define (with-raw-io port thunk)
        (with-stty '(not icanon isig echo) thunk port))
      (define (get-terminal-width . x) 80)
      (define-syntax protect
        (syntax-rules () ((protect . x) (guard . x))))
      (define (print-exception exn . o)
        (let ((out (if (pair? o) (car o) (current-error-port))))
          (write exn out)
          (newline out)))
      (define (exception? x) #f)
      (define (exception-kind x) #f))))
  (include "edit-line.scm"))
