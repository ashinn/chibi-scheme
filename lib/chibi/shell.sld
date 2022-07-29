
(define-library (chibi shell)
  (import (scheme base) (scheme bitwise) (scheme char) (scheme cxr)
          (scheme list) (scheme write) (srfi 130)
          (chibi io) (chibi filesystem) (chibi process)
          (only (chibi) port-fileno define-auxiliary-syntax))
  (export shell shell& shell-pipe call-with-shell-io
          shell->string shell->string-list
          shell->sexp shell->sexp-list
          shell-if shell-and shell-or shell-do
          in< out> err> out>> err>> >< >> <<)
  (begin
    (define shell-fork fork)
    (define shell-exec execute)
    (define shell-exit exit)
    (define (shell-wait pid)
      (cadr (waitpid pid 0)))
    (define (shell-create-pipe) (apply cons (open-pipe)))
    (define shell-dup duplicate-file-descriptor-to)
    (define shell-open-input open-input-file-descriptor)
    (define shell-open-output open-output-file-descriptor)
    (define shell-close close-file-descriptor)
    (define (shell-port->fd port)
      (port-fileno port))
    (define (shell-fd->input-port fd)
      (open-input-file-descriptor fd))
    (define (shell-fd->output-port fd)
      (open-output-file-descriptor fd)))
  (include "shell.scm"))
