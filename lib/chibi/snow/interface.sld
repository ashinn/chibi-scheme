
(define-library (chibi snow interface)
  (export warn info message die input input-password input-number yes-or-no?
          restore-history save-history)
  (import (scheme base) (scheme char) (scheme read) (scheme write)
          (scheme file) (scheme process-context) (srfi 1)
          (chibi config) (chibi pathname) (chibi show))
  (cond-expand
   (chibi
    (import (chibi filesystem) (chibi stty)))
   (chicken
    (import posix stty)
    (begin
      (define (create-directory* dir) (create-directory dir #t))
      (define (edit-line ))))
   (sagittarius
    (import (only (sagittarius) create-directory)
            (chibi string))
    (begin
      (define (create-directory* dir . o)
        (let ((mode (if (pair? o) (car o) #o755)))
          (or (file-directory? dir)
              (create-directory dir mode)
              (let* ((start (string-cursor-start dir))
                     (slash
                      (string-find-right dir #\/ start
                                         (string-skip-right dir #\/))))
                (and (string-cursor>? slash start)
                     (let ((parent (substring-cursor dir start slash)))
                       (and (not (equal? parent dir))
                            (not (file-exists? parent))
                            (create-directory* parent mode)
                            (create-directory dir mode))))))))
      (define (with-stty spec thunk)
        (thunk)))))
  (cond-expand
   (chibi
    (import (chibi term edit-line)))
   (else
    (begin
      (define (get-key ls key . o)
        (let ((x (memq key ls)))
          (if (and x (pair? (cdr x))) (cadr x) (and (pair? o) (car o)))))
      (define (edit-line . args)
        (let ((in (if (and (pair? args) (input-port? (car args)))
                      (car args)
                      (current-input-port)))
              (out (if (and (eq? in (car args))
                            (pair? (cdr args))
                            (output-port? (cadr args)))
                       (cadr args)
                       (current-output-port)))
              (prompter (get-key args 'prompt: "> ")))
          (display (if (procedure? prompter) (prompter) prompter) out)
          (flush-output-port out)
          (read-line in))))))
  (include "interface.scm"))
