;; repl.scm - friendlier repl with line editing and signal handling
;; Copyright (c) 2012-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A user-friendly REPL with line editing and signal handling.  The
;;> default REPL provided by chibi-scheme is very minimal, meant
;;> primarily to be small and work on any platform.  This module
;;> provides an advanced REPL that handles vt100 line editing and
;;> signal handling, so that C-c will interrupt a computation and
;;> bring you back to the REPL prompt.  To use this repl, run
;;> \command{chibi-scheme -R} from the command line or within Emacs.

(define (with-signal-handler sig handler thunk)
  (let ((old-handler #f))
    (dynamic-wind
      (lambda () (set! old-handler (set-signal-action! sig handler)))
      thunk
      (lambda () (set-signal-action! sig old-handler)))))

(define (warn msg . args)
  (let ((out (current-error-port)))
    (display msg out)
    (for-each (lambda (x) (write-char #\space out) (write x out)) args)
    (newline out)))

(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

(define (complete-sexp? str)
  (call-with-input-string str
    (lambda (in)
      (let lp () (if (not (eof-object? (read/ss in))) (lp))))))

(define (read-line/complete-sexp in)
  (let lp ((res ""))
    (let ((line (read-line in)))
      (cond
       ((eof-object? line)
        (if (equal? res "") line res))
       (else
        (let ((res (string-append res line "\n")))
          (if (protect (exn (else #f)) (complete-sexp? res))
              res
              (lp res))))))))

(define (buffer-complete-sexp? buf)
  (complete-sexp? (buffer->string buf)))

(define module? vector?)
(define (module-env mod) (vector-ref mod 1))

(define (all-exports env)
  (let lp ((env env) (res '()))
    (if (not env)
        res
        (lp (env-parent env) (lset-union eq? (env-exports env) res)))))

(define (string-common-prefix-length strings)
  (if (null? strings)
      0
      (let lp ((len (string-length (car strings)))
               (prev (car strings))
               (ls (cdr strings)))
        (if (or (null? ls) (zero? len))
            len
            (call-with-values (lambda () (string-mismatch prev (car ls)))
              (lambda (i1 i2)
                (lp (min len (string-cursor->index prev i1))
                    (car ls)
                    (cdr ls))))))))

(define (make-sexp-buffer-completer)
  (buffer-make-completer
   (lambda (buf word)
     (let* ((len (string-length word))
            (candidates
             (filter
              (lambda (w)
                (and (>= (string-length w) len)
                     (equal? word (substring w 0 len))))
              (map symbol->string
                   (map identifier->symbol
                        (all-exports (interaction-environment))))))
            (prefix-len (string-common-prefix-length candidates)))
       (if (> prefix-len len)
           (list (substring (car candidates) 0 prefix-len))
           (sort candidates))))))

(define (describe x . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (cond
     ((null? x)
      (display "empty list\n" out))
     ((list? x)
      (display "list of length " out) (write (length x) out) (newline out)
      (let lp ((ls x) (i 0))
        (cond
         ((pair? ls)
          (display " " out) (write i out) (display ": " out)
          (write/ss (car ls) out) (newline out)
          (lp (cdr ls) (+ i 1))))))
     ((pair? x)
      (display "pair with car " out) (write/ss (car x) out) (newline out)
      (display "and cdr " out) (write/ss (cdr x) out) (newline out))
     ((vector? x)
      (let ((len (vector-length x)))
        (display "vector of length " out) (write len out) (newline out)
        (let lp ((i 0))
          (cond
           ((< i len)
            (display " " out) (write i out) (display ": " out)
            (write/ss (vector-ref x i) out) (newline out)
            (lp (+ i 1)))))))
     ((boolean? x)
      (display (if x "boolean true\n" "boolean false\n") out))
     ((char? x)
      (let ((n (char->integer x)))
        (display "character " out) (write x out)
        (display ", code: " out) (write n out)
        (display ", #x" out) (display (number->string n 16) out)
        (display ", #o" out) (display (number->string n 8) out)
        (newline out)))
     ((and (integer? x) (exact? x))
      (display "exact integer " out) (write x out)
      (display "\n  #x" out) (display (number->string x 16) out)
      (display "\n  #o" out) (display (number->string x 8) out)
      (display "\n  #b" out) (display (number->string x 2) out)
      (newline out))
     (else
      (write/ss x out) (newline out)))))

;;> Runs an interactive REPL.  Repeatedly displays a prompt,
;;> then Reads an expression, Evaluates the expression, Prints
;;> the result then Loops.  Terminates when the end of input is
;;> reached or the \scheme|{\exit}| command is given.
;;>
;;> Basic Emacs-style line editing with persistent history
;;> completion is provided.  C-c can be used to interrupt the
;;> current computation and drop back to the prompt.  The
;;> following keyword arguments customize the REPL:
;;>
;;> \itemlist[
;;> \item{\scheme{in:} - the input port (default \scheme{(current-input-port)})}
;;> \item{\scheme{out:} - the output port (default \scheme{(current-output-port)})}
;;> \item{\scheme{module:} - the initial module}
;;> \item{\scheme{environment:} - the initial environment (default \scheme{(interaction-environment)})}
;;> \item{\scheme{escape:} - the command escape character (default \scheme|{#\\}|)}
;;> \item{\scheme{make-prompt:} - a procedure taking one argument (the current module name as a list) and returning a string to be used as the prompt}
;;> \item{\scheme{history:} - the initial command history}
;;> \item{\scheme{history-file:} - the file to save history to (default ~/.chibi-repl-history)}
;;> ]
;;>
;;> The module: and environment: keyword arguments should not both be given.
;;>
;;> REPL commands in the style of \hyperlink["http://s48.org/"]{Scheme48}
;;> are available to control out-of-band properties.  By default a
;;> command is written as an identifier beginning with an "@"
;;> character, but this can be customized with the \scheme{escape:}
;;> keyword.  The following commands are available:
;;>
;;> \itemlist[
;;> \item{\scheme|{\import <import-spec>}| - import the \var{<import-spec>} in the \scheme{interaction-environment}, useful if the \scheme{import} binding is not available}
;;> \item{\scheme|{\import-only <import-spec>}| - replace the \scheme{interaction-environment} with the given \var{<import-spec>}}
;;> \item{\scheme|{\in [<module>]}| - switch to \var{<module>}, or the \scheme{interaction-environment} if \var{<module>} is not specified}
;;> \item{\scheme|{\meta <expr>}| - evaluate \var{<expr>} in the \scheme{(meta)} module}
;;> \item{\scheme|{\meta-module-is <module>}| - switch the meta module to \var{<module>}}
;;> \item{\scheme|{\exit}| - exit the REPL}
;;> ]

;;> The results of the last ten successful evaluations are available
;;> via a history facility. \var{$0} holds the most recent result
;;> while \var{$9} holds the tenth-most recent result. Evaluations
;;> yielding single values are stored as single values while evaluations
;;> that yield multiple values are stored as lists of values. 


(define-record-type Repl
  (make-repl
   in out escape module env meta-env make-prompt history-file history raw?)
  repl?
  (in repl-in repl-in-set!)
  (out repl-out repl-out-set!)
  (escape repl-escape repl-escape-set!)
  (module repl-module repl-module-set!)
  (env repl-env repl-env-set!)
  (meta-env repl-meta-env  repl-meta-env-set!)
  (make-prompt repl-make-prompt repl-make-prompt-set!)
  (history-file repl-history-file repl-history-file-set!)
  (history repl-history repl-history-set!)
  (raw? repl-raw? repl-raw?-set!))

(define (repl/import-aux rp args meta continue only?)
  (let* ((mod-name (cadr args))
         (mod+imps (eval `(resolve-import ',mod-name) (repl-meta-env rp))))
    (cond
     ((pair? mod+imps)
      (protect
          (exn
           (else
            (print-exception exn (current-error-port))
            (warn "error loading module:" mod-name)
            (continue rp)))
        (let ((env (if only? (make-environment) (repl-env rp)))
              (imp-env
               (module-env
                (eval `(load-module ',(car mod+imps)) (repl-meta-env rp)))))
          (%import env imp-env (cdr mod+imps) #f)
          (repl-env-set! rp env)
          (continue rp))))
     (else
      (warn "couldn't find module:" mod-name)
      (continue rp)))))

(define (repl/import rp args meta continue)
  (repl/import-aux rp args meta continue #f))

(define (repl/import-only rp args meta continue)
  (repl/import-aux rp args meta continue #t))

(define (repl/in rp args meta continue)
  (cond
   ((null? (cdr args))
    (repl-module-set! rp #f)
    (repl-env-set! rp (interaction-environment)))
   ((eval `(load-module ',(cadr args)) (repl-meta-env rp))
    => (lambda (m)
         (repl-module-set! rp (cadr args))
         (repl-env-set! rp (module-env m))))
   (else
    (warn "couldn't find module:" (cadr args))))
  (continue rp))

(define (repl/meta rp args meta continue)
  (cond
   ((null? (cdr args))
    (warn "usage: @meta <expr>")
    (continue rp))
   ((and (symbol? (cadr args))
         (eqv? (repl-escape rp) (string-ref (symbol->string (cadr args)) 0)))
    (meta rp (cdr args) (lambda _ (continue rp))))
   (else
    (eval (cadr args) (repl-meta-env rp))
    (continue rp))))

(define (repl/meta-module-is rp args meta continue)
  (cond
   ((null? (cdr args))
    (warn "usage: @meta-module-is <module>"))
   ((eval `(load-module ',(cadr args)) (repl-meta-env rp))
    => (lambda (m) (repl-meta-env-set! rp (module-env m))))
   (else
    (warn "couldn't find module:" (cadr args))))
  (continue rp))

(define (repl/help rp args meta continue)
  (let ((out (repl-out rp)))
    (cond
     ((null? (cdr args))
      (display "Try @help <identifier> [<module>]\n" out))
     ((null? (cddr args))
      (let* ((failed (list 'failed))
             (val (protect (exn (else (print-exception exn) failed))
                    (eval (second args) (repl-env rp))))
             (mod (and (procedure? val) (containing-module val))))
        (cond
         (mod
          (write val out) (newline out) (newline out)
          (print-module-binding-docs (car mod) (second args) out))
         ((not (eq? val failed))
          (describe val out)))))
     (else
      (protect (exn (else (print-exception exn (current-error-port))))
        (print-module-binding-docs (third args) (second args) out))))
    (continue rp)))

(define (repl/exit rp args meta continue)
  ;; To exit the repl simply don't call continue.
  #f)

;; Utility to read all objects from a port accumulated into a list.
(define (read/ss/all port)
  (let loop ((l '()))
    (let ((x (read/ss port)))
      (if (eof-object? x)
        (reverse l)
        (loop (cons x l))))))

(define (string->sexp-list str)
  (call-with-input-string str read/ss/all))

;; Utility to provide additional help for common exceptions.
(define (repl-advise-exception exn out)
  (cond
   ((and (exception? exn)
         (equal? "undefined variable" (exception-message exn))
         (pair? (exception-irritants exn)))
    (let ((name (car (exception-irritants exn))))
      (cond
       ((identifier? name)
        (display "Searching for modules exporting " out)
        (display name out)
        (display " ...\n" out)
        (let ((mods (modules-exporting-identifier name)))
          (cond
           ((pair? mods)
            (display name out)
            (display " is exported by:\n" out)
            (for-each
             (lambda (m)
               (display "  " out) (write m out) (newline out))
             (sort (map car mods)
                   (lambda (a b)
                     (string<? (write-to-string a) (write-to-string b))))))
           (else
            (display "... none found.\n" out))))))))
   ((and (exception? exn)
         (equal? "couldn't find import" (exception-message exn))
         (pair? (exception-irritants exn)))
    (let* ((mod-name (car (exception-irritants exn)))
           (mod-file (module-name->file mod-name))
           (scm-file (string-append
                      (substring mod-file
                                 0
                                 (- (string-length mod-file) 4))
                      ".scm")))
      (display "Searched module path " out)
      (display (current-module-path) out)
      (display " for " out)
      (write mod-file out)
      (display ".\n" out)
      (cond
       ((find-module-file scm-file)
        => (lambda (file)
             (display "But found non-module-definition file " out)
             (write file out)
             (display ".\nNote module files must end in \".sld\".\n" out)))))
    )))

(define undefined-value (if #f #f))

(define $0 undefined-value)
(define $1 undefined-value)
(define $2 undefined-value)
(define $3 undefined-value)
(define $4 undefined-value)
(define $5 undefined-value)
(define $6 undefined-value)
(define $7 undefined-value)
(define $8 undefined-value)
(define $9 undefined-value)

(define (push-history-value! value)
  (set! $9 $8)
  (set! $8 $7)
  (set! $7 $6)
  (set! $6 $5)
  (set! $5 $4)
  (set! $4 $3)
  (set! $3 $2)
  (set! $2 $1)
  (set! $1 $0)
  (set! $0 value))

(define (push-history-value-maybe! value)
  (cond ((eq? value undefined-value) undefined-value)
        ((not (list? value)) (push-history-value! value))
        ((= (length value) 0) undefined-value)
        ((= (length value) 1) (push-history-value! (car value)))
        (else (push-history-value! value))))

(define (repl/eval rp expr-list)
  (let ((out (repl-out rp)))
    (protect (exn (else (print-exception exn out)))
      (let ((thread
             (make-thread
              (lambda ()
                ;; The inner protect in the child thread catches errors
                ;; from eval.
                (protect (exn
                          (else
                           (print-exception exn out)
                           (repl-advise-exception exn (current-error-port))))
                  (for-each
                   (lambda (expr)
                     (call-with-values
                         (lambda ()
                           (if (or (identifier? expr)
                                   (pair? expr)
                                   (null? expr))
                               (eval expr (repl-env rp))
                               expr))
                       (lambda res-list
                         (cond
                          ((not (or (null? res-list)
                                    (equal? res-list (list (if #f #f)))))
                           (push-history-value-maybe! res-list)
                           (write/ss (car res-list) out)
                           (for-each
                            (lambda (res)
                              (write-char #\space out)
                              (write/ss res out))
                            (cdr res-list))
                           (newline out))))))
                   expr-list))))))
        ;; If an interrupt occurs while the child thread is
        ;; still running, terminate it, otherwise wait for it
        ;; to complete.
        (with-signal-handler
         signal/interrupt
         (lambda (n)
           (display "\nInterrupt\n" out)
           (thread-terminate! thread))
         (lambda () (thread-join! (thread-start! thread))))))))

(define (repl/eval-string rp str)
  (repl/eval
   rp
   (protect (exn (else (print-exception exn (current-error-port))))
     ;; Ugly wrapper to account for the implicit state mutation
     ;; implied by the #!fold-case read syntax.
     (let ((in (repl-in rp))
           (in2 (open-input-string str)))
       (set-port-fold-case! in2 (port-fold-case? in))
       (set-port-line! in2 (port-line in))
       (let ((expr-list (read/ss/all in2)))
         (set-port-fold-case! in (port-fold-case? in2))
         expr-list)))))

(define (keywords->repl ls)
  (let-keywords* ls
      ((in in: (current-input-port))
       (out out: (current-output-port))
       (escape escape: #\@)
       (module module: #f)
       (env
        environment:
        (if module
            (module-env
             (if (module? module) module (load-module module)))
            (interaction-environment)))
       (make-prompt
        make-prompt:
        (lambda (module)
          (string-append (if module (write-to-string module) "") "> ")))
       (history-file
        history-file:
        (string-append (get-environment-variable "HOME")
                       "/.chibi-repl-history"))
       (history
        history:
        (or (protect (exn (else #f))
              (list->history (call-with-input-file history-file read)))
            (make-history)))
       (raw? raw?:
             (member (get-environment-variable "TERM") '("emacs" "dumb")))
       (meta-env meta-env: (module-env (load-module '(meta)))))
    (make-repl
     in out escape module env meta-env make-prompt history-file history raw?)))

(define (repl/edit-line rp)
  (let ((prompt ((repl-make-prompt rp) (repl-module rp)))
        (in (repl-in rp))
        (out (repl-out rp)))
    (cond
     ((repl-raw? rp)
      (display prompt out)
      (flush-output out)
      (read-line/complete-sexp in))
     (else
      (edit-line in out
                 'prompt: prompt
                 'history: (repl-history rp)
                 'complete?: buffer-complete-sexp?
                 'completion: (make-sexp-buffer-completer)
                 'catch-control-c?: #t
                 'fresh-line: " \x1B;[33m\\\x1B;[0m")))))

(define repl-commands
  `((import . ,repl/import)
    (import-only . ,repl/import-only)
    (in . ,repl/in)
    (meta . ,repl/meta)
    (meta-module-is . ,repl/meta-module-is)
    (? . ,repl/help)
    (h . ,repl/help)
    (help . ,repl/help)
    (exit . ,repl/exit)))

(define (repl . o)
  (let ((rp (keywords->repl o)))
    (let lp ((rp rp))
      (let ((line (repl/edit-line rp)))
        (cond
         ((or (not line) (eof-object? line)))
         ((equal? line "")
          (history-reset! (repl-history rp))
          (lp rp))
         (else
          (history-commit! (repl-history rp) line)
          (cond
           ((and (> (string-length line) 1)
                 (eqv? (repl-escape rp) (string-ref line 0)))
            ;; @ escaped command
            (let meta ((rp rp)
                       (args (string->sexp-list (substring line 1)))
                       (continue lp))
              (cond
               ((null? args)
                (warn "empty repl command")
                (continue rp))
               ((assq (car args) repl-commands)
                => (lambda (x) ((cdr x) rp args meta continue)))
               (else
                (warn "unknown repl command" (car args))
                (continue rp)))))
           (else
            ;; Normal expression to eval.
            (repl/eval-string rp line)
            (lp rp)))))))
    ;; Update the history file on completion.
    (if (repl-history-file rp)
        (protect
            (exn
             (else
              (let ((msg (integer->error-string)))
                (display "couldn't save repl history: " (current-error-port))
                (display msg (current-error-port))
                (newline (current-error-port)))))
          (call-with-output-file (repl-history-file rp)
            (lambda (out) (write (history->list (repl-history rp)) out)))))))

(define (main args)
  (import (only (chibi repl) $0 $1 $2 $3 $4 $5 $6 $7 $8 $9))
  (repl))
