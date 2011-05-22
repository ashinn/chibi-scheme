;;;; repl.scm - friendlier repl with line editing and signal handling
;;
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

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

(define (buffer-complete-sexp? buf)
  (guard (exn (else #f))
    (call-with-input-string (buffer->string buf)
      (lambda (in)
        (let lp () (if (not (eof-object? (read/ss in))) (lp)))))
    #t))

(define module? vector?)
(define (module-env mod) (vector-ref mod 1))

(define (repl . o)
  (let* ((module (cond ((memq 'module: o) => cadr) (else #f)))
         (env (if module
                  (module-env (if (module? module)
                                  module
                                  (eval `(load-module ',module) *config-env*)))
                  (interaction-environment)))
         (history (cond ((memq 'history: o) => cadr) (else (make-history))))
         (raw? (cond ((memq 'raw?: o) => cadr)
                     (else (member (get-environment-variable "TERM")
                                   '("emacs" "dumb"))))))
    (let lp ((module module) (env env) (config-env *config-env*))
      (let* ((prompt
              (string-append (if module (write-to-string module) "") "> "))
             (line
              (cond
               (raw?
                (display prompt)
                (flush-output)
                (read-line))
               (else
                (edit-line
                 'prompt: prompt
                 'history: history
                 'complete?: buffer-complete-sexp?)))))
        (cond
         ((or (not line) (eof-object? line)))
         ((equal? line "") (lp module env config-env))
         (else
          (history-commit! history line)
          (cond
           ((and (> (string-length line) 0) (eqv? #\@ (string-ref line 0)))
            (let meta ((env env)
                       (line (substring line 1 (string-length line)))
                       (continue lp))
              (define (fail msg . args)
                (apply warn msg args)
                (continue module env config-env))
              (call-with-input-string line
               (lambda (in)
                 (let ((op (read/ss in)))
                   (case op
                     ((in)
                      (let ((name (read/ss in)))
                        (cond
                         ((eof-object? name)
                          (continue #f (interaction-environment) config-env))
                         ((eval `(load-module ',name) config-env)
                          => (lambda (m)
                               (continue name (module-env m) config-env)))
                         (else
                          (fail "couldn't find module:" name)))))
                     ((config)
                      (let ((expr (read/ss in)))
                        (cond
                         ((and (symbol? expr)
                               (eqv? #\@ (string-ref (symbol->string expr) 0)))
                          (meta config-env
                                (substring line 6 (string-length line))
                                (lambda _ (continue module env config-env))))
                         (else
                          (eval expr config-env)
                          (continue module env config-env)))))
                     ((config-module-is)
                      (let ((name (read/ss in)))
                        (cond
                         ((eval `(load-module ',name) config-env)
                          => (lambda (m) (lp module env (module-env m))))
                         (else
                          (fail "couldn't find module:" name)))))
                     (else
                      (fail "unknown repl command:" op))))))))
           (else
            (guard
             (exn
              (else (print-exception exn (current-error-port))))
             (let* ((expr (call-with-input-string line read/ss))
                    (thread
                     (make-thread
                      (lambda ()
                        (guard
                         (exn
                          (else (print-exception exn (current-error-port))))
                         (let ((res (eval expr env)))
                           (cond
                            ((not (eq? res (if #f #f)))
                             (write/ss res)
                             (newline)))))))))
               (with-signal-handler
                signal/interrupt
                (lambda (n)
                  (display "Interrupt\n" (current-error-port))
                  (thread-terminate! thread))
                (lambda () (thread-join! (thread-start! thread))))))
            (lp module env config-env)))))))))
