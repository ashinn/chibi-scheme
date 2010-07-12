
(define (run-repl module env)
  (if module (display module))
  (display "> ")
  (flush-output)
  (let lp ()
    (let ((ch (peek-char)))
      (cond ((eof-object? ch)
             (exit 0))
            ((and (char? ch) (char-whitespace? ch))
             (read-char)
             (lp)))))
  (cond
   ((eq? #\@ (peek-char))
    (read-char)
    (let ((sym (read)))
      (if (not (symbol? sym))
          (error "repl: invalid @ syntax: @" sym)
          (case sym
            ((config)
             (let ((res (eval (read) *config-env*)))
               (cond
                ((not (eq? res (if #f #f)))
                 (write res)
                 (newline)))
               (run-repl module env)))
            ((in)
             (let ((mod (read)))
               (if (or (not mod) (equal? mod '(scheme)))
                   (run-repl #f (interaction-environment))
                   (let ((env (eval `(module-env (load-module ',mod))
                                    *config-env*)))
                     (run-repl mod env)))))
            (else
             (error "repl: unknown @ escape" sym))))))
   (else
    (let ((expr (read)))
      (cond
       ((eof-object? expr)
        (exit 0))
       (else
        (let ((res (eval expr env)))
          (cond
           ((not (eq? res (if #f #f)))
            (write res)
            (newline)))
          (run-repl module env))))))))

(define (repl)
  (set-signal-action! signal/interrupt
                      (lambda (n info)
                        (newline)
                        (run-repl #f (interaction-environment))))
  (current-exception-handler
   (lambda (exn)
     (print-exception exn (current-error-port))
     (run-repl #f (interaction-environment))))
  (run-repl #f (interaction-environment)))
