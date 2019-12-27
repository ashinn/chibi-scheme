(define (list-bindings env)
  (let parents ((env env) (binds '()))
    (if (not env) binds
        (let symbols ((syms (env-exports env)) (binds binds))
          (if (null? syms) (parents (env-parent env) binds)
              (symbols (cdr syms) (if (assv (car syms) binds) binds
                                      (cons (cons (car syms) env)
                                            binds))))))))

(define (apropos-list-bindings query)
  (cond ((symbol? query) (set! query (symbol->string query)))
        ((not (string? query))
         (error "Apropos query must be a symbol or a string")))
  (sort (filter (lambda (binding)
                  (string-contains (symbol->string (car binding)) query))
                (list-bindings (interaction-environment)))
        (lambda (a b) (string<? (symbol->string (car a))
                                (symbol->string (car b))))))

(define (apropos-list query) (map car (apropos-list-bindings query)))

(define (apropos-prefix sym env)
  (let ((p "procedure  ")
        (s "syntax     ")
        (v "variable   "))
    (guard (_ (else s)) (if (procedure? (eval sym env)) p v))))

(define (apropos query)
  (for-each (lambda (bind)
              (display (apropos-prefix (car bind) (cdr bind)))
              (write (car bind))
              (newline))
            (apropos-list-bindings query)))
