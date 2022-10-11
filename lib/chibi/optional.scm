
;;> Syntax to support optional and named keyword arguments.
;;> \scheme{let-optionals[*]} is originally from SCSH, and
;;> \scheme{let-keywords[*]} derived from Gauche.

;; Wrap bindings in temp variables to convert a let* definition to a
;; let definition.

(define-syntax let*-to-let
  (syntax-rules ()
    ((let*-to-let letstar ls (vars ...) ((v . d) . rest) . body)
     (let*-to-let letstar ls (vars ... (v tmp (tmp . d))) rest . body))
    ((let*-to-let letstar ls (vars ...) (v . rest) . body)
     (let*-to-let letstar ls (vars ... (v tmp tmp)) rest . body))
    ((let*-to-let letstar ls ((var tmp bind) ...) rest . body)
     (letstar ls (bind ... . rest)
       (let ((var tmp) ...) . body)))))

;;> \macro{(let-optionals ls ((var default) ... [rest]) body ...)}
;;>
;;> Binding construct similar to \scheme{let}.  The \var{var}s are
;;> bound to fresh locations holding values taken in order from the
;;> list \var{ls}, \var{body} is evaluated in the resulting
;;> environment, and the value(s) of the last expression of \var{body}
;;> returned.  If the length of \var{ls} is shorter than the number of
;;> \var{var}s, then the remaining \var{var}s taken their values from
;;> their corresponding \var{default}s, evaluated in an unspecified
;;> order.  Unused \var{default}s are not evaluated.  If a final
;;> \var{rest} var is specified, then it is bound to any remaining
;;> elements of \var{ls} beyond the length of \var{ls}, otherwise any
;;> extra values are unused.
;;>
;;> \var{ls} is evaluated only once.  It is an error if any
;;> \var{default} mutates \var{ls}.
;;>
;;> Typically used on the dotted rest list at the start of a lambda,
;;> \scheme{let-optionals} is more concise and more efficient than
;;> \scheme{case-lambda} for simple optional argument uses.
;;>
;;> \emph{Example:}
;;> \schemeblock{
;;> (define (copy-port . o)
;;>   (let-optionals o ((in (current-input-port))
;;>                     (out (current-output-port))
;;>                     (n-bytes #f))
;;>     (do ((i 0 (+ i 1))
;;>          (n (read-u8 in) (read-u8 in)))
;;>         ((or (and n-bytes (>= i n-bytes))
;;>              (eof-object? b)))
;;>       (write-u8 b out)))}
;;>
;;> \emph{Example:}
;;> \example{
;;> (let-optionals '(0) ((a 10) (b 11) (c 12))
;;>   (list a b c))}

(define-syntax let-optionals
  (syntax-rules ()
    ((let-optionals ls (var&default ... . rest) body ...)
     (let*-to-let let-optionals* ls () (var&default ... . rest) body ...))))

;;> \macro{(let-optionals* ls ((var default) ... [rest]) body ...)}
;;>
;;> \scheme{let*} equivalent to \scheme{let-optionals}.  Any required
;;> \var{default} values are evaluated in left-to-right order, with
;;> all preceding \var{var}s in scope.

;;> \macro{(opt-lambda ((var default) ... [rest]) body ...)}
;;>
;;> Shorthand for
;;> \schemeblock{
;;> (lambda (required ... . o)
;;>   (let-optionals o ((var default) ... [rest])
;;>      body ...))}

(define-syntax opt-lambda
  (syntax-rules ()
    ((opt-lambda vars . body)
     (lambda args (let-optionals args vars . body)))))

;;> \macro{(opt-lambda* ((var default) ... [rest]) body ...)}
;;>
;;> Variant of \scheme{opt-lambda} which binds using
;;> \scheme{let-optionals*}.

(define-syntax opt-lambda*
  (syntax-rules ()
    ((opt-lambda* vars . body)
     (lambda args (let-optionals* args vars . body)))))

;;> \macro{(define-opt (name (var default) ... [rest]) body ...)}
;;>
;;> Shorthand for
;;> \schemeblock{
;;> (define name (opt-lambda (var default) ... [rest]) body ...)}

(define-syntax define-opt
  (syntax-rules ()
    ((define-opt (name . vars) . body)
     (define name (opt-lambda vars . body)))))

;;> \macro{(define-opt* (name (var default) ... [rest]) body ...)}
;;>
;;> Shorthand for
;;> \schemeblock{
;;> (define name (opt-lambda* (var default) ... [rest]) body ...)}

(define-syntax define-opt*
  (syntax-rules ()
    ((define-opt* (name . vars) . body)
     (define name (opt-lambda* vars . body)))))

(define (mem-key key ls)
  (and (pair? ls)
       (pair? (cdr ls))
       (if (eq? key (car ls))
           ls
           (mem-key key (cddr ls)))))

;;> \procedure{(keyword-ref ls key [default])}
;;>
;;> Search for the identifier \var{key} in the list \var{ls}, treating
;;> it as a property list of the form \scheme{(key1 val1 key2 val2
;;> ...)}, and return the associated \var{val}.  If not found, return
;;> \var{default}, or \scheme{#f}.

(define (keyword-ref ls key . o)
  (cond ((mem-key key ls) => (lambda (cell) (cadr cell)))
        (else (and (pair? o) (car o)))))

;;> \macro{(keyword-ref* ls key default)}
;;>
;;> Macro equivalent of \scheme{keyword-ref}, where \var{default} is
;;> only evaluated if \var{key} is not found.

(define-syntax keyword-ref*
  (syntax-rules ()
    ((keyword-ref* ls key default)
     (cond ((mem-key key ls) => cadr) (else default)))))

(define (symbol->keyword sym)
  (string->symbol (string-append (symbol->string sym) ":")))

(define-syntax let-key*-to-let
  (syntax-rules ()
    ((let-key*-to-let ls (vars ...) ((v d) . rest) . body)
     (let-key*-to-let ls (vars ... (v tmp ,(symbol->keyword 'v) d)) rest
                      . body))
    ((let-key*-to-let ls (vars ...) ((v k d) . rest) . body)
     (let-key*-to-let ls (vars ... (v tmp k d)) rest . body))
    ((let-key*-to-let ls ((var tmp k d) ...) rest . body)
     (let-keywords* ls ((tmp k d) ... . rest)
       (let ((var tmp) ...) . body)))))

;;> \macro{(let-keywords ls ((var [keyword] default) ... [rest]) body ...)}
;;>
;;> Analogous to \scheme{let-optionals}, except instead of binding the
;;> \var{var}s by position they are bound by name, by searching in
;;> \var{ls} with \scheme{keyword-ref*}.  If an optional \var{keyword}
;;> argument is provided it must be an identifier to use as the name,
;;> otherwise \var{var} is used, appending a ":" (colon).  If the name
;;> is not found, \var{var} is bound to \var{default}, even if unused
;;> names remain in \var{ls}.
;;>
;;> Keyword arguments have precedence in CommonLisp, DSSSL, and SRFI
;;> 89.  However, unlike these systems you cannot mix optional and
;;> keyword arguments.
;;>
;;> If an optional trailing identifier \var{rest} is provided, it is
;;> bound to the list of unused arguments not bound to any \var{var}.
;;> This is useful for chaining together keyword argument procedures -
;;> you can extract just the arguments you need and pass on the rest
;;> to another procedure.  The \var{rest} usage is similar to Python's
;;> \code{**args} (again predated by CommonLisp and DSSSL).
;;>
;;> Note R7RS does not have a disjoint keyword type or auto-quoting
;;> syntax for keywords - they are simply identifiers (though no type
;;> checking is performed).  Thus when passing keyword arguments they
;;> must be quoted (or otherwise dynamically evaluated).
;;>
;;> \emph{Example:}
;;> \example{
;;> (define (make-person . o)
;;>   (let-keywords o ((name "John Doe")
;;>                    (age 0)
;;>                    (occupation job: 'unemployed))
;;>     (vector name age occupation)))
;;>
;;> (list (make-person)
;;>       (make-person 'name: "Methuselah" 'age: 969)
;;>       (make-person 'name: "Dr. Who" 'job: 'time-lord 'age: 1500))
;;> }
;;>
;;> \emph{Example:}
;;> \example{
;;> (let-keywords '(b: 2 a: 1 other: 9)
;;>     ((a 0) (b 0) (c 0) rest)
;;>   (list a b c rest))
;;> }
;;>
;;> \emph{Example:}
;;> \example{
;;> (define (auth-wrapper proc)
;;>   (lambda o
;;>     (let-keywords o ((user #f)
;;>                      (password #f)
;;>                      rest)
;;>       (if (authenticate? user password)
;;>           (apply proc rest)
;;>           (error "access denied")))))
;;>
;;> ((auth-wrapper make-payment) 'user: "bob" 'password: "5ecret" 'amount: 50)
;;> }

(define-syntax let-keywords
  (syntax-rules ()
    ((let-keywords ls vars . body)
     (let-key*-to-let ls () vars . body))))

;; Returns the plist ls filtering out key-values found in keywords.
(define (remove-keywords ls keywords)
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pair? (cdr ls)))
        (if (memq (car ls) keywords)
            (lp (cddr ls) res)
            (lp (cddr ls) (cons (cadr ls) (cons (car ls) res))))
        (reverse res))))

;; Extracts the known keywords from a let-keyword spec and removes
;; them from the opt-ls.
(define-syntax remove-keywords*
  (syntax-rules ()
    ((remove-keywords* opt-ls (keys ...) ((var key default) . rest))
     (remove-keywords* opt-ls (keys ... key) rest))
    ((remove-keywords* opt-ls (keys ...) ((var default) . rest))
     (remove-keywords* opt-ls (keys ... ,(symbol->keyword* 'var)) rest))
    ((remove-keywords* opt-ls (keys ...) ())
     (remove-keywords opt-ls `(keys ...)))))

;;> \macro{(let-keywords* ls ((var [keyword] default) ... [rest]) body ...)}
;;>
;;> \scheme{let*} equivalent to \scheme{let-keywords}.  Any required
;;> \var{default} values are evaluated in left-to-right order, with
;;> all preceding \var{var}s in scope.
;;>
;;> \emph{Example:}
;;> \example{
;;> (let-keywords* '(b: 5)
;;>     ((a 1) (b (* a 2)) (c (* b 3)))
;;>   (list a b c))
;;> }

(define-syntax let-keywords*
  (syntax-rules ()
    ((let-keywords* opt-ls () . body)
     (begin . body))
    ((let-keywords* (op . args) vars . body)
     (let ((tmp (op . args)))
       (let-keywords* tmp vars . body)))
    ((let-keywords* opt-ls ((var) (vars . x) ...) . body)
     (let-keywords* opt-ls ((var #f) (vars . x) ...) . body))
    ((let-keywords* opt-ls ((var default) (vars . x) ...) . body)
     (let ((var (keyword-ref* opt-ls (symbol->keyword* 'var) default)))
       (let-keywords* opt-ls ((vars . x) ...) . body)))
    ((let-keywords* opt-ls ((var key default) (vars . x) ...) . body)
     (let ((var (keyword-ref* opt-ls `key default)))
       (let-keywords* opt-ls ((vars . x) ...) . body)))
    ((let-keywords* opt-ls ((vars . x) ... tail) . body)
     (let ((tail (remove-keywords* opt-ls () ((vars . x) ...))))
       (let-keywords* opt-ls ((vars . x) ...) . body)))))
