
;;> Syntax to support optional and named keyword arguments.
;;> \scheme{let-optionals[*]} is originally from SCSH, and
;;> \scheme{let-keywords[*]} derived from Gauche.

;; Wrap bindings in temp variables to convert a let* definition to a
;; let definition.

(define-syntax let*-to-let
  (syntax-rules ()
    ((let*-to-let letstar ls (vars ...) ((v . d) . rest) . body)
     (let*-to-let letstar ls (vars ... (v tmp . d)) rest . body))
    ((let*-to-let letstar ls ((var tmp . d) ...) rest . body)
     (letstar ls ((tmp . d) ... . rest)
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
;;> \example{
;;> (let-optionals '(0) ((a 10) (b 11) (c 12))
;;>   (list a b c))}

(define-syntax let-optionals
  (syntax-rules ()
    ((let-optionals ls ((var default) ...) body ...)
     (let*-to-let let-optionals* ls () ((var default) ...) body ...))))

;;> \macro{(let-optionals* ls ((var default) ... [rest]) body ...)}
;;>
;;> \scheme{let*} equivalent to \scheme{let-optionals}.  Any required
;;> \var{default} values are evaluated in left-to-right order, with
;;> all preceding \var{var}s in scope.

;;> \macro{(opt-lambda ((var default) ... [rest]) body ...)}
;;>
;;> Shorthand for
;;> \schemeblock{
;;> (lambda o
;;>   (let-optionals o ((var default) ... [rest])
;;>      body ...))}

(define-syntax opt-lambda
  (syntax-rules ()
    ((opt-lambda vars . body)
     (lambda args (let-optionals args vars . body)))))

;;> \procedure{(keyword-ref ls key [default])}
;;>
;;> Search for the identifier \var{key} in the list \var{ls}, treating
;;> it as a property list of the form \scheme{(key1 val1 key2 val2
;;> ...)}, and return the associated \var{val}.  If not found, return
;;> \var{default}, or \scheme{#f}.

(define (keyword-ref ls key . o)
  (let lp ((ls ls))
    (if (and (pair? ls) (pair? (cdr ls)))
        (if (eq? key (caar ls))
            (cadr ls)
            (lp (cddr ls)))
        (and (pair? o) (car o)))))

;;> \macro{(keyword-ref* ls key default)}
;;>
;;> Macro equivalent of \scheme{keyword-ref}, where \var{default} is
;;> only evaluated if \var{key} is not found.

(define-syntax keyword-ref*
  (syntax-rules ()
    ((keyword-ref* ls key default)
     (cond ((memq key ls) => cadr) (else default)))))

(define (symbol->keyword sym)
  (string->symbol (string-append (symbol->string sym) ":")))

(define-syntax symbol->keyword*
  (syntax-rules ()
    ((symbol->keyword* sym)
     (string->symbol (string-append (symbol->string sym) ":")))))

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
;;> Note Chibi does not have any automatically quoted keywords, so
;;> when passing keyword arguments they must be quoted (or otherwise
;;> evaluated).
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

(define-syntax let-keywords
  (syntax-rules ()
    ((let-keywords ls vars . body)
     (let-key*-to-let ls () vars . body))))

;;> \macro{(let-keywords* ls ((var default) ... [rest]) body ...)}
;;>
;;> \scheme{let*} equivalent to \scheme{let-keywords*}.  Any required
;;> \var{default} values are evaluated in left-to-right order, with
;;> all preceding \var{var}s in scope.

(define-syntax let-keywords*
  (syntax-rules ()
    ((let-keywords* opt-ls () . body)
     (begin . body))
    ((let-keywords* (op . args) vars . body)
     (let ((tmp (op . args)))
       (let-keywords* tmp vars . body)))
    ((let-keywords* opt-ls ((var) . rest) . body)
     (let-keywords* opt-ls ((var #f) . rest) . body))
    ((let-keywords* opt-ls ((var default) . rest) . body)
     (let ((var (keyword-ref* opt-ls (symbol->keyword* 'var) default)))
       (let-keywords* opt-ls rest . body)))
    ((let-keywords* opt-ls ((var key default) . rest) . body)
     (let ((var (keyword-ref* opt-ls `key default)))
       (let-keywords* opt-ls rest . body)))
    ((let-keywords* opt-ls tail . body)
     (let ((tail opt-ls)) . body))))
