;; environment.scm - the environment (reader) monad for Scheme
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A Scheme take on the environment (reader) monad, focusing more on
;;> being efficient and convenient than pure.

;;> \macro{(define-environment-monad name keyword: value ...)}
;;>
;;> Define a new environment monad.  This syntax hides the
;;> implementation, allowing the use of records, dynamic parameters,
;;> or explicit value passing.
;;>
;;> The \var{name} is used for description and may or may not be bound
;;> to a value representing the monad.  All other parameters are
;;> keywords, and with the exception of \scheme{fields} simply provide
;;> binding names for the monad operators described below.
;;>
;;> The \scheme{fields:} keyword takes a list of field name
;;> identifiers known to be used by the monad.  This is an
;;> optimization hint, as the monad can be used to store and query
;;> values for any identifier at runtime.
;;>
;;> The following keywords obey the definition of a monad:
;;>
;;> sequence: sequence (>>) - Essentially a semi-colon, this joins two
;;>  operations together.
;;>
;;> bind: (>>=) - Runs a normal function.  As a syntactic convenience,
;;>  \scheme{bind} looks and behaves like a lambda, but the parameters
;;>  of the \scheme{bind} are bound as Scheme variables with the
;;>  values of the corresponding environment variables.  Thus you
;;>  fetch the values of foo and bar with:
;;>
;;>    \scheme{(bind (foo bar) ...)}
;;>
;;>  hiding the need for an explicit \scheme{ask}.  If you want to
;;>  bind the values to some other name, you can use it like a
;;>  \scheme{let}:
;;>
;;>    \scheme{(bind ((my-foo foo) (my-bar bar)) ...)}
;;>
;;> return: Returns a pure (non-monadic) value.
;;>
;;> run: Start the monad.
;;>
;;> The following are specific to the environment monad:
;;>
;;> ask: Ask the current value of an environment variable.  This is not
;;>  meant to be used directly - use the `bind' syntax to query bindings.
;;>
;;> local: Shadow the value one or more environment variables,
;;>  analogous to `let'.
;;>
;;> In addition, support for optional mutation is provided:
;;>
;;> local!: (local! (var val) ...) will update the environment with
;;>  the corresponding variable bindings.  In a sequence, successive
;;>  operations will see the result of the update, unlike with `local'.
;;>  This is allowed, but not required, to perform mutation.
;;>
;;> bind-fork: \scheme{(bind-fork a b)} runs `a' followed by `b',
;;>   passing `b' the original state before `a' was run.

(define-syntax define-environment-monad
  (syntax-rules ()
    ((define-environment-monad name clauses ...)
     (dem name (ask %ask) (tell %tell) c f! f s r w u z () clauses ...))))

(define-syntax dem
  (syntax-rules (fields: sequence: bind: bind-fork:
                 local: local!: run: return: ask: tell: copy:)
    ((dem n ask tell c f! f s r w u z (fls ...)
          (fields: (fl get put) . fl-r) . x)
     (dem n ask tell c f! f s r w u z (fls ... (fl #f get put))
          (fields: . fl-r) . x))
    ((dem n ask tell c f! f s r w u z (fls ...) (fields:) . x)
     (dem n ask tell c f! f s r w u z (fls ...) . x))
    ((dem n ask tell c f! f s r w u z ())
     (syntax-error "missing fields clause in define-state-monad"))
    ((dem n ask tell c f! f s r w u z fls (bind: fn!) . x)
     (dem n ask tell c fn! f s r w u z fls . x))
    ((dem n ask tell c f! f s r w u z fls (bind-fork: fn) . x)
     (dem n ask tell c f! fn s r w u z fls . x))
    ((dem n ask tell c f! f s r w u z fls (sequence: seq) . x)
     (dem n ask tell c f! f seq r w u z fls . x))
    ((dem n ask tell c f! f s r w u z fls (run: run) . x)
     (dem n ask tell c f! f s run w u z fls . x))
    ((dem n ask tell c f! f s r w u z fls (return: return) . x)
     (dem n ask tell c f! f s r w u return fls . x))
    ((dem n ask tell c f! f s r w u z fls (local: local) . x)
     (dem n ask tell c f! f s r local u z fls . x))
    ((dem n ask tell c f! f s r w u z fls (local!: local!) . x)
     (dem n ask tell c f! f s r w local! z fls . x))
    ((dem n ask tell c f! f s r w u z fls (ask: a %a) . x)
     (dem n (a %a) tell c f! f s r w u z fls . x))
    ((dem n ask tell c f! f s r w u z fls (tell: t %t) . x)
     (dem n ask (t %t) c f! f s r w u z fls . x))
    ((dem n ask tell c f! f s r w u z fls (copy: copy) . x)
     (dem n ask tell copy f! f s r w u z fls . x))
    ((dem n ask tell c f! f s r w u z fls clause . x)
     (syntax-error "unknown clause" 'clause))
    ((dem n (ask %ask) (tell %tell) c f! f s r w u z ((field init get put) ...))
     (begin
       ;; Internals
       (define-record-type n
         (make-state field ... %props)
         state?
         (field get put) ...
         (%props get-props set-props!))
       (define (%ask st x)
         (case x
           ((field) (get st)) ...
           (else (cond ((assq x (get-props st)) => cdr) (else #f)))))
       (define-syntax ask
         (syntax-rules (quote field ...)
           ((ask st 'field) (get st)) ...
           ((ask st x) (%ask st x))))
       (define (%tell st x val)
         (case x
           ((field) (put st val)) ...
           (else
            (cond
             ((assq x (get-props st))
              => (lambda (cell) (set-cdr! cell val)))
             (else
              (set-props! st (cons (cons x val) (get-props st))))))))
       (define-syntax tell
         (syntax-rules (quote field ...)
           ((tell st 'field val) (put st val)) ...
           ((tell st x val) (%tell st x val))))
       ;; External API
       ;;
       ;; copy
       (define (c st)
         (make-state
          (get st) ...
          (map (lambda (x) (cons (car x) (cdr x))) (get-props st))))
       ;; bind - a function
       (define-syntax f!
         (syntax-rules ooo ()
           ((f! ("step") (params ooo) ((p param) . rest) . body)
            (f! ("step") (params ooo (p param)) rest . body))
           ((f! ("step") (params ooo) ((param) . rest) . body)
            (f! ("step") (params ooo (param param)) rest . body))
           ((f! ("step") (params ooo) (param . rest) . body)
            (f! ("step") (params ooo (param param)) rest . body))
           ((f! ("step") ((p param) ooo) () . body)
            (lambda (st)
              (let ((p (ask st 'param)) ooo)
                ((let () . body) st))))
           ((f! params . body)
            (f! ("step") () params . body))))
       ;; fork - run on a copy of the state
       (define-syntax f
         (syntax-rules ()
           ((f a) a)
           ((f a b) (lambda (st) (a (c st)) (b st)))
           ((f a b . c) (f a (f b . c)))))
       ;; sequence
       (define-syntax s
         (syntax-rules ()
           ((s f) f)
           ((s f . g) (lambda (st) ((s . g) (f st))))))
       ;; update in place
       (define-syntax u
         (syntax-rules ooo ()
           ((u (prop value) ooo)
            (lambda (st)
              (tell st 'prop value) ooo
              st))))
       ;; local binding - update temporarily
       (define-syntax w
         (syntax-rules ooo ()
           ((w ("step") ((p tmp v) ooo) () . b)
            (lambda (st)
              (let ((tmp (ask st 'p)) ooo)
                (tell st 'p v) ooo
                (let ((st ((begin . b) st)))
                  (tell st 'p tmp) ooo
                  st))))
           ((w ("step") (props ooo) ((p v) . rest) . b)
            (w ("step") (props ooo (p tmp v)) rest . b))
           ((w ((prop value) ooo) . body)
            (w ("step") () ((prop value) ooo) . body))))
       ;; run
       (define (r proc)
         (proc (make-state init ... '())))
       ;; return
       (define (z x)
         (lambda (st) x))))))
