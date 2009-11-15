
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modules

(define *modules* '())
(define *this-module* '())
(define *load-path* (list "./lib" (string-append *module-directory* "/lib")))

(define (make-module exports env meta) (vector exports env meta))
(define (module-exports mod) (vector-ref mod 0))
(define (module-env mod) (vector-ref mod 1))
(define (module-meta-data mod) (vector-ref mod 2))
(define (module-env-set! mod env) (vector-set! mod 1 env))

(define (find-module-file name file)
  (let lp ((ls *load-path*))
    (and (pair? ls)
         (let ((path (string-append (car ls) "/" file)))
           (if (file-exists? path) path (lp (cdr ls)))))))

(define (module-name->strings ls res)
  (if (null? ls)
      res
      (let ((str (cond ((symbol? (car ls)) (symbol->string (car ls)))
                       ((number? (car ls)) (number->string (car ls)))
                       ((string? (car ls)) (car ls))
                       (else (error "invalid module name" (car ls))))))
        (module-name->strings (cdr ls) (cons "/" (cons str res))))))

(define (module-name->file name)
  (string-concatenate
   (reverse (cons ".module" (cdr (module-name->strings name '()))))))

(define (load-module-definition name)
  (let* ((file (module-name->file name))
         (path (find-module-file name file)))
    (if path (load path *config-env*))))

(define (find-module name)
  (cond
   ((assoc name *modules*) => cdr)
   (else
    (load-module-definition name)
    (cond ((assoc name *modules*) => cdr)
          (else #f)))))

(define (eval-module name mod)
  (let ((env (make-environment)))
    (for-each
     (lambda (x)
       (case (and (pair? x) (car x))
         ((import)
          (let ((mod2 (load-module (cadr x))))
            (%env-copy! env (module-env mod2) (module-exports mod2))))
         ((include)
          (for-each (lambda (f) (load (find-module-file name f) env)) (cdr x)))
         ((body)
          (for-each (lambda (expr) (eval expr env)) (cdr x)))))
     (module-meta-data mod))
    env))

(define (load-module name)
  (let ((mod (find-module name)))
    (if (and mod (not (module-env mod)))
        (module-env-set! mod (eval-module name mod)))
    mod))

(define-syntax define-module
  (rsc-macro-transformer
   (lambda (expr env)
     (let ((name (cadr expr))
           (body (cddr expr)))
       `(let ((tmp *this-module*))
          (set! *this-module* '())
          ,@body
          (set! *this-module* (reverse *this-module*))
          (let ((exports
                 (cond ((assq 'export *this-module*) => cdr)
                       (else '()))))
            (set! *modules*
                  (cons (cons ',name (make-module exports #f *this-module*))
                        *modules*)))
          (set! *this-module* tmp))))))

(define-syntax define-config-primitive
  (rsc-macro-transformer
   (lambda (expr env)
     `(define-syntax ,(cadr expr)
        (er-macro-transformer
         (lambda (expr rename compare)
           `(set! *this-module* (cons ',expr *this-module*))))))))

(define-config-primitive import)
(define-config-primitive export)
(define-config-primitive include)
(define-config-primitive body)

(let ((exports
       '(define set! let let* letrec lambda if cond case delay
         and or begin do quote quasiquote unquote unquote-splicing
         define-syntax let-syntax letrec-syntax syntax-rules eqv? eq? equal?
         not boolean? number? complex? real? rational? integer? exact? inexact?
         = < > <= >= zero? positive? negative? odd? even? max min + * - / abs
         quotient remainder modulo gcd lcm numerator denominator floor ceiling
         truncate round exp log sin cos tan asin acos atan sqrt
         expt make-rectangular make-polar real-part imag-part magnitude angle
         exact->inexact inexact->exact number->string string->number pair? cons
         car cdr set-car! set-cdr! caar cadr cdar cddr caaar caadr cadar caddr
         cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr
         caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
         null? list? list length append reverse list-tail list-ref memq memv
         member assq assv assoc symbol? symbol->string string->symbol char?
         char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>?
         char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
         char-upper-case? char-lower-case? char->integer integer->char
         char-upcase char-downcase string? make-string string string-length
         string-ref string-set! string=? string-ci=? string<? string>?
         string<=? string>=? string-ci<? string-ci>? string-ci<=? string-ci>=?
         substring string-append string->list list->string string-copy
         string-fill! vector? make-vector vector vector-length vector-ref
         vector-set! vector->list list->vector vector-fill! procedure? apply
         map for-each force call-with-current-continuation values
         call-with-values scheme-report-environment
         null-environment call-with-input-file call-with-output-file
         input-port? output-port? current-input-port current-output-port
         with-input-from-file with-output-to-file open-input-file
         open-output-file close-input-port close-output-port read read-char
         peek-char eof-object? char-ready? write display newline write-char
         load eval
         error file-exists? string-concatenate
         open-input-string open-output-string get-output-string
         sc-macro-transformer rsc-macro-transformer er-macro-transformer
         identifier? identifier=? identifier->symbol make-syntactic-closure
         register-simple-type make-constructor make-type-predicate
         make-getter make-setter
         )))
  (set! *modules*
        (list (cons '(scheme) (make-module exports
                                           (interaction-environment)
                                           (list (cons 'export exports)))))))

