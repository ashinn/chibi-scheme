
;;> Define a new generic function named \var{name}.

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic name)
     (define name (make-generic 'name)))))

;; call-next-method needs to be unhygienic
'(define-syntax define-method
  (syntax-rules ()
    ((define-method (name (param type) ...) . body)
     (generic-add! name
                   (list type ...)
                   (lambda (next param ...)
                     (let-syntax ((call))
                       . body))))))

;;> \macro{(define-method (name params ...) body ...)}

;;> Each parameter should be either a single identifier or a list of the form
;;> \scheme{(param type)} where \var{param} is the parameter name and
;;> \var{type} is a predicate which returns true if it's argument is of the
;;> correct type.
;;> Parameters without a predicate will always match.

;;> If multiple methods satisfy the arguments, the most recent method
;;> will be used.  The special form \scheme{(call-next-method)} can be
;;> invoked to call the next most recent method with the same arguments.

(define-syntax define-method
  (er-macro-transformer
   (lambda (e r c)
     (let ((name (car (cadr e)))
           (params (let lp ((params (cdr (cadr e))))
                     (cond
                      ((not (pair? params)) '())
                      ((identifier? (car params))
                       (cons `(,(car params) (lambda _ #t))
                             (lp (cdr params))))
                      (else
                       (cons (car params) (lp (cdr params)))))))
           (variadic? (let lp ((params (cdr (cadr e))))
                        (cond
                         ((pair? params) (lp (cdr params)))
                         ((null? params) #f)
                         (else params))))
           (body (cddr e)))
       `(,(r 'generic-add!) ,name
         (,(r 'list) ,@(map cadr params))
         ',variadic?
         (,(r 'lambda) ,@(if variadic?
                             `((,(r 'next) ,@(map car params) . ,variadic?))
                             `((,(r 'next) ,@(map car params))))
          (,(r 'let-syntax) ((call-next-method
                              (,(r 'syntax-rules) ()
                               ((_) (,(r 'next))))))
           ,@body)))))))

(define (no-applicable-method-error name args)
  (error "no applicable method" name args))

(define (satisfied? preds args)
  (cond ((null? preds) (null? args))
        ((null? args) #f)
        (((car preds) (car args)) (satisfied? (cdr preds) (cdr args)))
        (else #f)))

(define add-method-tag (list 'add-method-tag))

;;> Create a new first-class generic function named \var{name}.

(define (take n lst)
  (cond
   ((not (pair? lst))
    '())
   ((zero? n)
    '())
   (else
    (cons (car lst)
          (take (- n 1) (cdr lst))))))

(define (make-generic name)
  (let ((name name)
        (methods (make-vector 6 '()))
        (variadic-methods (make-vector 6 '())))
    (vector-set! methods
                 4
                 (list (cons (list (lambda (x) (eq? x add-method-tag))
                                   list?
                                   (lambda (x) (or (boolean? x)
                                                   (symbol? x)))
                                   procedure?)
                             (lambda (next t preds rest? func)
                               (if rest?
                                   (set! variadic-methods (insert-method! variadic-methods preds func))
                                   (set! methods (insert-method! methods preds func)))))))
    (lambda args
      (letrec* ((len (length args))
                (search-variadic
                 (lambda (idx)
                   (cond
                    ((> idx len)
                     (no-applicable-method-error name args))
                    (else
                     (let search-matching ((checks+fns (vector-ref variadic-methods idx)))
                       (cond
                        ((null? checks+fns)
                         (search-variadic (+ 1 idx)))
                        ((satisfied? (caar checks+fns)
                                     (take idx args))
                         (apply (cdar checks+fns)
                                (lambda () (search-matching (cdr checks+fns)))
                                args))
                        (else
                         (search-matching (cdr checks+fns))))))))))
        (cond
         ((>= len (vector-length methods))
          (search-variadic 0))
         (else
          (let lp ((ls (vector-ref methods len)))
            (cond
             ((null? ls)
              (search-variadic 0))
             ((satisfied? (car (car ls)) args)
              (apply (cdr (car ls)) (lambda () (lp (cdr ls))) args))
             (else
              (lp (cdr ls)))))))))))

(define (insert-method! vec preds f)
  (let ((vlen (vector-length vec))
        (plen (length preds)))
    (let ((res (if (>= plen vlen)
                   (let ((r (make-vector (+ vlen 1) '())))
                     (do ((i 0 (+ i 1)))
                         ((>= i vlen) r)
                       (vector-set! r i (vector-ref vec i))))
                   vec)))
      (vector-set! res plen (cons (cons preds f) (vector-ref res plen)))
      res)))

;;> Extend the generic \var{g} with a new method \var{func}
;;> that applies when all parameters match the given list
;;> of predicates \var{preds}, or if `variadic?` is true a prefix
;;> of the parameters matches \var{preds}.

(define (generic-add! g preds variadic? func)
  (g add-method-tag preds variadic? func))
