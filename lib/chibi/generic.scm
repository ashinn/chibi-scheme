
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

;;> \subsubsubsection{\scheme{(define-method (name params ...) body ...)}}

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
           (params (map (lambda (param)
                          (if (identifier? param)
                              `(,param (lambda _ #t))
                              param))
                        (cdr (cadr e))))
           (body (cddr e)))
       `(,(r 'generic-add!) ,name
         (,(r 'list) ,@(map cadr params))
         (,(r 'lambda) (,(r 'next) ,@(map car params))
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

(define (make-generic name)
  (let ((name name)
        (methods (make-vector 6 '())))
    (vector-set! methods
                 3
                 (list (cons (list (lambda (x) (eq? x add-method-tag))
                                   (lambda (x) (list? x))
                                   procedure?)
                             (lambda (next t p f)
                               (set! methods (insert-method! methods p f))))))
    (lambda args
      (let ((len (length args)))
        (cond
         ((>= len (vector-length methods))
          (no-applicable-method-error name args))
         (else
          (let lp ((ls (vector-ref methods len)))
            (cond
             ((null? ls)
              (no-applicable-method-error name args))
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

;;> Extend the generic \var{g} with a new method \var{f}
;;> that applies when all parameters match the given list
;;> of predicates \var{preds}.

(define (generic-add! g preds f)
  (g add-method-tag preds f))
