
(define (typed? x)
  (and (lambda? x)
       (lambda-return-type x)))

(define (union-type? a)
  (and (pair? a) (equal? (car a) 'or)))

(define (intersection-type? a)
  (and (pair? a) (equal? (car a) 'and)))

(define (unfinalized-type? a)
  (and (pair? a) (memq (car a) '(return-type param-type))))

(define (numeric-type? a)
  (or (eq? a number) (eq? a flonum) (eq? a integer)))

(define (procedure-type? a)
  (or (eq? a opcode) (eq? a procedure) (and (pair? a) (eq? (car a) 'lambda))))

(define (type-subset? a b)
  (or (equal? a b)
      (equal? a object)
      (equal? b object)
      (and (numeric-type? a) (numeric-type? b))
      (and (procedure-type? a) (procedure-type? b))
      (if (union-type? a)
          (if (union-type? b)
              (lset<= equal? (cdr a) (cdr b))
              (member b (cdr a)))
          (and (union-type? b) (member a (cdr b))))))

;; XXXX check for type hierarchies
(define (type-union a b)
  (cond
   ((equal? a b) a)
   ((or (equal? a object) (equal? b object)) object)
   ((union-type? a)
    (if (union-type? b)
        (cons (car a) (lset-union equal? (cdr a) (cdr b)))
        (cons (car a) (lset-adjoin equal? (cdr a) b))))
   (else (list 'or a b))))

;; XXXX check for conflicts
(define (type-intersection a b)
  (cond
   ((equal? a b) a)
   ((or (equal? a object) (unfinalized-type? a)) b)
   ((or (equal? b object) (unfinalized-type? b)) a)
   ((intersection-type? a)
    (if (intersection-type? b)
        (lset-intersection equal? (cdr a) (cdr b))
        (cons (car a) (lset-adjoin equal? (cdr a) b))))
   (else (list 'and a b))))

(define (lambda-param-types-initialize! f)
  (lambda-param-types-set! f (map (lambda (p) (list 'param-type f p))
                                  (lambda-params f))))

(define (lambda-param-type-memq f x)
  (let lp ((p (lambda-params f))
           (t (lambda-param-types f)))
    (and (pair? p)
         (pair? t)
         (if (eq? x (car p))
             t
             (lp (cdr p) (cdr t))))))

(define (lambda-param-type-ref f x)
  (cond ((lambda-param-type-memq f x) => car)
        (else #f)))

(define (lambda-param-type-set! f x y)
  (if (not (pair? (lambda-param-types f)))
      (lambda-param-types-initialize! f))
  (cond ((lambda-param-type-memq f x)
         => (lambda (cell) (set-car! cell y)))))

(define (type-analyze-expr x)
  ;;(write `(type-analyze-expr ,x ,(ast->sexp x)) (current-error-port)) (newline (current-error-port))
  (match x
    (($ lam name params body defs)
     (lambda-return-type-set! x (list 'return-type x))
     (lambda-param-types-initialize! x)
     (let ((ret-type (type-analyze-expr body)))
       (lambda-return-type-set! x ret-type)
       (cons 'lambda (cons ret-type (lambda-param-types x)))))
    (($ set ref value)
     (type-analyze-expr value)
     (if #f #f))
    (($ ref name (_ . loc) source)
     (if (lambda? loc)
         (lambda-param-type-ref loc name)
         object))
    (($ cnd test pass fail)
     (type-analyze-expr test)
     (type-union (type-analyze-expr pass) (type-analyze-expr fail)))
    (($ seq ls)
     (let lp ((ls ls))
       (cond ((null? (cdr ls))
              (type-analyze-expr (car ls)))
             (else
              (type-analyze-expr (car ls))
              (lp (cdr ls))))))
    ((f args ...)
     (cond
      ((opcode? f)
       ;;(write `(opcode app ,(opcode-param-types f) ,args) (current-error-port)) (newline (current-error-port))
       (let lp ((p (opcode-param-types f))
                (a args))
         (cond
          ((pair? a)
           (cond ((or (pair? p) (opcode-variadic? f))
                  (match (car a)
                   (($ ref name (_ . (and g ($ lam))))
                    (let ((t (type-intersection (lambda-param-type-ref g name)
                                                (if (pair? p)
                                                    (car p)
                                                    (opcode-param-type f (opcode-num-params f))))))
                      (lambda-param-type-set! g name t)))
                   (else
                    (let ((t (type-analyze-expr (car a))))
                      (cond
                       ((not (type-subset? t (car p)))
                        (display "WARNING: incompatible type: " (current-error-port))
                        (write (list x t (car p)) (current-error-port))
                        (newline (current-error-port))))
                      t)))
                  (lp (and (pair? p) (cdr p)) (cdr a)))
                 (else
                  (for-each type-analyze-expr a))))))
       (opcode-return-type f))
      (else
       (let ((f-type (type-analyze-expr f)))
         ;; XXXX apply f-type to params
         (for-each type-analyze-expr args)
         (if (and (pair? f-type) (eq? 'lambda (car f-type)))
             (cadr f-type)
             object)))))
    (else
     ;;(write `(unknown type ,x) (current-error-port)) (newline (current-error-port))
     object)))

(define (type-resolve-circularities x)
  #f)

;; basic type inference on the body of a module
;;  - internal references are to lambdas
;;  - external references are to procedures (with completed type info)
;;  - for each lambda
;;    + add parameter constraints (intersection) from body
;;    + add return type constaints (union) from last form(s)
;;  - when complete, resolve cycles (e.g. even/odd => boolean)
(define (type-analyze-module-body name ls)
  ;;(write `(type-analyze-module-body ,name) (current-error-port)) (newline (current-error-port))
  (for-each type-analyze-expr ls)
  (for-each type-resolve-circularities ls))

(define (type-analyze-module name)
  (let* ((mod (analyze-module name))
         (ls (and (vector? mod) (module-ast mod))))
    ;;(write `(analyzing ,ls) (current-error-port)) (newline (current-error-port))
    (and ls
         (let ((x (let lp ((ls ls)) ;; first lambda
                    (and (pair? ls)
                         (if (and (set? (car ls))
                                  (lambda? (set-value (car ls))))
                             (set-value (car ls))
                             (lp (cdr ls)))))))
           (if (and x (not (typed? x)))
               (type-analyze-module-body name ls))
           ls))))

(define (type-analyze sexp . o)
  (type-analyze-expr (apply analyze sexp o)))

(define (opcode-param-types x)
  (let lp ((n (- (opcode-num-params x) 1)) (res '()))
    (if (< n 0)
        res
        (lp (- n 1) (cons (opcode-param-type x n) res)))))

(define (procedure-signature x)
  (if (opcode? x)
      (cons (opcode-return-type x) (opcode-param-types x))
      (let lp ((count 0))
        (let ((lam (procedure-analysis x)))
          (cond
           ((and lam (not (typed? lam)) (zero? count)
                 (containing-module x))
            => (lambda (mod)
                 (and (type-analyze-module (car mod))
                      (lp (+ count 1)))))
           ((lambda? lam)
            (cons (lambda-return-type lam)
                  (lambda-param-types lam)))
           (else
            #f))))))
