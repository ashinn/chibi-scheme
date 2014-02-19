
(define (optimize-rest ast)
  (cond
   ((and (lambda? ast)
         (not (list? (lambda-params ast)))
         (rest-parameter-cdrs ast))
    => (lambda (cdrs)
         (replace-rest-destructuring-with-stack-references
          (length* (lambda-params ast))
          ast
          cdrs)))
   (else
    ast)))

(define safe-primitives (list car cdr null? pair?))

(define (adjust-cdrs cdrs f params args)
  (filter-map
   (lambda (p a)
     (match a
       (((? (lambda (op) (eq? op cdr))) ($ Ref name (_ . (? lambda? lam))))
        (let ((x (find (lambda (r)
                         (and (eq? name (car r)) (eq? lam (cadr r))))
                       cdrs)))
          (and x (list p f (+ (car (cddr x)) 1)))))
       (($ Cnd
           ((? (lambda (op) (eq? op pair?))) ($ Ref name (_ . (? lambda? lam))))
           ((? (lambda (op) (eq? op cdr))) ($ Ref name (_ . (? lambda? lam))))
           (or () ($ Lit ())))
        (let ((x (find (lambda (r)
                         (and (eq? name (car r)) (eq? lam (cadr r))))
                       cdrs)))
          (and x (list p f (+ (car (cddr x)) 1.0)))))
       (else #f)))
   params
   args))

(define (rest-parameter-cdrs ast)
  (let analyze ((x (lambda-body ast))
                (cdrs (list (list (dotted-tail (lambda-params ast)) ast 0)))
                (safe? #t))
    (define (recurse x cdrs) (analyze x cdrs safe?))
    (match x
      (($ Ref name (_ . (? lambda? f)))
       (and (not (any (lambda (r) (and (eq? name (car r)) (eq? f (cadr r)))) cdrs))
            cdrs))
      (($ Set ref value)
       (and (recurse ref cdrs) (recurse value cdrs)))
      (($ Cnd test pass fail)
       (fold-every recurse cdrs (list test pass fail)))
      (($ Seq ls)
       (fold-every recurse cdrs ls))
      (($ Lam name params body)
       (analyze body cdrs #f))
      (((and ($ Lam _ (params ...) body) f) args ...)
       (let ((cdrs (fold-every recurse cdrs args)))
         (and (equal? (length params) (length args))
              (recurse body (append (adjust-cdrs cdrs f params args) cdrs)))))
      (((? opcode? op) ($ Ref _ (_ . (? lambda?))))
       (if (and safe? (memq op safe-primitives))
           cdrs
           (recurse (cadr x) cdrs)))
      ((app ...)
       (fold-every recurse cdrs app))
      (else
       cdrs))))

(define (replace-rest-destructuring-with-stack-references base ast cdrs)
  (define (rename p)
    (make-syntactic-closure
     (current-environment) '() (strip-syntactic-closures p)))
  (define (replace-param x)
    (match x
      (($ Cnd test pass fail)
       (make-cnd (replace-param test)
                 (replace-param pass)
                 (replace-param fail)))
      (($ Seq ls)
       (let ((ls (map replace-param ls)))
         (and ls (make-seq ls))))
      (((? opcode? op) ($ Ref name (_ . (? lambda? f))))
       (let ((r (and (memq op safe-primitives)
                     (find (lambda (r) (and (eq? name (car r)) (eq? f (cadr r))))
                           cdrs))))
         (cond
          ((not r)
           x)
          ((eq? op car)
           `(,local-ref ,(+ 1 (inexact->exact (car (cddr r))))))
          ((eq? op cdr)
           (make-lit '()))
          ((eq? op pair?)
           `(,> (,num-parameters) ,(+ base (inexact->exact (car (cddr r))))))
          ((eq? op null?)
           `(,<= (,num-parameters) ,(+ base (inexact->exact (car (cddr r))))))
          (else
           x))))
      (($ Set ref value)
       #f)
      (($ Lam name params body)
       #f)
      ((app ...)
       #f)
      (else
       x)))
  (lambda-body-set!
   ast
   (let replace ((x (lambda-body ast)))
     (match x
       ((($ Lam name (params ...) body) args ...)
        (let* ((locals (map replace-param args))
               (names (map rename params))
               (refs (map (lambda (name) (make-ref name (cons name ast))) names)))
          (let ((res (fold (lambda (p new res)
                             (replace-references res p (car x) new))
                           (replace body)
                           params
                           refs)))
            (lambda-locals-set! ast (append names (lambda-locals ast)))
            (join-seq (make-seq (map make-set refs locals))
                      res))))
       (else
        x))))
  ast)

(register-lambda-optimization! optimize-rest)
