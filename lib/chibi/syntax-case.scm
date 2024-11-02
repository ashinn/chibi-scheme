;; Written by Marc Nieper-Wißkirchen

;; TODO: make-synthetic-identifier should return a truly unique (that
;; is not free-identifier=? to any other) identifier.

;; TODO: Consecutive ellipses in syntax templates.

;; TODO: Write many more tests.

(define current-renamer (make-parameter (lambda (x) x)))
(define current-usage-environment (make-parameter (current-environment)))

(define (free-identifier=? x y)
  (let ((env (or (current-usage-environment) (current-environment))))
    (identifier=? env x env y)))

(define (%make-transformer transformer)
  (cond
   ((and (= 1 (procedure-arity transformer))
         (not (procedure-variadic? transformer)))
    (lambda (expr use-env mac-env)
      (let ((old-use-env (current-usage-environment))
            (old-renamer (current-renamer)))
        (current-usage-environment use-env)
        (current-renamer (make-renamer mac-env))
        (let ((result (transformer expr)))
          (current-usage-environment old-use-env)
          (current-renamer old-renamer)
          result))))
   (else
    (lambda (expr use-env mac-env)
      (let ((old-use-env (current-usage-environment))
            (old-renamer (current-renamer)))
        (current-usage-environment use-env)
        (current-renamer (make-renamer mac-env))
        (let ((result (transformer expr use-env mac-env)))
          (current-usage-environment old-use-env)
          (current-renamer old-renamer)
          result))))))

(define (make-transformer base-transformer)
  (let ((wrapped-transformer (%make-transformer base-transformer)))
    (if (procedure-variable-transformer? base-transformer)
        (make-variable-transformer wrapped-transformer)
        wrapped-transformer)))

(%define-syntax define-syntax
  (lambda (expr use-env mac-env)
    (list (close-syntax '%define-syntax mac-env)
          (cadr expr)
          (list (close-syntax 'make-transformer mac-env)
                (car (cddr expr))))))

(define-syntax let-syntax
  (syntax-rules ()
    ((let-syntax ((keyword transformer) ...) . body)
     (%let-syntax ((keyword (make-transformer transformer)) ...) . body))))

(define-syntax letrec-syntax
  (syntax-rules ()
    ((letrec-syntax ((keyword transformer) ...) . body)
     (%letrec-syntax ((keyword (make-transformer transformer)) ...) . body))))

(define-record-type Pattern-Cell
  (make-pattern-cell val) pattern-cell?
  (val pattern-cell-value))

(define-syntax define-pattern-variable
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((id (cadr expr))
           (binding (cddr expr)))
       (let ((cell (env-cell (current-usage-environment) id)))
         (if cell
             (macro-aux-set! (cdr cell) (make-pattern-cell binding))))
       (rename '(begin))))))

(define (make-pattern-variable pvar)
  (lambda (expr)
    (syntax-violation #f "reference to pattern variable outside syntax" pvar)))

(define (pattern-variable x)
  (and-let*
      ((cell (env-cell (current-usage-environment) x))
       (cell-ref (cdr cell))
       ((macro? cell-ref))
       (aux (macro-aux cell-ref))
       ((pattern-cell? aux)))
    (pattern-cell-value aux)))

(define (rename id)
  ((current-renamer) id))

(define current-ellipsis-id
  (make-syntactic-closure (current-environment) '() 'current-ellipsis))

(define (ellipsis-identifier? id)
  (let* ((cell (env-cell (current-usage-environment) current-ellipsis-id))
         (ellipsis (if cell
                       (macro-aux (cdr cell))
                       (rename '...))))
    (free-identifier=? id ellipsis)))

(define bound-identifier=?
  (lambda (x y)
    (eq? x y)))

(define (syntax-transformer level)
  (er-macro-transformer
   (lambda (expr rename compare)
     (let*-values (((out envs)
                    (gen-template (cadr expr) '() ellipsis-identifier? level)))
       out))))

(define (syntax->datum stx)
  (strip-syntactic-closures stx))

(define-syntax syntax (syntax-transformer #f))
(define-syntax quasisyntax (syntax-transformer 0))
(define-auxiliary-syntax unsyntax)
(define-auxiliary-syntax unsyntax-splicing)

(define (gen-template tmpl envs ell? level)
  (cond
   ((pair? tmpl)
    (cond
     ((and (identifier? (car tmpl))
           (free-identifier=? (car tmpl) (rename 'unsyntax)))
      (if (and level (zero? level))
          (values (cadr tmpl) envs)
          (let*-values (((out envs) (gen-template (cadr tmpl) envs ell? (and level (- level 1)))))
            (values `(,(rename 'list) ,(gen-data (car tmpl)) ,out) envs))))
     ((and (identifier? (car tmpl))
           (free-identifier=? (car tmpl) (rename 'quasisyntax)))
      (let*-values (((out envs) (gen-template (cadr tmpl) envs ell? (and level (+ level 1)))))
        (values `(,(rename 'list) ,(gen-data (car tmpl)) ,out) envs)))
     ((and (pair? (car tmpl))
           (free-identifier=? (caar tmpl) (rename 'unsyntax)))
      (if (and level (zero? level))
          (let*-values (((out envs) (gen-template (cdr tmpl) envs ell? level)))
            (values `(,(rename 'cons*) ,@(cdar tmpl) ,out) envs))
          (let*-values (((out1 envs) (gen-template (cdar tmpl) envs ell? (and level (- level 1))))
                        ((out2 envs) (gen-template (cdr tmpl) envs ell? level)))
            (values `(,(rename 'cons) (,(rename 'cons) ,(gen-data (caar tmpl)) ,out1)
                      ,out2) envs))))
     ((and (pair? (car tmpl))
           (free-identifier=? (caar tmpl) (rename 'unsyntax-splicing)))
      (if (and level (zero? level))
          (let*-values (((out envs) (gen-template (cdr tmpl) envs ell? level)))
            (values `(,(rename 'append) ,@(cdar tmpl) ,out) envs))
          (let*-values (((out1 envs) (gen-template (cdar tmpl) envs ell? (and level (- level 1))))
                        ((out2 envs) (gen-template (cdr tmpl) envs ell? level)))
            (values `(,(rename 'cons) (,(rename 'cons) ,(gen-data (caar tmpl)) ,out1)
                      ,out2) envs))))
     ((and (identifier? (car tmpl))
           (ell? (car tmpl)))
      (gen-template (cadr tmpl) envs (lambda (id) #f) level))
     ((and (pair? (cdr tmpl))
           (identifier? (cadr tmpl))
           (ell? (cadr tmpl)))
      (let*-values (((out* envs)
                     (gen-template (cddr tmpl) envs ell? level))
                    ((out envs)
                     (gen-template (car tmpl) (cons '() envs) ell? level)))
        (if (null? (car envs))
            (syntax-violation 'syntax
                              "too many ellipses following syntax template"
                              (car tmpl)))
        (values `(,(rename 'fold-right) (,(rename 'lambda) (,@(car envs) ,(rename 'stx))
                                         (,(rename 'cons) ,out ,(rename 'stx)))
                  ,out* ,@(car envs))
                (cdr envs))))
     (else
      (let*-values (((out1 envs)
                     (gen-template (car tmpl) envs ell? level))
                    ((out2 envs)
                     (gen-template (cdr tmpl) envs ell? level)))
        (values `(,(rename 'cons) ,out1 ,out2) envs)))))
   ((vector? tmpl)
    (let*-values (((out envs)
                   (gen-template (vector->list tmpl) envs ell? level)))
      (values `(,(rename 'list->vector) ,out) envs)))
   ((identifier? tmpl)
    (cond ((ell? tmpl)
           (syntax-violation 'syntax
                             "misplaced ellipsis in syntax template"
                             tmpl))
          ((pattern-variable tmpl) =>
           (lambda (binding)
             (values (car binding)
                     (update-envs tmpl (car binding) (cadr binding) envs))))
          (else
           (values (gen-data tmpl) envs))))
   (else
    (values `(,(rename 'quote) ,tmpl) envs))))

(define (gen-data id)
  `((,(rename 'current-renamer))
    (,(rename 'syntax-quote) ,id)))

(define (update-envs id x level envs)
  (let loop ((level level) (envs envs))
    (cond ((zero? level)
           envs)
          ((null? envs)
           (syntax-violation #f "too few ellipses following syntax template" id))
          (else
           (let ((outer-envs (loop (- level 1) (cdr envs))))
             (cond ((member x (car envs) bound-identifier=?)
                    envs)
                   (else
                    (cons (cons x (car envs))
                          outer-envs))))))))

(define-syntax syntax-case
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((expr (cadr expr))
           (lit* (car (cddr expr)))
           (clause* (reverse (cdr (cddr expr))))
           (error #`(syntax-violation #f "syntax error" e)))
       #`(let ((e #,expr))
           #,(if (null? clause*)
                 error
                 #`(let ((fail (lambda () #,error)))
                     #,(let loop ((clause (car clause*))
                                  (clause* (cdr clause*)))
                         (if (null? clause*)
                             (gen-clause lit* clause)
                             #`(let ((fail (lambda ()
                                             #,(gen-clause lit* clause))))
                                 #,(loop (car clause*) (cdr clause*))))))))))))

(define (gen-clause lit* clause)
  (if (= 3 (length clause))
      (gen-output lit* (car clause) (cadr clause) (car (cddr clause)))
      (gen-output lit* (car clause) #t (cadr clause))))

(define (gen-output lit* pattern fender output-expr)
  (let*-values (((matcher vars)
                 (gen-matcher #'e lit* pattern '())))
    (matcher
     (lambda ()
       #`(let-syntax
             #,(map (lambda (var)
                      #`(#,(car var)
                         (make-pattern-variable (syntax-quote #,(car var)))))
                    vars)
           #,@(map (lambda (var)
                     #`(define-pattern-variable . #,var))
                   vars)
           (if #,fender
               #,output-expr
               (fail)))))))

(define (gen-matcher e lit* pattern vars)
  (cond
   ((pair? pattern)
    (cond
     ((and (pair? (cdr pattern))
           (identifier? (cadr pattern))
           (ellipsis-identifier? (cadr pattern)))
      (let* ((l (length+ (cddr pattern)))
             (h (car (generate-temporaries '(#f))))
             (t (car (generate-temporaries '(#f)))))
        (let*-values (((head-matcher vars)
                       (gen-map h lit* (car pattern) vars))
                      ((tail-matcher vars)
                       (gen-matcher* t lit* (cddr pattern) vars)))
          (values (lambda (k)
                    #`(let ((n (length+ #,e)))
                        (if (and n (>= n #,l))
                            (let*-values (((#,h #,t) (split-at #,e (- n #,l))))
                              #,(head-matcher (lambda ()
                                                (tail-matcher k))))
                            (fail))))
                  vars))))
     (else
      (let ((e1 (car (generate-temporaries '(#f))))
            (e2 (car (generate-temporaries '(#f)))))
        (let*-values (((car-matcher vars)
                       (gen-matcher e1 lit* (car pattern) vars))
                      ((cdr-matcher vars)
                       (gen-matcher e2 lit* (cdr pattern) vars)))
          (values (lambda (k)
                    #`(if (pair? #,e)
                          (let ((#,e1 (car #,e))
                                (#,e2 (cdr #,e)))
                            #,(car-matcher (lambda ()
                                             (cdr-matcher k))))
                          (fail)))
                  vars))))))
   ((identifier? pattern)
    (cond ((member pattern lit* free-identifier=?)
           (values (lambda (k)
                     #`(if (free-identifier=? #'#,pattern #,e)
                           #,(k)
                           (fail)))
                   vars))
          ((ellipsis-identifier? pattern)
           (syntax-violation #f "misplaced ellipsis" pattern))
          ((free-identifier=? pattern #'_)
           (values (lambda (k)
                     (k))
                   vars))
          (else
           (values (lambda (k)
                     (k))
                   (alist-cons pattern (list e 0) vars)))))
   (else
    (values (lambda (k)
              #`(if (equal? (syntax->datum #,e) '#,pattern)
                    #,(k)
                    (fail)))
            vars))))

(define (gen-map h lit* pattern vars)
  (let*-values (((matcher inner-vars) (gen-matcher #'g lit* pattern '())))
    (let ((loop (car (generate-temporaries '(#f))))
          (g* (generate-temporaries inner-vars)))
      (values
       (lambda (k)
         #`(let #,loop ((#,h (reverse #,h))
                        #,@(map (lambda (g)
                                  #`(#,g '()))
                                g*))
                (if (null? #,h)
                    #,(k)
                    (let ((g (car #,h)))
                      #,(matcher
                         (lambda ()
                           #`(#,loop (cdr #,h)
                                     #,@(map (lambda (var g)
                                               #`(cons #,(cadr var) #,g))
                                             inner-vars g*))))))))
       (fold (lambda (g var vars)
               (alist-cons (car var) (list g (+ (cadr (cdr var)) 1)) vars))
             vars g* inner-vars)))))

(define (gen-matcher* e lit* pattern* vars)
  (let loop ((e e) (pattern* pattern*) (vars vars))
    (cond ((null? pattern*)
           (values (lambda (k)
                     #`(if (null? #,e)
                           #,(k)
                           (fail)))
                   vars))
          ((pair? pattern*)
           (let ((e1 (car (generate-temporaries '(#f))))
                 (e2 (car (generate-temporaries '(#f)))))
             (let*-values (((car-matcher vars)
                            (gen-matcher e1 lit* (car pattern*) vars))
                           ((cdr-matcher vars)
                            (loop e2 (cdr pattern*) vars)))
               (values (lambda (k)
                         #`(let ((#,e1 (car #,e))
                                 (#,e2 (cdr #,e)))
                             #,(car-matcher (lambda ()
                                              (cdr-matcher k)))))
                       vars))))
          (else
           (gen-matcher e lit* pattern* vars)))))

(define (make-synthetic-identifier id)
  (close-syntax id (environment)))

(define (generate-temporaries l)
  (map (lambda (x) (make-synthetic-identifier 't)) l))

(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ ((p e0) ...) e1 e2 ...)
       #'(syntax-case (list e0 ...) ()
           ((p ...) (let () e1 e2 ...)))))))

(define (syntax-violation who message form . maybe-subform)
  (raise (condition (make-syntax-violation form
                                           (if (null? maybe-subform)
                                               #f
                                               (car maybe-subform)))
                    (cond (who => make-who-condition)
                          ((identifier? form)
                           (make-who-condition (syntax->datum form)))
                          ((and (pair? form)
                                (identifier? (car form)))
                           (make-who-condition (syntax->datum (car form))))
                          (else (condition)))
                    (make-message-condition message))))

(define-syntax define-current-ellipsis
  (lambda (stx)
    (syntax-case stx ()
      ((_ ellipsis)
       (let ((mac (cdr (env-cell (current-usage-environment) current-ellipsis-id))))
         (macro-aux-set! mac #'ellipsis))
       #'(begin)))))

(define-syntax with-ellipsis
  (lambda (stx)
    (syntax-case stx ()
      ((_ ellipsis . body)
       (with-syntax ((current-ellipsis current-ellipsis-id))
         #'(let-syntax ((current-ellipsis (syntax-rules ())))
             (define-current-ellipsis ellipsis)
             . body))))))

;; Local variables:
;; eval: (put '%define-syntax 'scheme-indent-function 1)
;; End:
