(define id-comparator
  (make-pair-comparator eq-comparator eq-comparator))
(define *identifier-properties* (make-hash-table id-comparator))

(define-syntax define-property
  (lambda (expr use-env mac-env)
    (let* ((id (cadr expr))
           (id-cell
            (or (env-cell use-env id)
                (error "can't attach property to unbound identifier" id)))
           (key (car (cddr expr)))
           (key-cell
            (or (env-cell use-env key)
                (error "identifier property key must be a bound identifier" key)))
           (value (eval (cadr (cddr expr)) use-env))
           (this-id-props
            (hash-table-intern! *identifier-properties*
                                (cons use-env id-cell)
                                (lambda ()
                                  (make-hash-table id-comparator)))))
      (hash-table-set! this-id-props
                       (cons use-env key-cell)
                       value)
      (if #f #f))))

(define (capture-lookup proc)
  (lambda (use-env)
    (proc
     (letrec
         ((f
           (case-lambda
             ((id key)
              (f use-env id use-env key))
             ((id-env id key-env key)
              (let ((cell (env-cell id-env id)))
                (cond ((not cell)
                       (error "identifier not bound in environment"
                              id id-env))
                      ((hash-table-ref/default
                        *identifier-properties*
                        (cons id-env cell)
                        #f)
                       =>
                       (lambda (this-id-props)
                         (hash-table-ref/default
                          this-id-props
                          (cons key-env (env-cell key-env key))
                          #f)))
                      (else #f)))))))
       f))))
