(define (constantly . args)
  (lambda ignored-args
    (apply values args)))

(define (complement proc)
  (lambda (obj)
    (not (proc obj))))

(define (swap proc)
  (lambda (obj1 obj2 . rest)
    (apply proc obj2 obj1 rest)))

(define (flip proc)
  (lambda args
    (apply proc (reverse args))))

(define (on-left proc)
  (lambda (obj1 obj2)
    (proc obj1)))

(define (on-right proc)
  (lambda (obj1 obj2)
    (proc obj2)))

(define (conjoin . predicates)
  (lambda args
      (every (lambda (proc) (apply proc args)) predicates)))

(define (disjoin . predicates)
  (lambda args
      (any (lambda (proc) (apply proc args)) predicates)))

(define (each-of . procs)
  (lambda args
    (for-each
     (lambda (proc) (apply proc args))
     procs)))

(define (all-of predicate)
  (lambda (lst)
    (let loop ((lst lst)
               (last #t))
      (cond
       ((null? lst) last)
       ((predicate (car lst)) => (lambda (value)
                                   (loop (cdr lst) value)))
       (else #f)))))

(define (any-of predicate)
  (lambda (lst)
    (if (null? lst)
        #f
        (let loop ((lst lst))
          (cond
           ((null? lst) #f)
           ((predicate (car lst)))
           (else (loop (cdr lst))))))))

(define (on reducer mapper)
  (lambda objs
    (apply reducer (map mapper objs))))

(define (left-section proc . args)
  (lambda objs
    (apply proc (append args objs))))

(define (right-section proc . args)
  (let ((args-reverse (reverse args)))
    (lambda objs
      (apply proc (append objs args-reverse)))))

(define (apply-chain . procs)
  (define procs/rev (reverse procs))
  (lambda args
    (let loop ((values-provider (lambda () (apply values args)))
               (procs procs/rev))
      (if (null? procs)
          (values-provider)
          (loop (lambda ()
                  (call-with-values
                    values-provider
                    (car procs)))
                (cdr procs))))))

(define (arguments-drop/take proc drop/take n)
  (lambda args
    (apply proc (drop/take args n))))

(define (arguments-drop proc n)
  (arguments-drop/take proc drop n))

(define (arguments-drop-right proc n)
  (arguments-drop/take proc drop-right n))

(define (arguments-take proc n)
  (arguments-drop/take proc take n))

(define (arguments-take-right proc n)
  (arguments-drop/take proc take-right n))

(define group-by
  (case-lambda
    ((key-proc) (group-by key-proc equal?))
    ((key-proc =)
     (lambda (lst)
       (let loop ((lst lst)
                  (mapping-alist '()))
         (cond
           ((null? lst)
            (reverse
              (map
                (lambda (entry)
                  (reverse (cdr entry)))
                mapping-alist)))
           (else (let* ((value (car lst))
                        (key (key-proc value)))
                   (cond
                     ((assoc key mapping-alist =) => (lambda (entry)
                                                       (set-cdr! entry (cons value (cdr entry)))
                                                       (loop (cdr lst)
                                                             mapping-alist)))
                     (else (loop (cdr lst)
                                 (cons (cons key (list value))
                                       mapping-alist))))))))))))

(define (begin-procedure . thunks)
  (let loop ((value (if #f #f))
             (thunks thunks))
    (if (null? thunks)
        value
        (loop ((car thunks))
              (cdr thunks)))))

(define (if-procedure value then-thunk else-thunk)
  (if value
      (then-thunk)
      (else-thunk)))

(define (when-procedure value . thunks)
  (when value
    (for-each
     (lambda (fn) (fn))
     thunks)))

(define (unless-procedure value . thunks)
  (unless value
    (for-each
     (lambda (fn) (fn))
     thunks)))

(define (value-procedure value then-proc else-thunk)
  (if value
      (then-proc value)
      (else-thunk)))

(define case-procedure
  (case-lambda
    ((value thunk-alist) (case-procedure value thunk-alist (lambda args (if #f #f))))
    ((value thunk-alist else-thunk)
     (cond
      ((assv value thunk-alist) => (lambda (entry)
                                     ((cdr entry))))
      (else (else-thunk))))))

(define and-procedure
  (case-lambda
    (() #t)
    (thunks (let loop ((thunks thunks))
              (cond
               ((null? (cdr thunks)) ((car thunks)))
               ((not ((car thunks))) #f)
               (else (loop (cdr thunks))))))))

(define eager-and-procedure
  (case-lambda
    (() #t)
    (thunks (let loop ((thunks thunks)
                       (result #t))
              (cond
               ((null? (cdr thunks)) (let ((r ((car thunks))))
                                      (and result r)))
               ((not ((car thunks))) (loop (cdr thunks) #f))
               (else (loop (cdr thunks) result)))))))

(define or-procedure
  (case-lambda
    (() #f)
    (thunks (let loop ((thunks thunks))
              (cond
               ((null? thunks) #f)
               (((car thunks)) => values)
               (else (loop (cdr thunks))))))))

(define eager-or-procedure
  (case-lambda
    (() #f)
    (thunks (let loop ((thunks thunks)
                       (result #f))
              (cond
               ((null? thunks) result)
               (((car thunks)) => (lambda (res)
                                    (loop (cdr thunks)
                                          (or result
                                              res))))
               (else (loop (cdr thunks) result)))))))

(define (funcall-procedure thunk)
  (thunk))

(define (loop-procedure thunk)
  (thunk)
  (loop-procedure thunk))

(define (while-procedure thunk)
  (if (thunk)
      (while-procedure thunk)
      #f))

(define (until-procedure thunk)
  (define v (thunk))
  (if v
      v
      (until-procedure thunk)))

(define (always . args) #t)

(define (never . args) #f)

(define (boolean obj)
  (if obj #t #f))
