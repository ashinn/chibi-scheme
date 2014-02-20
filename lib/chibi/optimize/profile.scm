
(define (ref=? a b)
  (or (eq? a b)
      (and (ref? a) (ref? b)
           (eq? (ref-name a) (ref-name b))
           (eq? (car (ref-cell a)) (car (ref-cell b)))
           (eq? (cdr (ref-cell a)) (cdr (ref-cell b))))))

(define profile-cells '())

(define (profile-get-cell f)
  (or (assoc f profile-cells ref=?)
      (let ((cell (cons f 0)))
        (set! profile-cells (cons cell profile-cells))
        cell)))

(define (profile-reset)
  (for-each (lambda (x) (set-cdr! x 0)) profile-cells))

(define (profile-report)
  (define (report-op op)
    (match op
      (($ Ref name (p . (and ($ Lam lam-name) f)))
       (write name)
       (cond
        ((not (eq? p name))
         (display " ")
         (write p)))
       (cond
        ((lambda-source f)
         (display " [") (write (lambda-source f)) (display "]"))))
      (($ Ref name (_ . f))
       (write name) (display " (") (write f) (display ")"))
      (else
       (write op))))
  (let ((ls (filter (lambda (x) (> (cdr x) 0))
                    profile-cells)))
    (for-each (lambda (x)
                (write (cdr x)) (display ": ")
                (report-op (car x)) (newline))
              (sort ls > cdr))))

(define (optimize-profile ast)
  (let-syntax ((opt (syntax-rules () ((opt x) (optimize-profile x)))))
    (match ast
      (($ Set ref value)
       (set-value-set! ast (opt value))
       ast)
      (($ Cnd test pass fail)
       (make-cnd (opt test) (opt pass) (opt fail)))
      (($ Seq ls)
       (make-seq (map optimize-profile ls)))
      (($ Lam name params body)
       (lambda-body-set! ast (opt body))
       ast)
      ((($ Ref name cell) args ...)
       (make-seq (list (list increment-cdr!
                             (make-lit (profile-get-cell (car ast))))
                       (cons (car ast) (map optimize-profile args)))))
      ((app ...)
       (map optimize-profile app))
      (else
       ast))))

(register-lambda-optimization! optimize-profile)
