
(define-syntax define-record-type
  (er-macro-transformer
   (lambda (expr rename compare)
     (let* ((name (if (pair? (cadr expr)) (caadr expr) (cadr expr)))
            (parent (and (pair? (cadr expr)) (cadadr expr)))
            (name-str (symbol->string (identifier->symbol name)))
            (make (caaddr expr))
            (make-fields (cdaddr expr))
            (pred (cadddr expr))
            (fields (cddddr expr))
            (_define (rename 'define))
            (_lambda (rename 'lambda))
            (_let (rename 'let))
            (_register (rename 'register-simple-type)))
       (define (index-of field ls)
         (let lp ((ls ls) (i 0))
           (if (eq? field (caar ls)) i (lp (cdr ls) (+ i 1)))))
       (write `(name: ,name parent: ,parent)) (newline)
       `(,(rename 'begin)
         ;; type
         (,_define ,name (,_register ,name-str ,parent ',fields))
         ;; predicate
         (,_define ,pred (,(rename 'make-type-predicate)
                          ,(symbol->string (identifier->symbol pred))
                          ,name))
         ;; fields
         ,@(let lp ((ls fields) (i 0) (res '()))
             (if (null? ls)
                 res
                 (let ((res
                        (cons `(,_define ,(cadar ls)
                                 (,(rename 'make-getter)
                                  ,(symbol->string
                                    (identifier->symbol (cadar ls)))
                                  ,name
                                  ,i))
                              res)))
                   (lp (cdr ls)
                       (+ i 1)
                       (if (pair? (cddar ls))
                           (cons
                            `(,_define ,(caddar ls)
                               (,(rename 'make-setter)
                                ,(symbol->string
                                  (identifier->symbol (caddar ls)))
                                ,name
                                ,i))
                            res)
                           res)))))
         ;; constructor
         (,_define ,make
           ,(let lp ((ls make-fields) (sets '()) (set-defs '()))
              (cond
               ((null? ls)
                `(,_let ((%make (,(rename 'make-constructor)
                                 ,(symbol->string (identifier->symbol make))
                                 ,name))
                         ,@set-defs)
                   (,_lambda ,make-fields
                     (,_let ((res (%make)))
                       ,@sets
                       res))))
               (else
                (let ((field (assq (car ls) fields)))
                  (cond
                   ((not field)
                    (error "unknown record field in constructor" (car ls)))
                   ((pair? (cddr field))
                    (lp (cdr ls)
                        (cons (list (caddr field) 'res (car ls)) sets)
                        set-defs))
                   (else
                    (let* ((setter-name
                            (string-append "%" name-str "-"
                                           (symbol->string (car ls)) "-set!"))
                           (setter (rename (string->symbol setter-name)))
                           (i (index-of (car ls) fields)))
                      (lp (cdr ls)
                          (cons (list setter 'res (car ls)) sets)
                          (cons (list setter
                                      (list (rename 'make-setter)
                                            setter-name
                                            name
                                            (index-of (car ls) fields)))
                                set-defs))))))))))
         (display "done\n"))))))
