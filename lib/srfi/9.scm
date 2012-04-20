
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
            (_register (rename 'register-simple-type))
            (_slot-set! (rename 'slot-set!))
            (_type_slot_offset (rename 'type-slot-offset)))
       `(,(rename 'begin)
         ;; type
         (,_define ,name (,_register ,name-str ,parent ',(map car fields)))
         ;; predicate
         (,_define ,pred (,(rename 'make-type-predicate)
                          ,(symbol->string (identifier->symbol pred))
                          ,name))
         ;; fields
         ,@(map (lambda (f)
                  (and (pair? f) (pair? (cdr f))
                       `(,_define ,(cadr f)
                          (,(rename 'make-getter)
                           ,(symbol->string
                             (identifier->symbol (cadr f)))
                           ,name
                           (,_type_slot_offset ,name ',(car f))))))
                fields)
         ,@(map (lambda (f)
                  (and (pair? f) (pair? (cdr f)) (pair? (cddr f))
                       `(,_define ,(caddr f)
                          (,(rename 'make-setter)
                           ,(symbol->string
                             (identifier->symbol (caddr f)))
                           ,name
                           (,_type_slot_offset ,name ',(car f))))))
                fields)
         ;; constructor
         (,_define ,make
           ,(let lp ((ls make-fields) (sets '()))
              (cond
               ((null? ls)
                `(,_let ((%make (,(rename 'make-constructor)
                                 ,(symbol->string (identifier->symbol make))
                                 ,name)))
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
                        (cons `(,(caddr field) res ,(car ls)) sets)))
                   (else
                    (lp (cdr ls)
                        (cons `(,_slot-set! ,name res (,_type_slot_offset ,name ',(car ls)) ,(car ls))
                              sets))))))))))))))
