
(define-syntax define-record-type
  (er-macro-transformer
   (lambda (expr rename compare)
     (let* ((id->string (lambda (x) (symbol->string (identifier->symbol x))))
            (name (if (pair? (cadr expr)) (caadr expr) (cadr expr)))
            (parent (and (pair? (cadr expr)) (cadadr expr)))
            (name-str (id->string name))
            (make (caddr expr))
            (make-name (if (eq? make #t)
                           (string->symbol (string-append "make-" name-str))
                           (if (pair? make) (car make) make)))
            (pred (cadddr expr))
            (pred-name (if (eq? pred #t)
                           (string->symbol (string-append name-str "?"))
                           pred))
            (fields (cddddr expr))
            (field-names (map (lambda (x) (if (pair? x) (car x) x)) fields))
            (make-fields (if (pair? make) (cdr make) (and (not parent) field-names)))
            (_define (rename 'define))
            (_lambda (rename 'lambda))
            (_let (rename 'let))
            (_register (rename 'register-simple-type))
            (_slot-set! (rename 'slot-set!))
            (_vector->list (rename 'vector->list))
            (_type_slot_offset (rename 'type-slot-offset))
            (_rtd-all-field-names (rename 'rtd-all-field-names)))
       `(,(rename 'begin)
         ;; type
         (,_define ,name (,_register ,name-str ,parent ',field-names))
         ;; predicate
         ,@(if pred-name
               `((,_define ,pred-name
                   (,(rename 'make-type-predicate)
                    ,(id->string pred-name)
                    ,name)))
               #f)
         ;; accessors
         ,@(map (lambda (f)
                  (let ((g (if (and (pair? f) (pair? (cdr f)))
                               (cadr f)
                               (and (identifier? f)
                                    (string->symbol
                                     (string-append name-str "-" (id->string f)))))))
                    (and g
                         `(,_define ,g
                            (,(rename 'make-getter)
                             ,(id->string g)
                             ,name
                             (,_type_slot_offset ,name ',(if (pair? f) (car f) f)))))))
                fields)
         ,@(map (lambda (f)
                  (let ((s (if (and (pair? f) (pair? (cdr f)) (pair? (cddr f)))
                               (caddr f)
                               (and (identifier? f)
                                    (string->symbol
                                     (string-append name-str "-" (id->string f) "-set!"))))))
                    (and s
                         `(,_define ,s
                            (,(rename 'make-setter)
                             ,(id->string s)
                             ,name
                             (,_type_slot_offset ,name ',(if (pair? f) (car f) f)))))))
                fields)
         ;; constructor
         ,(if make-fields
              `(,_define ,make-name
                 ,(let lp ((ls make-fields) (sets '()))
                    (cond
                     ((null? ls)
                      `(,_let ((%make (,(rename 'make-constructor)
                                       ,(id->string make-name)
                                       ,name)))
                         (,_lambda ,make-fields
                           (,_let ((res (%make)))
                             ,@sets
                             res))))
                     (else
                      (let ((field (assq (car ls) fields)))
                        (cond
                         ;;((not field)
                         ;; (error "unknown record field in constructor" (car ls)))
                         ((and (pair? field) (pair? (cdr field)) (pair? (cddr field)))
                          (lp (cdr ls)
                              (cons (list (caddr field) 'res (car ls)) sets)))
                         (else
                          (lp (cdr ls)
                              (cons `(,_slot-set! ,name res (,_type_slot_offset ,name ',(car ls)) ,(car ls)) sets)))))))))
              `(,_define ,make-name
                 (,_let ((%make (,(rename 'make-constructor)
                                 ,(id->string make-name)
                                 ,name)))
                   (,_lambda args
                     (,_let ((res (%make)))
                       (let lp ((a args)
                                (p (,_vector->list (,_rtd-all-field-names ,name))))
                         (cond
                          ((null? a)
                           (if (null? p)
                               res
                               (error ,(string-append "not enough arguments to " (id->string make-name) ": missing")
                                      p)))
                          ((null? p)
                           (error ,(string-append "too many arguments to " (id->string make-name))
                                  a))
                          (else
                           (,_slot-set! ,name res (,_type_slot_offset ,name (car p)) (car a))
                           (lp (cdr a) (cdr p)))))))))))))))
