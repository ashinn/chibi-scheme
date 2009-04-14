
(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((lits (cadr expr))
           (forms (cddr expr))
           (count 0)
           (_er-macro-transformer (rename 'er-macro-transformer))
           (_lambda (rename 'lambda))      (_let (rename 'let))
           (_begin (rename 'begin))        (_if (rename 'if))
           (_and (rename 'and))            (_or (rename 'or))
           (_eq? (rename 'eq?))            (_equal? (rename 'equal?))
           (_car (rename 'car))            (_cdr (rename 'cdr))
           (_cons (rename 'cons))          (_pair? (rename 'pair?))
           (_null? (rename 'null?))        (_expr (rename 'expr))
           (_rename (rename 'rename))      (_compare (rename 'compare))
           (_quote (rename 'quote))        (_apply (rename 'apply))
           (_append (rename 'append))      (_map (rename 'map))
           (_vector? (rename 'vector?))    (_list? (rename 'list?))
           (_lp (rename 'lp))              (_reverse (rename 'reverse))
           (_vector->list (rename 'vector->list))
           (_list->vector (rename 'list->vector)))
       (define (next-v)
         (set! count (+ count 1))
         (rename (string->symbol (string-append "v." (number->string count)))))
       (define (expand-pattern pat tmpl)
         (let lp ((p (cdr pat))
                  (x (list _cdr _expr))
                  (dim 0)
                  (vars '())
                  (k (lambda (vars)
                       (or (expand-template tmpl vars)
                           (list _begin #f)))))
           (let ((v (next-v)))
             (list
              _let (list (list v x))
              (cond
               ((identifier? p)
                (if (any (lambda (l) (compare p l)) lits)
                    (list _and (list _compare v (list _quote p)) (k vars))
                    (list _let (list (list p v)) (k (cons (cons p dim) vars)))))
               ((ellipse? p)
                (cond
                 ((not (null? (cddr p)))
                  (error "non-trailing ellipse"))
                 ((identifier? (car p))
                  (list _and (list _list? v)
                        (list _let (list (list (car p) v))
                              (k (cons (cons (car p) (+ 1 dim)) vars)))))
                 (else
                  (let* ((w (next-v))
                         (new-vars (all-vars (car p) (+ dim 1)))
                         (ls-vars (map (lambda (x)
                                         (rename
                                          (string->symbol
                                           (string-append
                                            (symbol->string
                                             (identifier->symbol (car x)))
                                            "-ls"))))
                                       new-vars))
                         (once
                          (lp (car p) (list _car w) (+ dim 1) '()
                              (lambda (_)
                                (cons
                                 _lp
                                 (cons
                                  (list _cdr w)
                                  (map (lambda (x l)
                                         (list _cons (car x) l))
                                       new-vars
                                       ls-vars)))))))
                    (list
                     _let
                     _lp (cons (list w v)
                               (map (lambda (x) (list x '())) ls-vars))
                     (list _if (list _null? w)
                           (list _let (map (lambda (x l)
                                             (list (car x) (list _reverse l)))
                                           new-vars
                                           ls-vars)
                                 (k (append new-vars vars)))
                           (list _and (list _pair? w) once)))))))
               ((pair? p)
                (list _and (list _pair? v)
                      (lp (car p)
                          (list _car v)
                          dim
                          vars
                          (lambda (vars)
                            (lp (cdr p) (list _cdr v) dim vars k)))))
               ((vector? p)
                (list _and
                      (list _vector? v)
                      (lp (vector->list p) (list _vector->list v) dim vars k)))
               ((null? p) (list _and (list _null? v) (k vars)))
               (else (list _and (list _equal? v p) (k vars))))))))
       (define (ellipse? x)
         (and (pair? x) (pair? (cdr x)) (compare '... (cadr x))))
       (define (ellipse-depth x)
         (if (ellipse? x)
             (+ 1 (ellipse-depth (cdr x)))
             0))
       (define (ellipse-tail x)
         (if (ellipse? x)
             (ellipse-tail (cdr x))
             (cdr x)))
       (define (all-vars x dim)
         (let lp ((x x) (dim dim) (vars '()))
           (cond ((identifier? x) (if (memq x (list _quote lits))
                                      vars
                                      (cons (cons x dim) vars)))
                 ((ellipse? x) (lp (car x) (+ dim 1) vars))
                 ((pair? x) (lp (car x) dim (lp (cdr x) dim vars)))
                 ((vector? x) (lp (vector->list x) dim vars))
                 (else vars))))
       (define (free-vars x vars dim)
         (let lp ((x x) (free '()))
           (cond
            ((identifier? x)
             (if (and (not (memq x free))
                      (cond ((assq x vars) => (lambda (cell) (>= (cdr cell) dim)))
                            (else #f)))
                 (cons x free)
                 free))
            ((pair? x) (lp (car x) (lp (cdr x) free)))
            ((vector? x) (lp (vector->list x) free))
            (else free))))
       (define (expand-template tmpl vars)
         (let lp ((t tmpl) (dim 0))
           (cond
            ((identifier? t)
             (cond
              ((assq t vars)
               => (lambda (cell)
                    (if (<= (cdr cell) dim)
                        t
                        (error "too few ...'s"))))
              (else
               (list _rename (list _quote t)))))
            ((pair? t)
             (if (ellipse? t)
                 (let* ((depth (ellipse-depth t))
                        (ell-dim (+ dim depth))
                        (ell-vars (free-vars (car t) vars ell-dim)))
                   (if (null? ell-vars)
                       (error "too many ...'s")
                       (let* ((once (lp (car t) ell-dim))
                              (nest (if (and (null? (cdr ell-vars))
                                             (identifier? once)
                                             (eq? once (car vars)))
                                        once ;; shortcut
                                        (cons _map
                                              (cons (list _lambda ell-vars once)
                                                    ell-vars))))
                              (many (do ((d depth (- d 1))
                                         (many nest
                                               (list _apply _append many)))
                                        ((= d 1) many))))
                         (if (null? (ellipse-tail t))
                             many ;; shortcut
                             (list _append many (lp (ellipse-tail t) dim))))))
                 (list _cons (lp (car t) dim) (lp (cdr t) dim))))
            ((vector? t) (list _list->vector (lp (vector->list t) dim)))
            ((null? t) (list _quote '()))
            (else t))))
       (list
        _er-macro-transformer
        (list _lambda (list _expr _rename _compare)
              (cons
               _or
               (append
                (map
                 (lambda (clause) (expand-pattern (car clause) (cadr clause)))
                 forms)
                (list (list 'error "no expansion"))))))))))

;; Local Variables:
;; eval: (put '_lambda 'scheme-indent-function 1)
;; eval: (put '_let 'scheme-indent-function 'scheme-let-indent)
;; eval: (put '_if 'scheme-indent-function 3)
;; End:

