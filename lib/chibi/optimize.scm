
(define (register-lambda-optimization! proc . o)
  (define (optimize ast)
    (match ast
      (($ Set ref value)
       (make-set ref (optimize value)))
      (($ Cnd test pass fail)
       (make-cnd (optimize test) (optimize pass) (optimize fail)))
      (($ Seq ls)
       (make-seq (map optimize ls)))
      (($ Lam name params body)
       (lambda-body-set! ast (optimize body))
       (proc ast))
      ((app ...)
       (map optimize app))
      (else
       ast)))
  (register-optimization! optimize (if (pair? o) (car o) 600)))

(define (replace-references ast name lam new)
  (let replace ((x ast))
    (match x
      (($ Ref _ (n . (? lambda? f)))
       (if (and (eq? n name) (eq? f lam))
           new
           x))
      (($ Set ref value)
       (make-set (replace ref) (replace value)))
      (($ Cnd test pass fail)
       (make-cnd (replace test) (replace pass) (replace fail)))
      (($ Seq ls)
       (make-seq (map replace ls)))
      (($ Lam name params body)
       (lambda-body-set! x (replace body))
       x)
      ((app ...)
       (map replace app))
      (else
       x))))

(define (join-seq a b)
  (make-seq (append (if (seq? a) (seq-ls a) (list a))
                    (if (seq? b) (seq-ls b) (list b)))))

(define (dotted-tail ls)
  (if (pair? ls) (dotted-tail (cdr ls)) ls))

(define (fold-every kons knil ls)
  (if (null? ls)
      knil
      (let ((knil (kons (car ls) knil)))
        (and knil (fold-every kons knil (cdr ls))))))
