
(write (or 1))
(newline)
(write (or #f 2))
(newline)
(write (or 3 #t))
(newline)

(let ((tmp 4))
  (write (or #f tmp))
  (newline))

(write
 (letrec-syntax
     ((myor
       (er-macro-transformer
        (lambda (expr rename compare)
          (if (null? (cdr expr))
              #f
              (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                    (list (rename 'if) (rename 'tmp)
                          (rename 'tmp)
                          (cons (rename 'myor) (cddr expr)))))))))
   (let ((tmp 5)) (myor #f tmp))))
(newline)

(define-syntax myor
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr))
         #f
         (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
               (list (rename 'if) (rename 'tmp)
                     (rename 'tmp)
                     (cons (rename 'myor) (cddr expr))))))))

(write (let ((tmp 6)) (myor #f tmp)))
(newline)

(let ((x 'outer))
  (let-syntax ((with-x
                (syntax-rules ()
                  ((_ y expr)
                   (let-syntax ((y (syntax-rules () ((_) x))))
                     expr)))))
    (let ((x 'inner))
      (write (with-x z (z)))
      (newline)))) 
