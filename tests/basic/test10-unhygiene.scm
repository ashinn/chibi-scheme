
(define-syntax aif
  (sc-macro-transformer
   (lambda (form environment)
     (let ((condition
            (make-syntactic-closure environment '() (cadr form)))
           (consequent
            (make-syntactic-closure environment '(it) (car (cddr form))))
           (alternative
            (make-syntactic-closure environment '() (cadr (cddr form)))))
       `(let ((it ,condition))
             (if it
                 ,consequent
                 ,alternative))))))

(write (aif 1 it 3))
(newline)

(write (let ((it 4)) (aif 1 it 3)))
(newline)

(write (let ((it 4)) (aif (let ((it 5)) 1) it 3)))
(newline)

(write (let ((it 4)) (aif (let ((it 5)) 1) (let ((it 6)) it) 3)))
(newline)

(write
 (letrec-syntax
     ((myor
       (er-macro-transformer
        (lambda (expr rename compare)
          (if (null? (cdr expr))
              #f
              (list (rename 'let) (list (list (rename 'it) (cadr expr)))
                    (list (rename 'if) (rename 'it)
                          (rename 'it)
                          (cons (rename 'myor) (cddr expr)))))))))
   (let ((it 7)) (myor #f it))))
(newline)

(define-syntax define-foo
  (sc-macro-transformer
   (lambda (form environment)
     (make-syntactic-closure environment '(foo) `(define foo 8)))))

(define-foo)
(write foo)
(newline)
