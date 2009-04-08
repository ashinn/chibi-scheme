
(define-syntax aif
  (sc-macro-transformer
   (lambda (form environment)
     (let ((condition
            (make-syntactic-closure environment '() (cadr form)))
           (consequent
            (make-syntactic-closure environment '(it) (caddr form)))
           (alternative
            (make-syntactic-closure environment '() (cadddr form))))
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

