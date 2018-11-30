(define-syntax out
  (er-macro-transformer
   (lambda (expr rename compare)
     (for-each set-cdr! (car (cddr expr)) (cadr (cddr expr)))
     (car (cdr expr)))))

(%define-syntax syntax-parameterize
  (lambda (expr use-env mac-env)
    (let* ((_let (make-syntactic-closure mac-env '() 'let))
           (_set! (make-syntactic-closure mac-env '() 'set!))
           (_out (make-syntactic-closure mac-env '() 'out))
           (_tmp (make-syntactic-closure mac-env '() 'tmp))
           (bindings (cadr expr))
           (body (cddr expr))
           (keywords (map car bindings))
           (transformers (map cadr bindings))
           (cells
            (map (lambda (keyword)
                   (env-cell use-env keyword))
                 keywords))
           (old (map cdr cells))
           (new (map (lambda (transformer)
                       (make-macro
			(make-transformer
			 (eval
			  (make-syntactic-closure use-env '() transformer)))
                        use-env))
                     transformers)))
      (for-each set-cdr! cells new)
      `(,_let ((,_tmp #f))
         (,_set! ,_tmp (,_let () ,@body))
         (,_out ,_tmp ,cells ,old)))))
