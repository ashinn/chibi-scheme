
(define (file->sexp-list file)
  (call-with-input-file file
    (lambda (in)
      (let lp ((res '()))
        (let ((x (read in)))
          (if (eof-object? x)
              (reverse res)
              (lp (cons x res))))))))

;; load the module and return it with a list of all top-level forms in
;; the module analyzed
(define (analyze-module name . o)
  (let ((recursive? (and (pair? o) (car o)))
        (modules `(((scheme) . ,(find-module '(scheme))))))
    (let go ((name name))
      (let ((env (make-environment))
            (dir (module-name-prefix name)))
        (define (load-modules files extension)
          (for-each
           (lambda (f)
             (let ((f (string-append dir f extension)))
               (cond ((find-module-file f) => (lambda (x) (load x env)))
                     (else (error "couldn't find include" f)))))
           files))
        (define (include-source file)
          (cond ((find-module-file (string-append dir file))
                 => (lambda (x) (cons 'body (file->sexp-list x))))
                (else (error "couldn't find include" file))))
        (cond
         ((assoc name modules) => cdr)
         (else
          (let ((mod (find-module name)))
            (let lp ((ls (module-meta-data mod)) (res '()))
              (cond
               ((not (pair? ls)) (reverse res))
               (else
                (case (and (pair? (car ls)) (caar ls))
                  ((import import-immutable)
                   (for-each
                    (lambda (m)
                      (let* ((mod2-name+imports (resolve-import m))
                             (mod2 (load-module (car mod2-name+imports))))
                        (%env-copy! env (module-env mod2) (cdr mod2-name+imports)
                                    (eq? (caar ls) 'import-immutable))))
                    (cdar ls))
                   (lp (cdr ls) res))
                  ((include)
                   (lp (append (map include-source (cdar ls)) (cdr ls)) res))
                  ((include-shared)
                   (cond-expand
                    (dynamic-loading
                     (load-modules (cdar ls) *shared-object-extension*))
                    (else #f)))
                  ((body)
                   (let lp2 ((ls2 (cdar ls)) (res res))
                     (cond
                      ((pair? ls2)
                       (eval (car ls2) env)
                       (lp2 (cdr ls2) (cons (analyze (car ls2)) res)))
                      (else
                       (lp (cdr ls) res)))))
                  (else
                   (lp (cdr ls) res)))))))))))))
