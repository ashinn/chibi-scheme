(define-library (chibi assert)
  (export assert)
  (cond-expand
   (chibi
    (import (chibi))
    (begin
      (define-syntax check-identifier
        (er-macro-transformer
         (lambda (expr rename compare)
           (if (identifier? (cadr expr))
               (car (cddr expr))
               (cadr (cddr expr))))))))
   (else
    (import (scheme base))
    (begin
      ;; from match.scm
      (define-syntax check-identifier
        (syntax-rules ()
          ((_ (x . y) success-k failure-k) failure-k)
          ((_ #(x ...) success-k failure-k) failure-k)
          ((_ x success-k failure-k)
           (let-syntax
               ((sym?
                 (syntax-rules ()
                   ((sym? x sk fk) sk)
                   ((sym? y sk fk) fk))))
             (sym? abracadabra success-k failure-k))))))))
  (begin
    (define-syntax report-vars
      (syntax-rules (quote quasiquote)
        ((report-vars 'x (next ...) res)
         (next ... res))
        ((report-vars `x (next ...) res)
         (next ... res))
        ((report-vars (op arg0 arg1 ...) next res)
         (report-vars arg0 (report-vars (op arg1 ...) next) res))
        ((report-vars (op . x) (next ...) res)
         (next ... res))
        ((report-vars x (next ...) (res ...))
         (check-identifier x
                           (next ... (res ... (x ,x)))
                           (next ... (res ...))))))
    (define-syntax report-final
      (syntax-rules ()
        ((report-final expr (vars ...))
         (error "assertion failed" 'expr `vars ...))))
    (define-syntax assert
      (syntax-rules ()
        ((assert test0 test1 ...)
         (if test0
             (assert test1 ...)
             (report-vars test0 (report-final test0) ())))
        ((assert) #t)))))
