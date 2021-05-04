(define-library (chibi assert)
  (export assert)
  (cond-expand
   (chibi
    (import (chibi))
    (begin
      (define-syntax syntax-identifier?
        (er-macro-transformer
         (lambda (expr rename compare)
           (if (identifier? (cadr expr))
               (car (cddr expr))
               (cadr (cddr expr))))))
      (define-syntax syntax-memq?
        (er-macro-transformer
         (lambda (expr rename compare)
           (let ((expr (cdr expr)))
             (if (memq (car expr) (cadr expr))
                 (car (cddr expr))
                 (cadr (cddr expr)))))))))
   (else
    (import (scheme base))
    (begin
      ;; from match.scm
      (define-syntax syntax-identifier?
        (syntax-rules ()
          ((_ (x . y) success-k failure-k) failure-k)
          ((_ #(x ...) success-k failure-k) failure-k)
          ((_ x success-k failure-k)
           (let-syntax
               ((sym?
                 (syntax-rules ()
                   ((sym? x sk fk) sk)
                   ((sym? y sk fk) fk))))
             (sym? abracadabra success-k failure-k)))))
      (define-syntax syntax-memq?
        (syntax-rules ()
          ((syntax-memq? id (ids ...) sk fk)
           (let-syntax
               ((memq?
                 (syntax-rules (ids ...)
                   ((memq? id sk2 fk2) fk2)
                   ((memq? any-other sk2 fk2) sk2))))
             (memq? random-symbol-to-match sk fk))))))))
  (begin
    (define-syntax report-vars
      (syntax-rules (quote quasiquote lambda)
        ((report-vars (lambda . x) (next ...) res)
         (next ... res))
        ((report-vars 'x (next ...) res)
         (next ... res))
        ((report-vars `x (next ...) res)
         (next ... res))
        ((report-vars (op arg0 arg1 ...) next res)
         (report-vars arg0 (report-vars (op arg1 ...) next) res))
        ((report-vars (op . x) (next ...) res)
         (next ... res))
        ((report-vars x (next ...) (res ...))
         (syntax-identifier? x
                             (syntax-memq? x (res ...)
                                           (next ... (res ...))
                                           (next ... (res ... x)))
                             (next ... (res ...))))))
    (define-syntax report-final
      (syntax-rules ()
        ((report-final expr (var ...))
         (error "assertion failed" 'expr `(var ,var) ...))))
    (define-syntax assert
      (syntax-rules (report:)
        ((assert test report: msg ...)
         (unless test
           (error msg ...)))
        ((assert test0 test1 ...)
         (if test0
             (assert test1 ...)
             (report-vars test0 (report-final test0) ())))
        ((assert) #t)))))
