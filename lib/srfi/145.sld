
(define-library (srfi 145)
  (export assume)
  (import (scheme base))
  (cond-expand
   (elide-assumptions
    (begin
      (define-syntax assume
        (syntax-rules ()
          ((assume expression objs ...)
           expression)
          ((assume)
           (syntax-error "assume requires an expression"))))))
   (else
    (begin
      (define-syntax assume
        (syntax-rules ()
          ((assume expression objs ...)
           (or expression
               (fatal-error "invalid assumption" 'expression objs ...)))
          ((assume)
           (syntax-error "assume requires an expression")))))))
  (cond-expand
   (debug
    (begin
      (define fatal-error error)))
   (else
    (begin
      (define (fatal-error message . objs)
        (car 0))))))
