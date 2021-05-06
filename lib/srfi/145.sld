(define-library (srfi 145)
  (export assume)
  (import (scheme base) (chibi assert))
  (cond-expand
    ((or elide-assumptions
         (and (not assumptions)
              (not debug)))
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
            (assert expression objs ...))
           ((assume)
            (syntax-error "assume requires an expression"))))))))
