(define-library (srfi 229)
  (export procedure/tag? procedure-tag lambda/tag
          case-lambda/tag)
  (import (scheme base)
          (only (chibi) lambda/generative length*)
          (only (chibi ast)
                Procedure type-of
                procedure-tag
                procedure-tag-set!
                procedure-tagged?))
  (begin
    (define-syntax lambda/tag
      (syntax-rules ()
        ((lambda/tag tag-expr formals body1 ... body2)
         (let ((proc (lambda/generative formals body1 ... body2)))
           (procedure-tag-set! proc tag-expr)
           proc))))
    (define (procedure/tag? obj)
      (and (eq? (type-of obj) Procedure)
           (procedure-tagged? obj)))
   (define-syntax %case
     (syntax-rules ()
       ((%case args len n p ((params ...) . body) . rest)
        (if (= len (length '(params ...)))
            (apply (lambda (params ...) . body) args)
            (%case args len 0 () . rest)))
       ((%case args len n (p ...) ((x . y) . body) . rest)
        (%case args len (+ n 1) (p ... x) (y . body) . rest))
       ((%case args len n (p ...) (y . body) . rest)
        (if (>= len n)
            (apply (lambda (p ... . y) . body) args)
            (%case args len 0 () . rest)))
       ((%case args len n p)
        (error "case-lambda/tag: no cases matched"))))
   (define-syntax case-lambda/tag
     (syntax-rules ()
       ((case-lambda tag-expr . clauses)
        (lambda/tag tag-expr args (let ((len (length* args))) (%case args len 0 () . clauses))))))))
