
(define-library (chibi optional)
  (export let-optionals let-optionals* opt-lambda
          let-keywords let-keywords* keyword-ref keyword-ref*)
  (cond-expand
   (chibi
    (import (chibi))
    (begin
      (define-syntax symbol->keyword*
        (er-macro-transformer
         (lambda (expr rename compare)
           (if (and (pair? (cdr expr)) (pair? (cadr expr))
                    (compare 'quote (car (cadr expr))))
               `(,(rename 'quote)
                 ,(string->symbol
                   (string-append
                    (symbol->string
                     (identifier->symbol (cadr (cadr expr)))) ":")))
               `(string->symbol
                 (string-append (symbol->string ,(cadr expr)) ":"))))))))
   (else
    (import (scheme base))
    (begin
      (define-syntax let-optionals*
        (syntax-rules ()
          ((let-optionals* opt-ls () . body)
           (begin . body))
          ((let-optionals* (op . args) vars . body)
           (let ((tmp (op . args)))
             (let-optionals* tmp vars . body)))
          ((let-optionals* tmp ((var default) . rest) . body)
           (let ((var (if (pair? tmp) (car tmp) default))
                 (tmp2 (if (pair? tmp) (cdr tmp) '())))
             (let-optionals* tmp2 rest . body)))
          ((let-optionals* tmp tail . body)
           (let ((tail tmp)) . body))))
      (define-syntax symbol->keyword*
        (syntax-rules ()
          ((symbol->keyword* sym)
           (string->symbol (string-append (symbol->string sym) ":")))
          )))))
  (include "optional.scm"))
