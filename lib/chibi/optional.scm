
;; let-optionals* is in the core

(define-syntax let*-to-let
  (syntax-rules ()
    ((let*-to-let letstar ls (vars ...) ((v . d) . rest) . body)
     (let*-to-let letstar ls (vars ... (v tmp . d)) rest . body))
    ((let*-to-let letstar ls ((var tmp . d) ...) rest . body)
     (letstar ls ((tmp . d) ... . rest)
       (let ((var tmp) ...) . body)))))

(define-syntax let-optionals
  (syntax-rules ()
    ((let-optionals ls vars . body)
     (let*-to-let let-optionals* ls () vars . body))))

(define-syntax opt-lambda
  (syntax-rules ()
    ((opt-lambda vars . body)
     (lambda args (let-optionals args vars . body)))))

(define (keyword-ref ls key . o)
  (cond ((memq key ls) => cadr)
        (else (and (pair? o) (car o)))))

(define-syntax let-keywords*
  (syntax-rules ()
    ((let-keywords* opt-ls () . body)
     (begin . body))
    ((let-keywords* (op . args) vars . body)
     (let ((tmp (op . args)))
       (let-keywords* tmp vars . body)))
    ((let-keywords* opt-ls ((var default) . rest) . body)
     (let ((var (keyword-ref opt-ls 'var default)))
       (let-keywords* opt-ls rest . body)))
    ((let-keywords* opt-ls ((var key default) . rest) . body)
     (let ((var (keyword-ref opt-ls 'key default)))
       (let-keywords* opt-ls rest . body)))
    ((let-keywords* opt-ls tail . body)
     (let ((tail opt-ls)) . body))))

(define-syntax let-keywords
  (syntax-rules ()
    ((let-keywords ls vars . body)
     (let*-to-let let-keywords* ls () vars . body))))
