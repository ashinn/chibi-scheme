
(define-library (srfi 16)
  (export case-lambda)
  (import (chibi))
  (begin
    (define-syntax %case
      (syntax-rules ()
        ((%case (args ... . rest-arg) len n p ((params ...) . body) . rest)
         (if (= len (length '(params ...)))
             (apply (lambda (params ...) . body) args ... rest-arg)
             (%case (args ... . rest-arg) len 0 () . rest)))
        ((%case args len n (p ...) ((x . y) . body) . rest)
         (%case args len (+ n 1) (p ... x) (y . body) . rest))
        ((%case (args ... . rest-arg) len n (p ...) (y . body) . rest)
         (if (>= len n)
             (apply (lambda (p ... . y) . body) args ... rest-arg)
             (%case (args ... . rest-arg) len 0 () . rest)))
        ((%case args len n p)
         (error "case-lambda: no cases matched"))))
    (define-syntax %shortest-arglist
      (syntax-rules ()
        ((%shortest-arglist (collected ...) (a as ...) () cases clauses ...)
         (%collect-args (collected ...) cases clauses ...))
        ((%shortest-arglist (collected ...) () (b bs ...) cases clauses ...)
         (%collect-args (collected ...) cases clauses ...))
        ((%shortest-arglist (collected ...) () () cases clauses ...)
         (%collect-args (collected ...) cases clauses ...))
        ((%shortest-arglist (collected ...) (a as ...) (b bs ...) cases clauses ...)
         (%shortest-arglist (collected ... a) (as ...) (bs ...) cases clauses ...))))
    (define-syntax %collect-args
      (syntax-rules ()
        ;; Generate the actual lambda with the right minimum arity
        ((%collect-args (shortest ...) (clauses ...))
         (lambda (shortest ... . rest)
           (let ((len (length `((unquote shortest) ... . (unquote rest)))))
             (%case (shortest ... . rest) len 0 () clauses ...))))
        ;; Generate a lambda with a single rest argument
        ((%collect-args rest-arg (clauses ...))
         (lambda rest-arg
           (let ((len (length rest-arg)))
             (%case rest-arg len 0 () clauses ...))))
        ;; Regular args: find the shorter of saved and new args
        ((%collect-args (shortest ...) (cases ...) ((args ...) body ...) clauses ...)
         (%shortest-arglist () (shortest ...) (args ...) (cases ... ((args ...) body ...)) clauses ...))
        ;; Dotted args: only consider pre-dot args
        ((%collect-args (shortest ...) (cases ...) ((args ... . rest) body ...) clauses ...)
         (%shortest-arglist () (shortest ...) (args ...) (cases ... ((args ... . rest) body ...)) clauses ...))
        ;; Rest arg: nullify the arglist and terminate
        ((%collect-args (shortest ...) (cases ...) (rest-arg body ...) clauses ...)
         (%collect-args () (cases ... (rest-arg body ...)) clauses ...))
        ;; Dotted initial arglist
        ((%collect-args (shortest ... . rest) (cases ...) (args body ...) clauses ...)
         (%collect-args (shortest ...) (cases ...) (args body ...) clauses ...))
        ;; Rest initial arglist: nullify the arglist and terminate
        ((%collect-args rest (cases ...) (args body ...) clauses ...)
         (%collect-args () (cases ...) (cases ... (args body ...) clauses ...)))))
    (define-syntax case-lambda
      (syntax-rules ()
        ((case-lambda (args body ...) clauses ...)
         (%collect-args args () (args body ...) clauses ...))))))
