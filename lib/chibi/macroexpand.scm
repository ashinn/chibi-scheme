;; macroexpand.scm -- macro expansion utility
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; This actually analyzes the expression then reverse-engineers an
;; sexp from the result, generating a minimal amount of renames.

(define (macroexpand x)
  (ast->sexp (analyze x)))

(define (ast-renames ast)
  (define i 0)
  (define renames '())
  (define (rename-symbol id)
    (set! i (+ i 1))
    (string->symbol
     (string-append (symbol->string (identifier->symbol id))
                    "." (number->string i))))
  (define (rename-lambda lam)
    (or (assq lam renames)
        (let ((res (list lam)))
          (set! renames (cons res renames))
          res)))
  (define (rename! id lam)
    (let ((cell (rename-lambda lam)))
      (set-cdr! cell (cons (cons id (rename-symbol id)) (cdr cell)))))
  (define (check-ref id lam env)
    (let ((sym (identifier->symbol id)))
      (let lp1 ((ls env))
        (cond
         ((pair? ls)
          (let lp2 ((ls2 (car ls)) (found? #f))
            (cond
             ((null? ls2)
              (if (not found?) (lp1 (cdr ls))))
             ((and (eq? id (caar ls2)) (eq? lam (cdar ls2)))
               (lp2 (cdr ls2) #t))
             ((eq? sym (identifier->symbol (caar ls2)))
              (rename! (caar ls2) (cdar ls2))
              (lp2 (cdr ls2) found?))
             (else
              (lp2 (cdr ls2) found?)))))))))
  (define (flatten-dot x)
    (cond ((pair? x) (cons (car x) (flatten-dot (cdr x))))
          ((null? x) x)
          (else (list x))))
  (define (extend-env lam env)
    (cons (map (lambda (x) (cons x lam)) (flatten-dot (lambda-params lam))) env))
  (let lp ((x ast) (env '()))
    (cond
     ((lambda? x) (lp (lambda-body x) (extend-env x env)))
     ((ref? x) (check-ref (ref-name x) (cdr (ref-cell x)) env))
     ((cnd? x) (lp (cnd-test x) env) (lp (cnd-pass x) env) (lp (cnd-fail x) env))
     ((set? x) (lp (set-var x) env) (lp (set-value x) env))
     ((seq? x) (for-each (lambda (x) (lp x env)) (seq-ls x)))
     ((pair? x) (for-each (lambda (x) (lp x env)) x))))
  renames)

(define (get-rename id lam renames)
  (let ((ls (assq lam renames)))
    (if (not ls)
        (identifier->symbol id)
        (cond ((assq id (cdr ls)) => cdr) (else (identifier->symbol id))))))

(define (ast->sexp ast)
  (let ((renames (ast-renames ast)))
    (let a2s ((x ast))
      (cond
       ((lambda? x)
        `(lambda ,(map (lambda (id) (get-rename id x renames)) (lambda-params x))
           ,@(map (lambda (d) `(define ,(identifier->symbol (cadar d)) #f))
                  (lambda-defs x))
           ,@(if (seq? (lambda-body x))
                 (map a2s (seq-ls (lambda-body x)))
                 (list (a2s (lambda-body x))))))
       ((cnd? x) `(if ,(a2s (cnd-test x)) ,(a2s (cnd-pass x)) ,(a2s (cnd-fail x))))
       ((set? x) `(set! ,(a2s (set-var x)) ,(a2s (set-value x))))
       ((ref? x) (get-rename (ref-name x) (cdr (ref-cell x)) renames))
       ((seq? x) `(begin ,@(map a2s (seq-ls x))))
       ((lit? x)
        (let ((v (lit-value x)))
          (if (or (pair? v) (null? v) (symbol? v)) `',v v)))
       ((pair? x) (cons (a2s (car x)) (a2s (cdr x))))
       ((opcode? x) (or (opcode-name x) x))
       (else x)))))

