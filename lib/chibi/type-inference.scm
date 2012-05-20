;; Copyright (c) 2010-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> General type-inference library.

(define (typed? x)
  (and (lambda? x)
       (lambda-return-type x)))

(define (union-type? a)
  (and (pair? a) (equal? (car a) 'or)))

(define (intersection-type? a)
  (and (pair? a) (equal? (car a) 'and)))

(define (unfinalized-type? a)
  (and (pair? a)
       (or (memq (car a) '(return-type param-type))
           (and (memq (car a) '(and or))
                (any unfinalized-type? (cdr a))))))

(define (finalized-type? a)
  (not (unfinalized-type? a)))

(define (numeric-type? a)
  (or (eq? a Number) (eq? a Flonum) (eq? a Integer)))

(define (procedure-type? a)
  (or (eq? a Opcode)
      (eq? a Procedure)
      (and (pair? a) (eq? (car a) 'lambda))))

(define (type=? a b)
  (cond
   ((and (pair? a) (eq? (car a) 'param-type))
    (and (pair? b) (eq? (car b) 'param-type)
         (eq? (cadr a) (cadr b))
         (eq? (car (cddr a)) (car (cddr b)))))
   ((and (pair? a) (eq? (car a) 'return-type))
    (and (pair? b) (eq? (car b) 'return-type)
         (eq? (cadr a) (cadr b))))
   (else
    (equal? a b))))

(define (type-subset? a b)
  (or (type=? a b)
      (eq? a Object)
      (eq? b Object)
      (and (numeric-type? a) (numeric-type? b))
      (and (procedure-type? a) (procedure-type? b))
      (if (union-type? a)
          (if (union-type? b)
              (lset<= type=? (cdr a) (cdr b))
              (member b (cdr a) type=?))
          (and (union-type? b) (member a (cdr b) type=?)))))

(define (type-not a)
  (match a
    (('not b) b)
    (else (list 'not a))))

;; XXXX check for type hierarchies
(define (type-union a b)
  (cond
   ((type=? a b) a)
   ((or (eq? a Object) (eq? b Object)) Object)
   ((union-type? a)
    (if (union-type? b)
        (cons (car a) (lset-union type=? (cdr a) (cdr b)))
        (cons (car a) (lset-adjoin type=? (cdr a) b))))
   (else (list 'or a b))))

;; XXXX check for conflicts
(define (type-intersection a b)
  (cond
   ((type=? a b) a)
   ((or (eq? a Object) (unfinalized-type? a)) b)
   ((or (eq? b Object) (unfinalized-type? b)) a)
   ((intersection-type? a)
    (if (intersection-type? b)
        (lset-intersection type=? (cdr a) (cdr b))
        (cons (car a) (lset-adjoin type=? (cdr a) b))))
   (else (list 'and a b))))

(define (lambda-param-types-initialize! f)
  (lambda-param-types-set! f (map (lambda (p) (list 'param-type f p))
                                  (lambda-params f))))

(define (lambda-param-type-memq f x)
  (let lp ((p (lambda-params f))
           (t (lambda-param-types f)))
    (and (pair? p)
         (pair? t)
         (if (eq? x (car p))
             t
             (lp (cdr p) (cdr t))))))

(define (lambda-param-type-ref f x)
  (cond ((lambda-param-type-memq f x) => car)
        (else #f)))

(define (lambda-param-type-set! f x y)
  (if (not (pair? (lambda-param-types f)))
      (lambda-param-types-initialize! f))
  (cond ((lambda-param-type-memq f x)
         => (lambda (cell) (set-car! cell y)))))

(define (type-assert x true?)
  (match x
    (((? opcode? f) ($ Ref name (_ . (? lambda? g))))
     (cond
      ((eq? (opcode-class f) (opcode-class pair?))
       (let ((t (type-intersection
                 (lambda-param-type-ref g name)
                 (if true? (opcode-data f) (type-not (opcode-data f))))))
         (lambda-param-type-set! g name t)))))
    ((($ Ref _ ('not . (? procedure? f))) expr)
     (if (eq? f not)
         (type-assert expr (not true?))))
    (else #f)))

(define (type-analyze-expr x)
  (match x
    (($ Lam name params body defs)
     (cond
      ((not (lambda-return-type x))
       (lambda-return-type-set! x (list 'return-type x))
       (lambda-param-types-initialize! x)
       (let ((ret-type (type-analyze-expr body)))
         (lambda-return-type-set! x ret-type)
         (cons 'lambda (cons ret-type (lambda-param-types x)))))))
    (($ Set ref value)
     (type-analyze-expr value)
     (if #f #f))
    (($ Ref name (value . loc))
     (cond
      ((lambda? loc) (lambda-param-type-ref loc name))
      ((procedure? loc)
       (let ((sig (procedure-signature loc)))
         (if (and (pair? sig) (car sig))
             (cons 'lambda sig)
             (list 'return-type (procedure-analysis loc)))))
      (else Object)))
    (($ Cnd test pass fail)
     (let ((test-type (type-analyze-expr test))
           (pass-type (type-analyze-expr pass))
           (fail-type (type-analyze-expr fail)))
       (cond
        ((equal? '(error) pass-type)
         (type-assert test #f)
         fail-type)
        ((equal? '(error) fail-type)
         (type-assert test #t)
         pass-type)
        (else
         (type-union pass-type fail-type)))))
    (($ Seq ls)
     (let lp ((ls ls))
       (cond ((null? (cdr ls))
              (type-analyze-expr (car ls)))
             (else
              (type-analyze-expr (car ls))
              (lp (cdr ls))))))
    (((? opcode? f) args ...)
     (let lp ((p (opcode-param-types f))
              (a args))
       (cond
        ((pair? a)
         (cond
          ((or (pair? p) (opcode-variadic? f))
           (let ((p-type
                  (if (pair? p)
                      (car p)
                      (opcode-param-type f (opcode-num-params f)))))
             (match (car a)
               (($ Ref name (_ . (and g ($ Lam))))
                (let ((t (type-intersection (lambda-param-type-ref g name)
                                            p-type)))
                  (lambda-param-type-set! g name t)))
               (else
                (let ((t (type-analyze-expr (car a))))
                  (cond
                   ((and t p-type
                         (finalized-type? t)
                         (finalized-type? p-type)
                         (not (type-subset? t p-type)))
                    (display "WARNING: incompatible type: "
                             (current-error-port))
                    (write/ss (list x t p-type) (current-error-port))
                    (newline (current-error-port))))
                  t))))
           (lp (and (pair? p) (cdr p)) (cdr a)))
          (else
           (for-each type-analyze-expr a))))))
     (opcode-return-type f))
    ((f args ...)
     (let ((f-type (type-analyze-expr f)))
       ;; XXXX apply f-type to params
       (for-each type-analyze-expr args)
       (cond
        ((and (pair? f-type) (eq? (car f-type) 'lambda))
         (cadr f-type))
        ((and (pair? f-type) (memq (car f-type) '(return-type param-type)))
         f-type)
        (else
         Object))))
    (($ Lit value)
     (type-of value))
    (else
     (type-of x))))

(define (resolve-delayed-type x)
  (let lp ((x x) (seen '()) (default Object))
    (match x
      (('return-type (? lambda? f))
       (if (memq f seen)
           default
           (lp (lambda-return-type f) (cons f seen) default)))
      (('param-type f p)
       (if (member x seen)
           default
           (lp (lambda-param-type-ref f p) (cons x seen) default)))
      (('or y ...)
       (let ((z (find finalized-type? y)))
         (if z
             (let ((default (if (eq? default Object)
                                (lp z seen default)
                                (type-union (lp z seen default) default))))
               (fold type-union
                     default
                     (map (lambda (y1) (lp y1 seen default)) (delete z y))))
             (fold type-union default (map (lambda (y1) (lp y1 seen default)) y)))))
      (('and y ...)
       (fold type-intersection default (map (lambda (y1) (lp y1 seen default)) y)))
      (('not y)
       (list 'not (lp y seen default)))
      (else
       x))))

(define (type-resolve-circularities x)
  (match x
    (($ Lam name params body defs)
     (if (unfinalized-type? (lambda-return-type x))
         (lambda-return-type-set! x (resolve-delayed-type
                                     (lambda-return-type x))))
     (for-each
      (lambda (p t)
        (if (unfinalized-type? t)
            (lambda-param-type-set! x p (resolve-delayed-type t))))
      params
      (lambda-param-types x))
     (type-resolve-circularities (lambda-body x)))
    (($ Set ref value)
     (type-resolve-circularities value))
    (($ Cnd test pass fail)
     (type-resolve-circularities test)
     (type-resolve-circularities pass)
     (type-resolve-circularities fail))
    (($ Seq ls)
     (for-each type-resolve-circularities ls))
    ((app ...)
     (for-each type-resolve-circularities app))
    (else #f)))

(define (type-analyze-module-body name ls)
  (for-each type-analyze-expr ls)
  (for-each type-resolve-circularities ls))

;;> Analyze the types of all bindings in the module @var{name}.

(define (type-analyze-module name)
  (let* ((mod (analyze-module name))
         (ls (and (vector? mod) (module-ast mod))))
    (and ls
         (let ((x (let lp ((ls ls)) ;; first lambda
                    (and (pair? ls)
                         (if (and (set? (car ls))
                                  (lambda? (set-value (car ls))))
                             (set-value (car ls))
                             (lp (cdr ls)))))))
           (if (and x (not (typed? x)))
               (type-analyze-module-body name ls))
           ls))))

;;> Return the type signature for a given source
;;> code expression.

(define (type-analyze sexp . o)
  (type-analyze-expr (apply analyze sexp o)))

(define (opcode-param-types x)
  (let lp ((n (- (opcode-num-params x) 1)) (res '()))
    (if (< n 0)
        res
        (lp (- n 1) (cons (opcode-param-type x n) res)))))

(define (opcode-type x)
  (cons 'lambda (cons (opcode-return-type x) (opcode-param-types x))))

(define (lambda-type x)
  (cons 'lambda (cons (lambda-return-type x) (lambda-param-types x))))

;;> Return the type signature for the procedure @var{x} as
;;> a list whose first element is the return type and whose
;;> remaining arguments are the parameter types.

(define (procedure-signature x)
  (cond
   ((opcode? x)
    (cdr (opcode-type x)))
   ((macro? x)
    (procedure-signature (macro-procedure x)))
   (else
    (let lp ((count 0))
      (let ((lam (procedure-analysis x)))
        (cond
         ((and lam (not (typed? lam)) (zero? count)
               (containing-module x))
          => (lambda (mod)
               (and (type-analyze-module (car mod))
                    (lp (+ count 1)))))
         ((lambda? lam)
          (cdr (lambda-type lam)))
         (else
          #f)))))))
