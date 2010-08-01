;; type-inference.scm -- general type-inference for Scheme
;;
;; Copyright (c) 2010 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

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
  (or (eq? a <number>) (eq? a <flonum>) (eq? a <integer>)))

(define (procedure-type? a)
  (or (eq? a <opcode>)
      (eq? a <procedure>)
      (and (pair? a) (eq? (car a) 'lambda))))

(define (type-subset? a b)
  (or (equal? a b)
      (equal? a <object>)
      (equal? b <object>)
      (and (numeric-type? a) (numeric-type? b))
      (and (procedure-type? a) (procedure-type? b))
      (if (union-type? a)
          (if (union-type? b)
              (lset<= equal? (cdr a) (cdr b))
              (member b (cdr a)))
          (and (union-type? b) (member a (cdr b))))))

;; XXXX check for type hierarchies
(define (type-union a b)
  (cond
   ((equal? a b) a)
   ((or (equal? a <object>) (equal? b <object>)) <object>)
   ((union-type? a)
    (if (union-type? b)
        (cons (car a) (lset-union equal? (cdr a) (cdr b)))
        (cons (car a) (lset-adjoin equal? (cdr a) b))))
   (else (list 'or a b))))

;; XXXX check for conflicts
(define (type-intersection a b)
  (cond
   ((equal? a b) a)
   ((or (equal? a <object>) (unfinalized-type? a)) b)
   ((or (equal? b <object>) (unfinalized-type? b)) a)
   ((intersection-type? a)
    (if (intersection-type? b)
        (lset-intersection equal? (cdr a) (cdr b))
        (cons (car a) (lset-adjoin equal? (cdr a) b))))
   (else (list 'and a b))))

(define (type-of x)
  (cond ((boolean? x) <boolean>)
        ((char? x) <char>)
        ((symbol? x) <symbol>)
        ((string? x) <string>)
        ((and (integer? x) (exact? x)) <integer>)
        ((flonum? x) <flonum>)
        ((pair? x) <pair>)
        ((vector? x) <vector>)
        (else <object>)))

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

(define (type-analyze-expr x)
  (match x
    (($ <lam> name params body defs)
     (cond
      ((not (lambda-return-type x))
       (lambda-return-type-set! x (list 'return-type x))
       (lambda-param-types-initialize! x)
       (let ((ret-type (type-analyze-expr body)))
         (lambda-return-type-set! x ret-type)
         (cons 'lambda (cons ret-type (lambda-param-types x)))))))
    (($ <set> ref value)
     (type-analyze-expr value)
     (if #f #f))
    (($ <ref> name (value . loc) source)
     (cond
      ((lambda? loc) (lambda-param-type-ref loc name))
      ((procedure? loc)
       (let ((sig (procedure-signature loc)))
         (if (and (pair? sig) (car sig))
             (cons 'lambda sig)
             (list 'return-type (procedure-analysis loc)))))
      (else <object>)))
    (($ <cnd> test pass fail)
     (let ((test-type (type-analyze-expr test))
           (pass-type (type-analyze-expr pass))
           (fail-type (type-analyze-expr fail)))
       (type-union pass-type fail-type)))
    (($ <seq> ls)
     (let lp ((ls ls))
       (cond ((null? (cdr ls))
              (type-analyze-expr (car ls)))
             (else
              (type-analyze-expr (car ls))
              (lp (cdr ls))))))
    ((f args ...)
     (cond
      ((opcode? f)
       (let lp ((p (opcode-param-types f))
                (a args))
         (cond
          ((pair? a)
           (cond ((or (pair? p) (opcode-variadic? f))
                  (let ((p-type
                         (if (pair? p)
                             (car p)
                             (opcode-param-type f (opcode-num-params f)))))
                    (match (car a)
                     (($ <ref> name (_ . (and g ($ <lam>))))
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
                          (write (list x t p-type) (current-error-port))
                          (newline (current-error-port))))
                        t))))
                  (lp (and (pair? p) (cdr p)) (cdr a)))
                 (else
                  (for-each type-analyze-expr a))))))
       (opcode-return-type f))
      (else
       (let ((f-type (type-analyze-expr f)))
         ;; XXXX apply f-type to params
         (for-each type-analyze-expr args)
         (cond
          ((and (pair? f-type) (eq? (car f-type) 'lambda))
           (cadr f-type))
          ((and (pair? f-type) (memq (car f-type) '(return-type param-type)))
           f-type)
          (else
           <object>))))))
    (else
     (type-of x))))

(define (resolve-delayed-type x)
  (let lp ((x x) (seen '()) (default <object>))
    (match x
      (('return-type f)
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
             (let ((default (if (eq? default <object>)
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
    (($ <lam> name params body defs)
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
    (($ <set> ref value)
     (type-resolve-circularities value))
    (($ <cnd> test pass fail)
     (type-resolve-circularities test)
     (type-resolve-circularities pass)
     (type-resolve-circularities fail))
    (($ <seq> ls)
     (for-each type-resolve-circularities ls))
    ((app ...)
     (for-each type-resolve-circularities app))
    (else #f)))

(define (type-analyze-module-body name ls)
  (for-each type-analyze-expr ls)
  (for-each type-resolve-circularities ls))

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

(define (procedure-signature x)
  (if (opcode? x)
      (cdr (opcode-type x))
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
            #f))))))
