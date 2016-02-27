
(cond-expand
 (modules (import (chibi) (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "syntax")

(define-syntax loop1
  (sc-macro-transformer
   (lambda (exp env)
     (let ((body (cdr exp)))
       `(call-with-current-continuation
         (lambda (exit)
           (let f ()
             ,@(map (lambda (exp)
                      (make-syntactic-closure env '(exit) exp))
                    body)
             (f))))))))

(define exit 42)
(test 10 (loop1 (exit 10)))

(define (y) 0)

(define-syntax macro
  (sc-macro-transformer
   (lambda (exp env)
     (make-syntactic-closure env '(y) (cadr exp)))))

(let ((y (lambda () 100)))
  (test 0 (macro (y))))

(let ((x 10))
  (define-syntax macro
    (sc-macro-transformer
     (lambda (exp env)
       (make-syntactic-closure env '(x) (cadr exp)))))
  (let ((x 20))
    (define-syntax macro2
      (sc-macro-transformer
       (lambda (exp env)
	 (macro (make-syntactic-closure env '(x) (cadr exp))))))
    (let ((x 30))
      (test 20 (macro2 x)))))

(define E1 1)

(define-syntax M
  (syntax-rules E1 ()
    ((M x E1) (quote (x E1)))))

(test '(1 2 3) (M 1 2 3))

(let ((E2 2))
  (define-syntax N
    (syntax-rules E2 ()
      ((N y E2) (quote (y E2)))))
  (test '(1 2 3) (N 1 2 3)))

(test-end)
