
(cond-expand
 (modules
  (import (chibi)
          (only (chibi test) test-begin test test-error test-end)
          (only (meta) mutable-environment)))
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

(define-syntax ell
  (syntax-rules ()
   ((ell body)
    (define-syntax emm
      (syntax-rules ...1 ()
        ((emm) body))))))

(ell
 (define-syntax enn
   (syntax-rules ...1 () ((enn args ...1) (quote (args ...1))))))

(let ((... 'local))
  (define-syntax asd
    (syntax-rules ()
      ((asd x ...) (quote (... x)))))
  (test '(2 1) (asd 1 2)))

(test-end)

(cond-expand
 ;; can only test identifier-syntax with access to modules (though
 ;; this could be fixed in theory)
 (modules
  (test-begin "identifier syntax")
  (define syntax-test-env (mutable-environment '(chibi) '(chibi ast)))

  (eval
   '(define-syntax low-level-id-macro
      (er-macro-transformer
       (lambda (expr rename compare)
         (if (pair? expr)
             (list (rename 'quote) 'operator)
             (list (rename 'quote) 'operand)))))
   syntax-test-env)

  (test 'operator (eval '(low-level-id-macro) syntax-test-env))
  (test 'operand (eval 'low-level-id-macro syntax-test-env))
  (test-error (eval '(set! low-level-id-macro 'foo) syntax-test-env))

  (eval
   '(define-syntax low-level-vt
    (make-variable-transformer
     (er-macro-transformer
      (lambda (expr rename compare)
        (list (rename 'quote)
              (if (pair? expr)
                  (if (compare (car expr) (rename 'set!))
                      'set
                      'app)
                  'ref))))))
   syntax-test-env)

  (test 'set (eval '(set! low-level-vt 'foo) syntax-test-env))
  (test 'app (eval '(low-level-vt) syntax-test-env))
  (test 'ref (eval 'low-level-vt syntax-test-env))

  (eval '(define p (cons 1 2)) syntax-test-env)
  (eval '(define-syntax p.car (identifier-syntax (car p))) syntax-test-env)
  (eval
   '(define-syntax p.cdr
      (identifier-syntax
       (_ (cdr p))
       ((set! _ v) (set-cdr! p v))))
   syntax-test-env)

  (test 1 (eval 'p.car syntax-test-env))
  (test-error (eval '(set! p.car 0) syntax-test-env))
  (test 2 (eval 'p.cdr syntax-test-env))
  (test 3 (eval
           '(begin
              (set! p.cdr 3)
              (cdr p))
           syntax-test-env))

  ;; weirdnesses: syntax that refers to its own name and uses ellipsis
  (eval
   '(define-syntax sr-id-macro
      (identifier-syntax
       (name 'name)
       ((set! name (val ...)) (cons 'name '(val ...)))))
   syntax-test-env)

  (test 'sr-id-macro (eval 'sr-id-macro syntax-test-env))
  (test '(sr-id-macro 1 2 3)
        (eval '(set! sr-id-macro (1 2 3))
              syntax-test-env))

  (test-end))
 (else #f))
