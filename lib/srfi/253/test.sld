(define-library (srfi 253 test)
  (import (scheme base)
          (srfi 253)
          (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)

      (define-syntax check-arg-true
        (syntax-rules ()
          ((_ pred val)
           (begin
             (check-arg pred val)
             #t))))

      (define checked-case-lambda
        (case-lambda-checked
         (() #t)
         (((a integer?)) #t)
         ((a (b string?)) #t)
         (((a string?) b . rest) #t)))

      (define-checked (c) #t)
      (define-checked (d (a integer?)) #t)
      (define-checked (e b) #t)
      (define-checked (f (b string?)) #t)
      (define-checked (g b . d) #t)
      (define-checked h string? "hello")
      (define-record-type-checked <test>
        (make-test a b)
        test?
        (a integer? test-a)
        (b string? test-b test-b-set!))
      (define test-test (make-test 1 "hello"))

      (test-begin "srfi-253: data (type-)checking")
      (test-group "check-arg"
        ;; Sanity checks
        (test-assert (check-arg-true exact-integer? 3))
        (test-assert (check-arg-true integer? 3))
        (test-assert (check-arg-true boolean? #f))
        (test-assert (check-arg-true char? #\d))
        (test-assert (check-arg-true complex? 3+2i))
        (test-assert (check-arg-true inexact? 3.8))
        (test-assert (check-arg-true real? 3))
        (test-assert (check-arg-true real? 3/2))
        (test-assert (check-arg-true real? 3.8))
        (test-assert (check-arg-true list? '()))
        (test-assert (check-arg-true list? '(1 2 3)))
        (test-assert (check-arg-true null? '()))
        (test-assert (check-arg-true number? 3))
        (test-assert (check-arg-true number? 3+2i))
        (test-assert (check-arg-true number? 3.8))
        (test-assert (check-arg-true pair? '(1 2 3)))
        (test-assert (check-arg-true input-port? (current-input-port)))
        (test-assert (check-arg-true output-port? (current-output-port)))
        (test-assert (check-arg-true procedure? procedure?))
        (test-assert (check-arg-true rational? 3))
        (test-assert (check-arg-true rational? 3/2))
        (test-assert (check-arg-true string? ""))
        (test-assert (check-arg-true string? "hello"))
        (test-assert (check-arg-true symbol? 'hello))
        (test-assert (check-arg-true vector? #(1 2 3)))
        ;; Predicate checks
        (test-assert (check-arg-true (lambda (x) (positive? (string-length x)))
                                     "hello"))
        (test-assert (check-arg-true positive? 9))
        (test-assert (check-arg-true string-length "hello")) ;; If it works it works.
        (test-assert (check-arg-true (lambda (x)
                                       (and (integer? x) (positive? x)))
                                     8))
        (test-assert (check-arg-true ((lambda (x y)
                                        (lambda (a) (and (x a) (y a))))
                                      integer? positive?)
                                     8))
        ;; Erroring checks
        (test-error (check-arg-true string? 3))
        (test-error (check-arg-true real? 3+2i))
        (test-error (check-arg-true symbol? "hello"))
        (test-error (check-arg-true procedure? 3))
        ;; It is an error when predicate doesn't pass, but it doesn't have to
        ;; throw errors. Disable depending on implementation.
        (test-error (check-arg-true (lambda (a) (> a 3)) 0))
        ;; Syntax checks
        (test-assert (begin (check-arg integer? 3 'testing-caller-arg) #t)))


      (test-group "values-checked"
        (test 3 (values-checked (integer?) 3))
        (test 3 (values-checked ((lambda (x) (= 3 x))) 3))
        (test 3.0 (values-checked (real?) 3.0))
        ;; Implementation-specific, might be 3.0
        (test 3 (values-checked (real?) 3))
        (test-assert (values-checked (integer? string?) 3 "hello"))
        (test 3.0 (values-checked (inexact?) 3.0))
        (test-error (values-checked (integer?) "hello"))
        (test-error (values-checked (integer? string?) 3 3)))

      (test-group "check-case"
        (test-assert (check-case "hello" (string? #t)))
        (test-assert (check-case 3 (integer? #t) (string? #f)))
        (test-assert (check-case 3.7 (inexact? #t)))
        (test-assert (check-case (current-output-port) (output-port? #t)))
        (test-assert (check-case #(1 2 3) (vector? #t)))
        (test-assert (check-case 3 (string? #f) (else #t)))
        (test-error (check-case 3 (string? #t))))


      (test-group "lambda-checked"
        (test-assert (lambda-checked () #t))
        (test-assert (lambda-checked args #t))
        (test-assert (lambda-checked (a) #t))
        (test-assert (lambda-checked (a b) #t))
        (test-assert (lambda-checked ((a integer?)) #t))
        (test-assert (lambda-checked (a (b integer?)) #t))
        (test-assert (lambda-checked ((a string?) (b integer?)) #t))
        (test-assert ((lambda-checked () #t)))
        (test-assert ((lambda-checked args #t) 1 2 3))
        (test-assert ((lambda-checked (a) #t) 3))
        (test-assert ((lambda-checked (a) #t) "hello"))
        (test-assert ((lambda-checked ((a integer?)) #t) 3))
        (test-assert ((lambda-checked (a (b integer?)) #t) 3 3))
        (test-assert ((lambda-checked (a (b integer?)) #t) "hello" 3))
        (test-error ((lambda-checked ((a integer?)) #t) "hello"))
        (test-error ((lambda-checked (a (b integer?)) #t) "hello" "hi"))
        ;; Rest args. Sample implementation doesn't reliably pass this.
        (test-assert (lambda-checked (a . c) #t))
        (test-assert (lambda-checked ((a integer?) . c) #t))
        (test-assert (lambda-checked (a b . c) #t))
        (test-assert (lambda-checked (a (b integer?) . c) #t)))


      (test-group "case-lambda-checked"
        (test-assert (case-lambda-checked
                      (() #t)))
        (test-assert (case-lambda-checked
                      (args #t)))
        (test-assert (case-lambda-checked
                      ((a) #t)))
        (test-assert (case-lambda-checked
                      ((a) #t)))
        (test-assert (case-lambda-checked
                      (() #t) ((a) #t)))
        (test-assert (case-lambda-checked
                      (() #t) ((a) #t) (args #t)))
        (test-assert (case-lambda-checked
                      (((a integer?)) #t)))
        (test-assert (case-lambda-checked
                      (((a integer?) b) #t)))
        (test-assert (case-lambda-checked
                      ((a (b integer?)) #t)))
        (test-assert (case-lambda-checked
                      (() #t)
                      (((a integer?)) #t)
                      ((a (b string?)) #t)
                      (args #t)))
        (test-assert (checked-case-lambda))
        (test-assert (checked-case-lambda 3))
        (test-error (checked-case-lambda "hello"))
        (test-assert (checked-case-lambda 3 "hello"))
        (test-assert (checked-case-lambda "hi" "hello"))
        (test-error (checked-case-lambda 3 3 3)))


      (test-group "define-checked"
        (test-assert (c))
        (test-assert (d 3))
        (test-error (d "hello"))
        (test-assert (e "anything"))
        (test-error (e 1 2 3))
        (test-assert (f "hello"))
        (test-error (f 3))
        (test-error (g))
        (test-assert (g 1))
        (test-assert (g 1 2))
        (test-assert (g 1 2 3))
        (test-assert h)
        (set! h "whatever")
        (test-assert h))


      (test-group "define-record-type-checked"
        (test-assert (make-test 1 "hello"))
        (test-error (make-test 1))
        (test-error (make-test 1 2))
        (test-error (make-test 1.2 "hello"))
        (test-assert (begin
                       (test-b-set! test-test "foo")
                       #t))
        (test-error (test-b-set! test-test 1)))

      (test-end))))
