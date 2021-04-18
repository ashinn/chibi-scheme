(define-library (srfi 219 test)
  (export run-tests)
  (import (chibi) (chibi test) (rename (srfi 219) (define define-219)))
  (begin
    (define (run-tests)
      (test-group
       "srfi-219: define higher-order lambda"

       (let ()
         (define-219 ((greet/prefix prefix) suffix)
           (string-append prefix " " suffix))
         (let ((greet (greet/prefix "Hello")))
           (test "Hello there!" (greet "there!"))))

       (let ()
         (define-219 ((append-to . a) . b)
           (apply append (append a b)))
         (test '()
               ((append-to '()) '()))
         (test '(1 2 3 4 5 6 7 8)
               ((append-to '(1 2) '(3 4)) '(5 6) '(7 8))))

       (let ()
         (define-219 (((jenga a b) c d))
           (list a b c d))
         (test '(1 2 3 4)
               (((jenga 1 2) 3 4))))))))
