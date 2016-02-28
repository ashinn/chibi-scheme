(define-library (srfi 16 test)
  (export run-tests)
  (import (chibi) (chibi test) (srfi 16))
  (begin
    (define (run-tests)
      (define plus
        (case-lambda 
         (() 0)
         ((x) x)
         ((x y) (+ x y))
         ((x y z) (+ (+ x y) z))
         (args (apply + args))))
      (define print
        (case-lambda
         (()
          (display ""))
         ((arg)
          (display arg))
         ((arg . args)
          (display arg)
          (display " ")
          (apply print args))))
      (define (print-to-string . args)
        (let ((out (open-output-string))
              (old-out (current-output-port)))
          (dynamic-wind
            (lambda () (current-output-port out))
            (lambda () (apply print args))
            (lambda () (current-output-port old-out)))
          (get-output-string out)))

      (test-begin "srfi-16: case-lambda")

      (test 0 (plus))
      (test 1 (plus 1))
      (test 6 (plus 1 2 3))
      (test-error ((case-lambda ((a) a) ((a b) (* a b))) 1 2 3))

      (test "" (print-to-string))
      (test "hi" (print-to-string 'hi))
      (test "hi there world" (print-to-string 'hi 'there 'world))

      (test-end))))
