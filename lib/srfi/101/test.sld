
;; Adapted from the reference implementation test suite for R7RS.

(define-library (srfi 101 test)
  (import (except (scheme base)
                  quote pair? cons car cdr 
                  caar cadr cddr cdar
                  null? list? list make-list length 
                  append reverse list-tail
                  list-ref map for-each)
          (prefix (scheme base) r7:)
          (srfi 101)
          (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)

      (test-begin "srfi-101: random access lists")

      (test-assert (let ((f (lambda () '(x))))
                     (eq? (f) (f))))

      (test '(1 2 3) (list 1 2 3))

      ;; pair?
      (test-assert (pair? (cons 'a 'b)))
      (test-assert (pair? (list 'a 'b 'c)))
      (test-not (pair? '()))
      (test-not (pair? '#(a b)))

      ;; cons
      (test (cons 'a '()) (list 'a))
      (test (cons (list 'a) (list 'b 'c 'd))
          (list (list 'a) 'b 'c 'd))
      (test (cons "a" (list 'b 'c))
          (list "a" 'b 'c))
      (test (cons 'a 3)
          (cons 'a 3))
      (test (cons (list 'a 'b) 'c)
          (cons (list 'a 'b) 'c))

      ;; car
      (test 'a
          (car (list 'a 'b 'c)))
      (test (list 'a)
          (car (list (list 'a) 'b 'c 'd)))
      (test 1 (car (cons 1 1)))
      (test-error (car '()))

      ;; cdr
      (test (list 'b 'c 'd)
          (cdr (list (list 'a) 'b 'c 'd)))
      (test 2
          (cdr (cons 1 2)))
      (test-error (cdr '()))

      ;; null?
      (test-assert (eq? null? r7:null?))
      (test-assert (null? '()))
      (test-not (null? (cons 1 2)))
      (test-not (null? 4))

      ;; list?
      (test-assert (list? (list 'a 'b 'c)))
      (test-assert (list? '()))
      (test-not (list? (cons 'a 'b)))

      ;; list
      (test (list 'a 7 'c)
          (list 'a (+ 3 4) 'c))
      (test '() (list))

      ;; make-list
      (test 5 (length (make-list 5)))
      (test (list 0 0 0 0 0)
          (make-list 5 0))

      ;; length
      (test 3 (length (list 'a 'b 'c)))
      (test 3 (length (list 'a (list 'b) (list 'c))))
      (test 0 (length '()))

      ;; append
      (test (list 'x 'y) (append (list 'x) (list 'y)))
      (test (list 'a 'b 'c 'd) (append (list 'a) (list 'b 'c 'd)))
      (test (list 'a (list 'b) (list 'c)) 
          (append (list 'a (list 'b)) (list (list 'c))))
      (test (cons 'a (cons 'b (cons 'c 'd))) 
          (append (list 'a 'b) (cons 'c 'd)))
      (test 'a (append '() 'a))

      ;; reverse
      (test (list 'c 'b 'a)
          (reverse (list 'a 'b 'c)))
      (test (list (list 'e (list 'f)) 'd (list 'b 'c) 'a)
          (reverse (list 'a (list 'b 'c) 'd (list 'e (list 'f)))))

      ;; list-tail
      (test (list 'c 'd)
          (list-tail (list 'a 'b 'c 'd) 2))

      ;; list-ref
      (test 'c (list-ref (list 'a 'b 'c 'd) 2))

      ;; list-set
      (test (list 'a 'b 'x 'd)
          (list-set (list 'a 'b 'c 'd) 2 'x))

      ;; list-ref/update
      (let-values (((a b) 
                    (list-ref/update (list 7 8 9 10) 2 -)))
        (test 9 a)
        (test (list 7 8 -9 10) (values b)))

      ;; map
      (test (list 'b 'e 'h)
          (map cadr (list (list 'a 'b) (list 'd 'e) (list 'g 'h))))
      (test (list 1 4 27 256 3125)
          (map (lambda (n) (expt n n))
                 (list 1 2 3 4 5)))
      (test (list 5 7 9)
          (map + (list 1 2 3) (list 4 5 6)))

      ;; for-each
      (test '#(0 1 4 9 16)
          (let ((v (make-vector 5)))
              (for-each (lambda (i)
                          (vector-set! v i (* i i)))
                        (list 0 1 2 3 4))
              v))

      ;; random-access-list->linear-access-list
      ;; linear-access-list->random-access-list
      (test '() (random-access-list->linear-access-list '()))
      (test '() (linear-access-list->random-access-list '()))

      (test (r7:list 1 2 3)
          (random-access-list->linear-access-list (list 1 2 3)))

      (test (list 1 2 3)
          (linear-access-list->random-access-list (r7:list 1 2 3)))

      (test-end))))
