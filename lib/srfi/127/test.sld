
(define-library (srfi 127 test)
  (import (scheme base) (srfi 127) (chibi test))
  (export run-tests)
  (begin
    ;; Make-generator for tests cloned from SRFI 121
    (define (make-generator . args)
      (lambda ()
        (if (null? args)
            (eof-object)
            (let ((next (car args)))
              (set! args (cdr args))
              next))))

    ;; Make-lseq creates an lseq, like list, but guarantees the use of
    ;; a generator.
    (define (make-lseq . args)
      (generator->lseq (apply make-generator args)))

    (define (factorial n)
      (cond
       ((< n 0) #f)
       ((= n 0) 1)
       (else (* n (factorial (- n 1))))))

    (define (run-tests)
      (test-group "srfi-127: lseqs"
        (test-group "lseqs/constructor"
          (let ((one23 (make-lseq 1 2 3)))
            (test 1 (car one23))
            (test-assert (procedure? (cdr one23)))
            (test '(1 2 3) (lseq-realize one23)))
          )

        (test-group "lseqs/predicates"
          (test-assert (lseq? '()))
          (test-assert (lseq? '(1 2 3)))
          (test-assert (lseq? (make-lseq 1 2 3)))
          (test-assert (lseq? (cons 'x (lambda () 'x))))

          (test-assert (lseq=? = '() '()))
          (test-assert (lseq=? = '(1 2 3) '(1 2 3)))
          (test-assert (lseq=? = (make-lseq 1 2 3)
                               (make-lseq 1 2 3)))
          (test-assert (lseq=? = (make-lseq 1 2 3) '(1 2 3)))
          )

        (test-group "lseqs/selectors"
          (test-error (lseq-car (make-generator)))
          (test 1 (lseq-car (make-lseq 1 2 3)))
          (test 1 (lseq-car '(1 2 3)))
          (test-error (lseq-car 2))

          (test-error (lseq-first (make-generator)))
          (test 1 (lseq-first (make-lseq 1 2 3)))
          (test 1 (lseq-first '(1 2 3)))
          (test-error (lseq-first 2))

          (test-error (lseq-cdr (make-generator)))
          (test 2 (lseq-cdr '(1 . 2)))
          (test 2 (lseq-car (lseq-cdr '(1 2 3))))
          (test 2 (lseq-car (lseq-cdr (make-lseq 1 2 3))))

          (test-error (lseq-rest (make-generator)))
          (test 2 (lseq-rest '(1 . 2)))
          (test 2 (lseq-car (lseq-rest '(1 2 3))))
          (test 2 (lseq-car (lseq-rest (make-lseq 1 2 3))))
          (test-error (lseq-rest 2))

          (test-error (lseq-ref '() 0))
          (test 1 (lseq-ref '(1) 0))
          (test 2 (lseq-ref '(1 2) 1))
          (test-error (lseq-ref (make-lseq) 0))
          (test 1 (lseq-ref (make-lseq 1) 0))
          (test 1 (lseq-ref (make-lseq 1 2) 0))
          (test 2 (lseq-ref (make-lseq 1 2) 1))

          (test-error (lseq-take '() 1))
          (test-error (lseq-take (make-lseq) 1))
          (test-assert (procedure? (cdr (lseq-take '(1 2 3 4 5) 3)))) ; test laziness
          (test '(1 2 3) (lseq-realize (lseq-take '(1 2 3 4 5) 3)))

          (test-error (lseq-drop '() 1))
          (test-error (lseq-drop (make-lseq 1) 2))
          (test '(3 4 5) (lseq-realize (lseq-drop '(1 2 3 4 5) 2)))
          (test '(3 4 5) (lseq-realize (lseq-drop (make-lseq 1 2 3 4 5) 2)))
          )

        (test-group "lseqs/whole"
          (test '() (lseq-realize '()))
          (test '(1 2 3) (lseq-realize '(1 2 3)))
          (test '() (lseq-realize (make-lseq)))
          (test '(1 2 3) (lseq-realize (make-lseq 1 2 3)))

          (let ((g (lseq->generator '(1 2 3))))
            (test 1 (g))
            (test 2 (g))
            (test 3 (g))
            (test-assert (eof-object? (g))))

          (let ((g (lseq->generator (make-lseq 1 2 3))))
            (test 1 (g))
            (test 2 (g))
            (test 3 (g))
            (test-assert (eof-object? (g))))

          (test 0 (lseq-length '()))
          (test 3 (lseq-length '(1 2 3)))
          (test 3 (lseq-length (make-lseq 1 2 3)))

          (test '(1 2 3 a b c) (lseq-realize (lseq-append '(1 2 3) '(a b c))))
          (let ((one23abc (lseq-append (make-lseq 1 2 3) (make-lseq 'a 'b 'c))))
            (test-assert (procedure? (cdr one23abc)))
            (test-assert (lseq-realize one23abc)))

          (let ((one2345 (make-lseq 1 2 3 4 5))
                (oddeven (make-lseq 'odd 'even 'odd 'even 'odd 'even 'odd 'even)))
            (test '((one 1 odd) (two 2 even) (three 3 odd))
                (lseq-realize (lseq-zip '(one two three) one2345 oddeven))))
          )

        (test-group "lseqs/mapping"
          (test '() (lseq-map - '()))
          (test '(-1 -2 -3) (lseq-realize (lseq-map - '(1 2 3))))
          (test '(-1 -2 -3) (lseq-realize (lseq-map - (make-lseq 1 2 3))))
          (test-assert (procedure? (cdr (lseq-map - '(1 2 3)))))

          (let* ((output '())
                 (out! (lambda (x) (set! output (cons x output)))))
            (lseq-for-each out! '())
            (test '() output)
            (lseq-for-each out! '(a b c))
            (test '(c b a) output)
            (lseq-for-each out! (make-lseq 1 2 3))
            (test '(3 2 1 c b a) output))

          (test '() (lseq-filter odd? '()))

          (let ((odds (lseq-filter odd? '(1 2 3 4 5))))
            (test-assert (procedure? (cdr odds)))
            (test '(1 3 5) (lseq-realize odds))
            (test '(1 3 5) (lseq-realize (lseq-filter odd? (make-lseq 1 2 3 4 5)))))

          (test '() (lseq-remove even? '()))
          (let ((odds (lseq-remove even? '(1 2 3 4 5))))
            (test-assert (procedure? (cdr odds)))
            (test '(1 3 5) (lseq-realize odds))
            (test '(1 3 5) (lseq-realize (lseq-remove even? (make-lseq 1 2 3 4 5))))))

        (test-group "lseqs/searching"
          (test 4 (lseq-find even? '(3 1 4 1 5 9 2 6)))
          (test 4 (lseq-find even? (make-lseq 3 1 4 1 5 9 2 6)))
          (test #f (lseq-find negative? (make-lseq 1 2 3 4 5)))

          (test '(-8 -5 0 0) (lseq-realize (lseq-find-tail even? '(3 1 37 -8 -5 0 0))))
          (test '(-8 -5 0 0) (lseq-realize (lseq-find-tail even?
                                                           (make-lseq 3 1 37 -8 -5 0 0))))
          (test #f (lseq-find-tail even? '()))
          (test #f (lseq-find-tail negative? (make-lseq 1 2 3 4 5)))

          (test '(2 18) (lseq-realize (lseq-take-while even? '(2 18 3 10 22 9))))
          (test '(2 18) (lseq-realize (lseq-take-while even?
                                                       (make-lseq 2 18 3 10 22 9))))
          (test '(2 18) (lseq-realize (lseq-take-while even?
                                                       (make-lseq 2 18 3 10 22 9))))

          (test '(3 10 22 9) (lseq-drop-while even? '(2 18 3 10 22 9)))
          (test '(3 10 22 9) (lseq-realize (lseq-drop-while even?
                                                            (make-lseq 2 18 3 10 22 9))))

          (test #t (lseq-any integer? '(a 3 b 2.7)))
          (test #t (lseq-any integer? (make-lseq 'a 3 'b 2.7)))
          (test #f (lseq-any integer? '(a 3.1 b 2.7)))
          (test #f (lseq-any integer? (make-lseq 'a 3.1 'b 2.7)))
          (test #t (lseq-any < '(3 1 4 1 5) '(2 7 1 8 2)))
          (test 6 (lseq-any factorial '(-1 -2 3 4)))
          (test 6 (lseq-any factorial (make-lseq -1 -2 3 4)))

          (test 24 (lseq-every factorial '(1 2 3 4)))
          (test 24 (lseq-every factorial (make-lseq 1 2 3 4)))

          (test 2 (lseq-index even? '(3 1 4 1 5 9)))
          (test 1 (lseq-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)))
          (test #f (lseq-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)))

          (test '(a b c) (lseq-realize (lseq-memq 'a '(a b c))))
          (test '(a b c) (lseq-realize (lseq-memq 'a (make-lseq 'a 'b 'c))))
          (test #f (lseq-memq 'a (make-lseq 'b 'c 'd)))
          (test #f (lseq-memq (list 'a) '(b c d)))
          (test #f (lseq-memq (list 'a) (make-lseq 'b 'c 'd)))

          (test '(101 102) (lseq-realize (lseq-memv 101 (make-lseq 100 101 102))))

          (test '((a) c) (lseq-realize (lseq-member (list 'a) (make-lseq 'b '(a) 'c))))
          (test '(2 3) (lseq-realize (lseq-member 2.0 (make-lseq 1 2 3) =)))
          )))
    ))
