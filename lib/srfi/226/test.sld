(define-library (srfi 226 test)
  (export
    run-tests)
  (import
    (except (scheme base)
            call-with-current-continuation
            call/cc
            dynamic-wind)
    (srfi 226 prompt)
    (srfi 226 continuation)
    (srfi 226 shift-reset)
    (chibi test))

  (begin

    (define tag (make-continuation-prompt-tag))

    (define-syntax prompt
      (syntax-rules ()
        ((prompt e1 e2 ...)
         (call-with-continuation-prompt
          (lambda ()
	    e1 e2 ...)
          (default-continuation-prompt-tag)
          (lambda (thunk)
	    (thunk))))))

    (define-syntax control
      (syntax-rules ()
        ((control k e1 e2 ...)
         (call-with-composable-continuation
          (lambda (k)
	    (abort-current-continuation (default-continuation-prompt-tag)
	      (lambda ()
	        e1 e2 ...)))))))

    (define (run-tests)

      (test-begin "srfi-226")

      (test #t (continuation-prompt-tag? (default-continuation-prompt-tag)))

      (test #t (eq? (default-continuation-prompt-tag) (default-continuation-prompt-tag)))

      (test #f (equal? (make-continuation-prompt-tag) (default-continuation-prompt-tag)))

      (test #f (equal? (make-continuation-prompt-tag) (make-continuation-prompt-tag)))

      (test '(foo bar)
            (let ((tag (make-continuation-prompt-tag)))
              (call-with-continuation-prompt
               (lambda ()
                 (+ 1
                    (abort-current-continuation tag 'foo 'bar)
                    2))
               tag
               list)))

      (test 27
            (let ((tag (make-continuation-prompt-tag)))
              (call-with-continuation-prompt
               (lambda ()
                 (abort-current-continuation tag
                   (lambda ()
                     (abort-current-continuation tag
                       (lambda ()
                         27)))))
               tag
               #f)))

      (test 990
            (let ((tag (make-continuation-prompt-tag)))
              (* 2
                 (call-with-continuation-prompt
                  (lambda ()
                    (* 3
                       (call-with-non-composable-continuation
                        (lambda (k)
                          (* 5
                             (call-with-continuation-prompt
                              (lambda ()
                                (* 7 (k 11)))
                              tag)))
                        tag)))
                  tag))))

      (test 6930
            (let ((tag (make-continuation-prompt-tag)))
              (* 2
                 (call-with-continuation-prompt
                  (lambda ()
                    (* 3
                       (call-with-composable-continuation
                        (lambda (k)
                          (* 5
                             (call-with-continuation-prompt
                              (lambda ()
                                (* 7 (k 11)))
                              tag)))
                        tag)))
                  tag))))

      (test 4 (+ 1 (reset 3)))

      (test 5 (+ 1 (reset (* 2 (shift k 4)))))

      (test 9 (+ 1 (reset (* 2 (shift k (k 4))))))

      (test 17 (+ 1 (reset (* 2 (shift k (k (k 4)))))))

      (test 25 (+ 1 (reset (* 2 (shift k1 (* 3 (shift k2 (k1 (k2 4)))))))))

      (test 7 (prompt (+ 2 (control k (k 5))))) 7

      (test 5 (prompt (+ 2 (control k 5)))) 5

      (test 12 (prompt (+ 5 (prompt (+ 2 (control k1 (+ 1 (control k2 (k2 6)))))))))

      (test 8 (prompt (+ 5 (prompt (+ 2 (control k1 (+ 1 (control k2 (k1 6))))))))) 8

      (test 18 (prompt
                (+ 12 (prompt (+ 5 (prompt (+ 2 (control
                                                 k1 (control
                                                     k2 (control
                                                         k3 (k3 6)))))))))))

      (test-error
       ((call-with-continuation-barrier
         (lambda ()
           (call/cc values)))))

      (test 'ok
            (call/cc
             (lambda (k)
               (call-with-continuation-barrier
                (lambda ()
                  (k 'ok))))))

      (test #t
            (call-with-continuation-prompt
             (lambda ()
               (continuation-prompt-available?
                tag
                (call-with-non-composable-continuation values)))
             tag))

      (test #t
            (call-with-continuation-prompt
             (lambda ()
               (continuation-prompt-available?
                tag
                (call-with-non-composable-continuation values tag)))
             tag))

      (test #f
            (call-with-continuation-prompt
             (lambda ()
               (continuation-prompt-available?
                tag
                (call-with-composable-continuation values tag)))
             tag))

      (test 7
            (let ((n 0))
              (call/cc
               (lambda (k)
                 (dynamic-wind
                   values
                   (lambda ()
                     (dynamic-wind
                       values
                       (lambda ()
                         (set! n (+ n 1))
                         (k))
                       (lambda ()
                         (set! n (+ n 2))
                         (k))))
                   (lambda ()
                     (set! n (+ n 4))))))
              n))

      (test-end))))


;; Local Variables:
;; mode: scheme
;; End:
