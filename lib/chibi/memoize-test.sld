(define-library (chibi memoize-test)
  (export run-tests)
  (import (scheme base)
          (scheme file)
          (chibi filesystem)
          (chibi memoize)
          (chibi pathname)
          (chibi process)
          (chibi temp-file)
          (chibi test))
  (begin
    (define (run-tests)
      (test-begin "memoize")

      (let ()
        (define-memoized (fib n)
          (if (<= n 1)
              1
              (+ (fib (- n 1)) (fib (- n 2)))))
        (test 1 (fib 1))
        (test 573147844013817084101 (fib 100)))

      (let ()
        (define-memoized (ack m n)
          (cond
           ((= m 0) (+ n 1))
           ((= n 0) (ack (- m 1) 1))
           (else (ack (- m 1) (ack m (- n 1))))))
        (test 29 (ack 3 2))
        (test 61 (ack 3 3)))

      (let ((n 0))
        (let ((f (memoize (lambda (x) (set! n (+ n 1)) (* x x)))))
          (test 0 n)
          (test 9 (f 3))
          (test 1 n)
          (test 9 (f 3))
          (test 1 n)))

      (let ((n 0))
        (let ((f (memoize (lambda (x) (set! n (+ n 1)) (* x x))
                          'size-limit: #f)))
          (test 0 n)
          (test 9 (f 3))
          (test 1 n)
          (test 9 (f 3))
          (test 1 n)))

      (let ((calls 0))
        (letrec ((fib (lambda (n)
                        (set! calls (+ calls 1))
                        (if (<= n 1)
                            1
                            (+ (fib (- n 1)) (fib (- n 2)))))))
         (call-with-temp-dir
          "memo.d"
          (lambda (dir preserve)
            (let ((f (memoize-to-file fib 'memo-dir: dir)))
              (test 89 (f 10))
              (test 177 calls)
              ;; (test-assert (file-exists? (make-path dir "%2810%29.memo")))
              (test 89 (f 10))
              (test 177 calls))))))

      (call-with-temp-file
       "tmp-file"
       (lambda (tmp-file out preserve)
         (write-string "123" out)
         (close-output-port out)
         (let ((calls 0))
           (let ((fast-file-size
                 (memoize-file-loader
                  (lambda (file)
                    (set! calls (+ calls 1))
                    (file-size file)))))
             (test 3 (fast-file-size tmp-file))
             (test 1 calls)
             (test 3 (fast-file-size tmp-file))
             (test 1 calls)
             (sleep 1)
             (call-with-output-file tmp-file
               (lambda (out) (write-string "1234" out)))
             (test 4 (fast-file-size tmp-file))
             (test 2 calls)
             (test 4 (fast-file-size tmp-file))
             (test 2 calls)
             ))))

      (test-end))))
