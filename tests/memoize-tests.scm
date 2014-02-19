
(import (chibi) (chibi memoize) (chibi filesystem) (chibi test))

(test-begin "memoize")

(define-memoized (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(test 1 (fib 1))
(test 573147844013817084101 (fib 100))

(define-memoized (ack m n)
  (cond
   ((= m 0) (+ n 1))
   ((= n 0) (ack (- m 1) 1))
   (else (ack (- m 1) (ack m (- n 1))))))

(test 29 (ack 3 2))
(test 61 (ack 3 3))

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

(letrec ((fib (lambda (n)
                (if (<= n 1)
                    1
                    (+ (fib (- n 1)) (fib (- n 2)))))))
  (let ((f (memoize-to-file fib 'memo-dir: "/tmp/memo.d/")))
    (test 89 (f 10))
    (test-assert (file-exists? "/tmp/memo.d/10.memo"))
    (test 89 (f 10))))

(test-end)
