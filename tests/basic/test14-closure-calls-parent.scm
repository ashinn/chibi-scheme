
;; A lambda in tail position of f is not f: its call to f must not be
;; compiled as a self tail call (a jump to the lambda's own loop start).
(define (f n)
  (if (= n 0)
      (lambda (k) (f k))
      (list 'f n)))

(write ((f 0) 5))
(newline)
