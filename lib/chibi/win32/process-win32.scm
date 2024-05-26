(define unwind #f)

((call/cc
    (lambda (k)
      (set! unwind k)
      (lambda () #f))))

(define (emergency-exit . code?)
  (%exit (if (pair? code?) 
           (let ((c (car code?)))
            (cond ((integer? c) c)
                  ((eq? #t c) 0)
                  (else 1)))
           0)))

(define (exit . o)
  (unwind (lambda () (apply emergency-exit o))))
