
(define (thread-join! thread . o)
  (let ((timeout (if (pair? o) (car o) #f)))
    (cond
     ((%thread-join! thread timeout))
     (else
      (thread-yield!)
      (if (thread-timeout?)
          (if (and (pair? o) (pair? (cdr o)))
              (cadr o)
              (error "timed out waiting for thread" thread)))))))

(define (thread-terminate! thread)
  (if (%thread-terminate! thread) ;; need to yield if terminating ourself
      (thread-yield!)))

(define (thread-sleep! timeout)
  (%thread-sleep! timeout)
  (thread-yield!))

(define (mutex-lock! mutex . o)
  (let ((timeout (and (pair? o) (car o)))
        (thread (if (and (pair? o) (pair? (cdr o))) (cadr o) #t)))
    (if (not (%mutex-lock! mutex timeout thread))
        (thread-yield!))))

(define (mutex-unlock! mutex . o)
  #f)

(define current-time get-time-of-day)
(define time? timeval?)
