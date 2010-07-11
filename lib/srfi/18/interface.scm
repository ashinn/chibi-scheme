
(define (thread-join! thread . o)
  (let ((timeout (if (pair? o) (car o) #f)))
    (cond
     ((%thread-join! thread timeout))
     (else
      (thread-yield!)
      (if (thread-timeout?)
          (if (and (pair? o) (pair? (cdr o)))
              (cadr o)
              (error "timed out waiting for thread" thread))
	  #t)))))

(define (thread-terminate! thread)
  (if (%thread-terminate! thread) ;; need to yield if terminating ourself
      (thread-yield!)))

(define (thread-sleep! timeout)
  (%thread-sleep! timeout)
  (thread-yield!))

(define (mutex-lock! mutex . o)
  (let ((timeout (and (pair? o) (car o)))
        (thread (if (and (pair? o) (pair? (cdr o))) (cadr o) #t)))
    (cond ((%mutex-lock! mutex timeout thread))
	  (else
	   (thread-yield!)
	   (not (thread-timeout?))))))

(define (mutex-unlock! mutex . o)
  (let ((condvar (and (pair? o) (car o)))
        (timeout (if (and (pair? o) (pair? (cdr o))) (cadr o) #f)))
    (cond ((%mutex-unlock! mutex condvar timeout))
	  (else
	   (thread-yield!)
	   (not (thread-timeout?))))))

(define current-time get-time-of-day)
(define time? timeval?)
