
(define thread-yield! yield!)

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

(define (time->seconds x)
  (timeval-seconds (if (pair? x) (car x) x)))

(define (seconds->time x)
  (make-timeval (if (inexact? x) (inexact->exact x) x) 0))

(define (join-timeout-exception? x)
  (and (exception? x)
       (equal? (exception-message x) "timed out waiting for thread")))

;; XXXX flush out exception types
(define (abandoned-mutex-exception? x) #f)
(define (terminated-thread-exception? x) #f)
(define (uncaught-exception? x) #f)
(define (uncaught-exception-reason x) #f)

;; signal runner

(define (signal-runner)
  (let lp ()
    (let ((n (pop-signal!)))
      (cond
       ((integer? n)
        (let ((handler (get-signal-handler n)))
          (if (procedure? handler)
              (handler n))))
       (else
        (thread-sleep! #t))))
    (lp)))
