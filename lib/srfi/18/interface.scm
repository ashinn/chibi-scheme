
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

