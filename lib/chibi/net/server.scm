;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define default-max-requests 10000)

(define (make-socket-listener-thunk listener port)
  (lambda ()
    (let ((addr (get-address-info #f port)))
      (cond
       ((accept listener
                (address-info-address addr)
                (address-info-address-length addr))
        => (lambda (sock) (list sock addr)))
       (else #f)))))

(define (make-listener-thunk x)
  (cond
   ((integer? x)
    (make-socket-listener-thunk
     (make-listener-socket (get-address-info #f x))
     x))
   ((address-info? x)
    (make-socket-listener-thunk (make-listener-socket x) 80))
   ((fileno? x)
    (make-socket-listener-thunk x 80))
   ((procedure? x)
    x)
   (else
    (error "expected a listener socket, fileno or thunk" x))))

(define (run-net-server listener-or-addr handler . o)
  (let ((listener-thunk (make-listener-thunk listener-or-addr))
        (max-requests (if (pair? o) (car o) default-max-requests)))
    (define (run sock addr count)
      (log-debug "net-server: accepting request:" count)
      (let ((ports
             (protect (exn
                       (else
                        (log-error "net-server: couldn't create port:" sock)
                        (close-file-descriptor sock)))
               (cons (open-input-file-descriptor sock)
                     (open-output-file-descriptor sock)))))
        (protect (exn
                  (else (log-error "net-server: error in request:" count)
                        (print-exception exn)
                        (print-stack-trace exn)
                        (close-input-port (car ports))
                        (close-output-port (cdr ports))
                        (close-file-descriptor sock)))
          (handler (car ports) (cdr ports) sock addr)
          (close-input-port (car ports))
          (close-output-port (cdr ports))
          (close-file-descriptor sock)))
      (log-debug "net-server: finished: " count))
    (let ((requests 0))
      (let serve ((count 0))
        (if (>= requests  max-requests)
            (thread-yield!)
            (let ((sock+addr (listener-thunk)))
              (cond
               ((not sock+addr)
                (serve count))
               (else
                (thread-start!
                 (make-thread
                  (lambda ()
                    (set! requests (+ requests 1))
                    (run (car sock+addr) (cadr sock+addr) count)
                    (set! requests (- requests 1)))
                  (string-append "net-client-" (number->string count))))
                (serve (+ 1 count))))))))))
