;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define default-max-requests 10000)

(define (run-net-server listener-or-addr handler . o)
  (let* ((listener (cond
                    ((fileno? listener-or-addr)
                     listener-or-addr)
                    ((integer? listener-or-addr)
                     (make-listener-socket
                      (get-address-info "localhost" listener-or-addr)))
                    (else
                     (make-listener-socket listener-or-addr))))
         (max-requests (if (pair? o) (car o) default-max-requests))
         (debug? (and (pair? o) (pair? (cdr o)))))
    (define (log-error msg . args)
      (display msg (current-error-port))
      (for-each
       (lambda (x)
         (write-char #\space (current-error-port))
         (display x (current-error-port)))
       args)
      (newline (current-error-port)))
    (define (log-debug msg . args)
      (if debug? (apply log-error msg args)))
    (define (run sock addr count)
      (log-debug "net-server: accepting request:" count)
      (let ((ports
             (guard (exn
                     (else
                      (log-error "net-server: couldn't create port:" sock)
                      (close-file-descriptor sock)))
               (cons (open-input-file-descriptor sock)
                     (open-output-file-descriptor sock)))))
        (guard (exn
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
            (let* ((addr (get-address-info "127.0.0.1" "8000"))
                   (sock (accept listener
                                 (address-info-address addr)
                                 (address-info-address-length addr))))
              (cond
               ((not sock)
                (serve count))
               (else
                (thread-start!
                 (make-thread
                  (lambda ()
                    (set! requests (+ requests 1))
                    (run sock addr count)
                    (set! requests (- requests 1)))
                  (string-append "net-client-" (number->string count))))
                (serve (+ 1 count))))))))))
