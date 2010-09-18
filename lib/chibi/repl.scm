;;;; repl.scm - friendlier repl with line editing and signal handling
;;
;; Copyright (c) 2010 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-syntax handle-exceptions
  (syntax-rules ()
    ((handle-exceptions exn handler expr)
     (call-with-current-continuation
      (lambda (return)
        (with-exception-handler (lambda (exn) (return handler))
                                (lambda () expr)))))))

(define (with-signal-handler sig handler thunk)
  (let ((old-handler #f))
    (dynamic-wind
        (lambda () (set! old-handler (set-signal-action! sig handler)))
        thunk
        (lambda () (set-signal-action! sig old-handler)))))

(define (run-repl module env . o)
  (let ((history (make-history)))
    (let lp ((module module) (env env))
      (let ((line (edit-line (if module (string-append (symbol->string module) "> ") "> ")
                             'history: history)))
        (cond
         ((or (not line) (eof-object? line)))
         ((equal? line "") (lp module env))
         (else
          (history-commit! history line)
          (handle-exceptions
           exn (print-exception exn (current-error-port))
           (let* ((expr (call-with-input-string line read/ss))
                  (thread (make-thread (lambda ()
                                         (let ((res (eval expr env)))
                                           (if (not (eq? res (if #f #f)))
                                               (write/ss res))
                                           (newline))))))
             (with-signal-handler
              signal/interrupt
              (lambda (n) (thread-terminate! thread))
              (lambda () (thread-start! thread) (thread-join! thread)))))
          (lp module env)))))))

(define (repl)
  (run-repl #f (interaction-environment)))
