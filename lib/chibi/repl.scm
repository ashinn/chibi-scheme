;;;; repl.scm - friendlier repl with line editing and signal handling
;;
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (with-signal-handler sig handler thunk)
  (let ((old-handler #f))
    (dynamic-wind
      (lambda () (set! old-handler (set-signal-action! sig handler)))
      thunk
      (lambda () (set-signal-action! sig old-handler)))))

(define (buffer-complete-sexp? buf)
  (guard (exn (else #f))
    (call-with-input-string (buffer->string buf) read)
    #t))

(define (run-repl module env . o)
  (let ((history (make-history)))
    (let lp ((module module) (env env))
      (let ((line
             (edit-line
              (string-append (if module (symbol->string module) "") "> ")
              'history: history
              'complete?: buffer-complete-sexp?)))
        (cond
         ((or (not line) (eof-object? line)))
         ((equal? line "") (lp module env))
         (else
          (history-commit! history line)
          (guard
           (exn
            (else (print-exception exn (current-error-port))))
           (let* ((expr (call-with-input-string line read/ss))
                  (thread
                   (make-thread
                    (lambda ()
                      (guard
                       (exn
                        (else (print-exception exn (current-error-port))))
                       (let ((res (eval expr env)))
                         (cond
                          ((not (eq? res (if #f #f)))
                           (write/ss res)
                           (newline)))))))))
             (with-signal-handler
              signal/interrupt
              (lambda (n)
                (display "Interrupt\n" (current-error-port))
                (thread-terminate! thread))
              (lambda () (thread-join! (thread-start! thread))))))
          (lp module env)))))))

(define (repl)
  (run-repl #f (interaction-environment)))
