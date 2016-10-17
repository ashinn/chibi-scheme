;; utilities for the snow repo server

(define-library (chibi snow fort)
  (export fail page respond static-url static-local-path
          escape-path email->path maybe-parse-hex
          valid-email? valid-package?
          extract-snowball-package package-dir
          invalid-signature-reason
          rewrite-repo update-repo
          update-repo-package update-repo-object
          repo-publishers current-repo get-user-password)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme file)
          (srfi 1)
          (srfi 18)
          (chibi snow package)
          (chibi bytevector)
          (chibi config)
          (chibi crypto rsa)
          (chibi filesystem)
          (chibi io)
          (chibi log)
          (chibi net servlet)
          (chibi pathname)
          (chibi regexp)
          (chibi string)
          (chibi sxml)
          (chibi tar))
  (cond-expand
   ((library (srfi 33))
    (import (srfi 33)))
   (else
    (import (srfi 60))))
  (cond-expand
   (chibi
    (import (only (chibi ast)
                  errno integer->error-string)
            (only (chibi)
                  string-size exception-protect
                  call-with-input-string call-with-output-string)))
   (else
    (begin
      (define (errno) 0)
      (define (integer->error-string n)
        (string-append "errno: " (number->string n)))
      (define string-size string-length)
      (define (call-with-input-string str proc)
        (let* ((in (open-input-string str))
               (res (proc in)))
          (close-input-port in)
          res))
      (define (call-with-output-string proc)
        (let ((out (open-output-string)))
          (proc out)
          (let ((res (get-output-string out)))
            (close-output-port out)
            res)))
      (define (with-exception-protect thunk final)
        (let* ((finalized? #f)
               (run-finalize
                (lambda ()
                  (cond ((not finalized?)
                         (set! finalized? #t)
                         (final))))))
          (guard (exn (else (run-finalize) (raise exn)))
            (let ((res (thunk)))
              (run-finalize)
              res))))
      (define-syntax exception-protect
        (syntax-rules ()
          ((exception-protect expr final)
           (with-exception-protect (lambda () expr)  (lambda () final))))))))
  (include "fort.scm"))
