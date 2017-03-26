
(define-library (chibi net http)
  (export http-get http-get/headers http-get-to-file
          http-head http-post http-put http-delete
          call-with-input-url call-with-input-url/headers
          with-input-from-url
          http-parse-request http-parse-form)
  (import (scheme base) (scheme write) (scheme char) (scheme file)
          (srfi 27)
          (chibi uri) (chibi mime))
  (cond-expand
   (chicken
    (import (only (chicken) parameterize))
    (import (only (ports) make-input-port))
    (import (only (tcp) tcp-connect))
    (begin
      (define (make-custom-binary-input-port read-bv)
        (let ((bv (make-bytevector 1024))
              (off 0)
              (fill 0))
          (define (refill!)
            (set! off 0)
            (set! fill (read-bv bv 0 1024)))
          (make-input-port
           (lambda ()
             (if (>= off fill)
                 (refill!))
             (if (< off fill)
                 (read-char (open-input-string ""))
                 (let ((res (integer->char (bytevector-u8-ref bv off))))
                   (set! off (+ 1 off))
                   res)))
           (lambda ()
             (or (< off fill)
                 (begin (refill!) (< off fill))))
           (lambda () #f))))
      (define (open-net-io host port . o)
        (call-with-values (lambda () (tcp-connect host port))
          (lambda (in out)
            (list #f in out))))
      (define (port->bytevector in)
        (let ((out (open-output-bytevector)))
          (do ((c (read-u8 in) (read-u8 in)))
              ((eof-object? c) (get-output-bytevector out))
            (write-u8 c out))))))
   (else
    (import (srfi 39) (chibi io) (chibi net))))
  (include "http.scm"))
