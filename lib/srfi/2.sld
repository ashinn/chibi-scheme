
(define-library (srfi 2)
  (export and-let*)
  (import (chibi))
  (begin
   (define-syntax and-let*
     (syntax-rules ()
       ((and-let* () . body)
        (begin #t . body))
       ((and-let* ((var expr)))
        expr)
       ((and-let* ((expr)))
        expr)
       ((and-let* (expr))  ; Extension: in SRFI-2 this can only be a var ref
        expr)
       ((and-let* ((var expr) . rest) . body)
        (let ((var expr))
          (and var (and-let* rest . body))))
       ((and-let* ((expr) . rest) . body)
        (and expr (and-let* rest . body)))
       ((and-let* (expr . rest) . body)   ; Same extension as above
        (let ((tmp expr))
          (and tmp (and-let* rest . body))))))))
