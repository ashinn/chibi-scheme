
(define-library (srfi 2)
  (export and-let*)
  (import (chibi))
  (begin
   (define-syntax and-let*
     (syntax-rules ()
       ((and-let* () . body)
        (begin . body))
       ((and-let* ((var expr) . rest) . body)
        (let ((var expr))
          (and var (and-let* rest . body))))
       ((and-let* ((expr) . rest) . body)
        (let ((tmp expr))
          (and tmp (and-let* rest . body))))))))
