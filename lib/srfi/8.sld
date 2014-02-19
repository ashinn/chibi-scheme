
(define-library (srfi 8)
  (export receive)
  (import (chibi))
  (body
   (define-syntax receive
     (syntax-rules ()
       ((receive params expr . body)
        (call-with-values (lambda () expr) (lambda params . body)))))))
