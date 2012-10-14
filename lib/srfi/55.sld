
(define-library (srfi 55)
  (export require-extension)
  (import (chibi))
  (begin
   (define-syntax require-extension
     (syntax-rules ()
       ((require-extension (prefix mod ...))
        (begin (import (prefix mod) ...)))))))
