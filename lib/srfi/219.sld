(define-library (srfi 219)
  (export define)
  (import (rename (scheme base) (define native-define)))
  (begin  (define-syntax define
            (syntax-rules ()
              ((define ((name . outer-args) . args) . body)
               (define (name . outer-args) (lambda args . body)))
              ((define head . body)
               (native-define head . body))))))
