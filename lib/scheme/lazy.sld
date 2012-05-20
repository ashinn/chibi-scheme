
(define-library (scheme lazy)
  (import (scheme))
  (export delay force delay-force make-promise)
  (begin (define (make-promise x) (delay x))))
