
(define-library (scheme lazy)
  (import (chibi))
  (export delay force delay-force make-promise)
  (begin (define (make-promise x) (delay x))))
