
(define-library (scheme lazy)
  (import (scheme))
  (export delay force lazy eager)
  (begin (define (eager x) (delay x))))
