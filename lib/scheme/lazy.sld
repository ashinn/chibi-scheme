
(define-library (scheme lazy)
  (import (chibi))
  (export delay force delay-force make-promise promise?)
  (begin
    (define (make-promise x)
      (if (promise? x) x (delay x)))))
