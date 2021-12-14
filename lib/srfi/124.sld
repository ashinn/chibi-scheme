(define-library (srfi 124)
  (export make-ephemeron ephemeron? ephemeron-broken?
          ephemeron-key ephemeron-datum reference-barrier)
  (import (rename (chibi weak) (ephemeron-value ephemeron-datum)))
  (begin
    (define (reference-barrier k) (if #f #f))))
