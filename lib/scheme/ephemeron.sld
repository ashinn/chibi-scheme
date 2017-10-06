
(define-library (scheme ephemeron)
  (import (srfi 124))
  (export make-ephemeron ephemeron? ephemeron-broken?
          ephemeron-key ephemeron-datum))
