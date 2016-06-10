(define-library (srfi 124)
  (export make-ephemeron ephemeron? ephemeron-broken?
	  ephemeron-key ephemeron-datum)
  (import (rename (chibi weak) (ephemeron-value ephemeron-datum))))
