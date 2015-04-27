(define-library (euler totient)
  (export totient)
  (import (scheme base) (scheme inexact))
  (include "totient-impl.scm"))
