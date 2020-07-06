
(define-library (chibi monad environment)
  (export define-environment-monad)
  (import (scheme base))
  (cond-expand
   (chibi (import (only (chibi) syntax-quote)))
   (else (begin (define-syntax syntax-quote (syntax-rules ((_ x) 'x))))))
  (include "environment.scm"))
