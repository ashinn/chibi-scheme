(define-library (srfi 227)
  (export opt-lambda
          (rename opt-lambda* opt*-lambda)
          let-optionals
          let-optionals*
          (rename define-opt define-optionals)
          (rename define-opt* define-optionals*))
  (import (chibi optional)))
