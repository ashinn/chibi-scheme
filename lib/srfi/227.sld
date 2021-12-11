(define-library (srfi 227)
  (export opt-lambda
          (rename opt-lambda* opt*-lambda)
          let-optionals
          let-optionals*)
  (import (chibi optional)))
