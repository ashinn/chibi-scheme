
(define-library (chibi optional)
  (export let-optionals let-optionals* opt-lambda
          let-keywords let-keywords* keyword-ref)
  (import (chibi))
  (include "optional.scm"))
