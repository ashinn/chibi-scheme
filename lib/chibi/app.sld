
(define-library (chibi app)
  (export parse-option parse-options parse-app run-application)
  (import (scheme base)
          (scheme process-context)
          (srfi 1)
          (chibi config)
          (chibi string))
  (include "app.scm"))
