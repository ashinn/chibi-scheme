;;> Unified command-line option parsing and config management.

(define-library (chibi app)
  (export parse-option parse-options parse-app run-application
          app-help app-help-command)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (srfi 1)
          (chibi config)
          (chibi string))
  (include "app.scm"))
