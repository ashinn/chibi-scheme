
(define-library (chibi repl)
  (export repl)
  (import (scheme) (only (meta) load-module)
          (chibi ast) (chibi io) (chibi process) (chibi term edit-line)
          (srfi 18) (srfi 38) (srfi 98))
  (include "repl.scm"))
