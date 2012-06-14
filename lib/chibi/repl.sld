
(define-library (chibi repl)
  (export repl)
  (import (scheme) (only (meta) load-module)
          (chibi ast) (chibi strings) (chibi io)
          (chibi process) (chibi term edit-line)
          (srfi 1) (srfi 18) (srfi 38) (srfi 95) (srfi 98))
  (include "repl.scm"))
