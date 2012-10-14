
(define-library (chibi repl)
  (export repl)
  (import (chibi) (only (meta) load-module)
          (chibi ast) (chibi string) (chibi io)
          (chibi process) (chibi term edit-line)
          (srfi 1) (srfi 18) (srfi 38) (srfi 95) (srfi 98))
  (include "repl.scm"))
