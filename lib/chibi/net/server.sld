
(define-library (chibi net server)
  (import (chibi) (chibi net) (chibi filesystem) (chibi log)
          (srfi 18) (srfi 98))
  (export run-net-server make-listener-thunk)
  (include "server.scm"))
