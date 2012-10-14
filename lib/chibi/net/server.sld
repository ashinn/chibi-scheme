
(define-library (chibi net server)
  (import (chibi) (chibi net) (chibi filesystem) (srfi 18))
  (export run-net-server)
  (include "server.scm"))
