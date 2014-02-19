
(define-library (chibi crypto sha2)
  (import (scheme base) (srfi 33) (chibi bytevector))
  (export sha-224 sha-256)
  (include "sha2.scm"))
