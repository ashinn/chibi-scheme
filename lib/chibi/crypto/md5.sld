
(define-library (chibi crypto md5)
  (import (scheme base) (srfi 33) (chibi bytevector))
  (export md5)
  (include "md5.scm"))
