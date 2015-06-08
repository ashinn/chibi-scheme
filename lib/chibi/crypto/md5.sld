
;;> Implementation of the MD5 (Message Digest) cryptographic hash.  In
;;> new applications SHA-2 should be preferred.

(define-library (chibi crypto md5)
  (import (scheme base) (chibi bytevector))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (export md5)
  (include "md5.scm"))
