
;;> Implementation of the MD5 (Message Digest) cryptographic hash.  In
;;> new applications SHA-2 should be preferred.

(define-library (chibi crypto md5)
  (import (scheme base) (srfi 33) (chibi bytevector))
  (export md5)
  (include "md5.scm"))
