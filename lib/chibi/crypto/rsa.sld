
(define-library (chibi crypto rsa)
  (import (scheme base) (srfi 33) (chibi bytevector) (chibi math prime))
  (export make-rsa-key rsa-key-gen rsa-key-gen-from-primes rsa-pub-key
          rsa-encrypt rsa-decrypt rsa-sign rsa-verify?
          rsa-key? rsa-key-bits rsa-key-n rsa-key-e rsa-key-d)
  (include "rsa.scm"))
