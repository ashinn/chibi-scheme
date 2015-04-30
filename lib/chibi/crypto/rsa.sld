
;;> RSA public key cryptography implementation.

(define-library (chibi crypto rsa)
  (import (scheme base) (srfi 27)
          (chibi bytevector) (chibi math prime))
  (cond-expand
   ((library (srfi 60)) (import (srfi 60)))
   (else (import (srfi 33))))
  (export make-rsa-key rsa-key-gen rsa-key-gen-from-primes rsa-pub-key
          rsa-encrypt rsa-decrypt rsa-sign rsa-verify rsa-verify?
          rsa-key? rsa-key-bits rsa-key-n rsa-key-e rsa-key-d
          pkcs1-pad pkcs1-unpad)
  (include "rsa.scm"))
