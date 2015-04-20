
(define-library (chibi crypto sha2)
  (import (scheme base))
  (export sha-224 sha-256)
  (cond-expand
   (chibi
    (include "sha2-native.scm")
    (include-shared "crypto"))
   (else
    (import (srfi 33) (chibi bytevector))
    (include "sha2.scm"))))

;;> \procedure{(sha-224 src)}
;;>
;;> Computes SHA-224 digest of the \var{src} which can be a string,
;;> a bytevector, or a binary input port. Returns a hexadecimal string
;;> (in lowercase).

;;> \procedure{(sha-256 src)}
;;>
;;> Computes SHA-256 digest of the \var{src} which can be a string,
;;> a bytevector, or a binary input port. Returns a hexadecimal string
;;> (in lowercase).
