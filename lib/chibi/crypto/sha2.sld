
(define-library (chibi crypto sha2)
  (import (scheme base) (srfi 33) (chibi bytevector))
  (export sha-224 sha-256)
  (include "sha2.scm"))

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
