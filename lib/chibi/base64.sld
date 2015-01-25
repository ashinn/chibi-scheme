
(define-library (chibi base64)
  (export base64-encode base64-encode-string base64-encode-bytevector
          base64-decode base64-decode-string base64-decode-bytevector
          base64-encode-header)
  (import (scheme base) (srfi 33) (chibi io)
          (only (chibi) string-concatenate))
  (include "base64.scm"))
