
(define-library (chibi base64)
  (export base64-encode base64-encode-string
          base64-decode base64-decode-string
          base64-encode-header)
  (import (chibi) (srfi 33) (chibi io))
  (include "base64.scm"))
