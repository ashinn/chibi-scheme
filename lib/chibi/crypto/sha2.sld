
(define-library (chibi crypto sha2)
  (import (chibi io))
  (export sha-224 sha-256)
  (include-shared "crypto"))
