
(define-library (chibi parse common)
  (export parse-integer parse-unsigned-integer parse-c-integer
          parse-real parse-complex
          parse-identifier parse-delimited parse-separated parse-records
          parse-space parse-binary-op
          parse-ipv4-address parse-ipv6-address parse-ip-address
          parse-domain parse-common-domain parse-email parse-uri
          char-hex-digit? char-octal-digit?)
  (cond-expand
   (chibi (import (chibi)))
   (else (import (scheme base) (scheme char))))
  (import (chibi parse))
  (include "common.scm"))
