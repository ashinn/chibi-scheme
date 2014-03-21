
(define-library (chibi quoted-printable)
  (export quoted-printable-encode quoted-printable-encode-string
          quoted-printable-encode-bytevector
          quoted-printable-encode-header
          quoted-printable-decode quoted-printable-decode-string
          quoted-printable-decode-bytevector)
  (import (scheme base) (srfi 33) (chibi io))
  (include "quoted-printable.scm"))
