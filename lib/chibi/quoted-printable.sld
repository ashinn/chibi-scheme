
(define-library (chibi quoted-printable)
  (export quoted-printable-encode quoted-printable-encode-string
          quoted-printable-encode-header
          quoted-printable-decode quoted-printable-decode-string)
  (import (chibi) (srfi 33) (chibi io))
  (include "quoted-printable.scm"))
