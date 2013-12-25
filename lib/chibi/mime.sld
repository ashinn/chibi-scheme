
(define-library (chibi mime)
  (export assq-ref mime-header-fold mime-headers->list
          mime-parse-content-type mime-decode-header
          mime-message-fold mime-message->sxml mime-write-headers)
  (import (chibi) (chibi base64) (chibi quoted-printable)
          (chibi string) (chibi io))
  (include "mime.scm"))
