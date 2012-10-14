
(define-library (chibi mime)
  (export mime-ref assoc-ref mime-header-fold mime-headers->list
          mime-parse-content-type mime-decode-header
          mime-message-fold mime-message->sxml)
  (import (chibi) (chibi base64) (chibi quoted-printable) (chibi io))
  (include "mime.scm"))
