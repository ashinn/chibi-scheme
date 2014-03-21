
(define-library (chibi net http)
  (export http-get http-get/headers
          call-with-input-url call-with-input-url/headers
          with-input-from-url
          http-parse-request http-parse-form)
  (import (scheme base) (scheme write) (scheme char)
          (srfi 39)
          (chibi net) (chibi io) (chibi uri) (chibi mime))
  (include "http.scm"))
