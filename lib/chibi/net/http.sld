
(define-library (chibi net http)
  (export http-get http-get/headers http-get-to-file
          http-head http-post http-put http-delete
          call-with-input-url call-with-input-url/headers
          with-input-from-url
          http-parse-request http-parse-form)
  (import (scheme base) (scheme write) (scheme char) (scheme file)
          (srfi 27) (srfi 39)
          (chibi net) (chibi io) (chibi uri) (chibi mime))
  (include "http.scm"))
