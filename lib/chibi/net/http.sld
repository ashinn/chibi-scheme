
(define-module (chibi net http)
  (export http-get call-with-input-url with-input-from-url
          http-parse-request http-parse-form)
  (import-immutable (scheme) (srfi 39) (chibi net) (chibi io)
                    (chibi uri) (chibi mime))
  (include "http.scm"))
