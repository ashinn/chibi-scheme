
(define-library (chibi snow utils)
  (export find-in-path find-sexp-in-path
          write-to-string resource->bytevector uri-normalize uri-directory
          version-split version-compare version>? version>=?)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (srfi 1)
          (chibi io)
          (chibi net http)
          (chibi pathname)
          (chibi string)
          (chibi uri))
  (include "utils.scm"))
