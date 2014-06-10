
(define-library (snow utils)
  (export copy-file move-file http-get-to-file/cached
          call-with-temp-file call-with-temp-dir create-directory*
          path-strip-leading-parents
          file-sha256)
  (import (scheme base) (scheme char) (scheme write) (scheme time)
          (snow interface)
          (srfi 1) (srfi 33)
          (chibi string) (chibi pathname) (chibi uri)
          (chibi filesystem) (chibi process) (chibi net http))
  (include "utils.scm"))
