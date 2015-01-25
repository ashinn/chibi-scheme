
(define-library (chibi pathname)
  (export path-strip-directory path-directory
          path-extension path-strip-extension path-replace-extension
          path-absolute? path-relative? path-strip-leading-parents
          path-relative-to path-resolve path-normalize make-path)
  (import (chibi) (chibi string))
  (include "pathname.scm"))
