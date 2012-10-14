
(define-library (chibi pathname)
  (export path-strip-directory path-directory ;; path-extension-pos
          path-extension path-strip-extension path-replace-extension
          path-absolute? path-relative? path-normalize make-path)
  (import (chibi))
  (include "pathname.scm"))
