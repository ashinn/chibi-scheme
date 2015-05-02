
;;> A general, non-filesystem-specific pathname library.

(define-library (chibi pathname)
  (export path-strip-directory path-directory
          path-extension path-strip-extension path-replace-extension
          path-absolute? path-relative? path-strip-leading-parents
          path-relative-to path-resolve path-normalize make-path)
  (cond-expand
   (chibi (import (chibi)))
   (else (import (except (scheme base) string-map string-for-each))))
  (import (chibi string))
  (include "pathname.scm"))
