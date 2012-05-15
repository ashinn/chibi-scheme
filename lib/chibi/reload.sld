
(define-library (chibi reload)
  (import (scheme)
          (meta)
          (srfi 39)
          (only (chibi time) current-seconds)
          (only (chibi filesystem) file-modification-time))
  (include "reload.scm")
  (export reload reload-modified-modules reload-verbose?))
