
(define-library (chibi config)
  (export make-conf conf? conf-load conf-load-in-path conf-load-cascaded
          conf-verify conf-extend conf-append conf-set conf-unfold-key
          conf-get conf-get-list conf-get-multi
          conf-specialize read-from-file)
  (import (scheme base) (scheme read) (scheme write) (scheme file)
          (scheme time) (srfi 1))
  ;; This is only used for config verification, it's acceptable to
  ;; substitute file existence for the stronger directory check.
  (cond-expand
   (chibi (import (only (chibi filesystem) file-directory?)))
   (else (begin (define file-directory? file-exists?))))
  (include "config.scm"))
