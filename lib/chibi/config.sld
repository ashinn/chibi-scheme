
(define-library (chibi config)
  (export make-conf conf? conf-load conf-load-in-path conf-load-cascaded
          conf-verify conf-extend conf-append conf-set conf-unfold-key
          conf-get conf-get-list conf-get-cdr conf-get-multi
          conf-specialize read-from-file conf-source conf-head conf-parent
          assoc-get assoc-get-list)
  (import (scheme base) (scheme read) (scheme write) (scheme file)
          (scheme time) (srfi 1))
  ;; This is only used for config verification, it's acceptable to
  ;; substitute file existence for the stronger directory check.
  (cond-expand
   (chibi
    (import (only (meta) warn))
    (import (only (chibi) print-exception print-stack-trace))
    (import (only (chibi filesystem) file-directory?)))
   (else
    (begin
      (define file-directory? file-exists?)
      (define (print-exception exn) (write exn))
      (define (print-stack-trace . o) #f)
      (define (warn msg . args)
        (let ((err (current-error-port)))
          (display msg err)
          (for-each (lambda (x) (display " " err) (write x err)) args)
          (newline err))))))
  (include "config.scm"))
