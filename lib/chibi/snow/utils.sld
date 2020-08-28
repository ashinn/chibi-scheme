
(define-library (chibi snow utils)
  (export find-in-path find-sexp-in-path
          write-to-string display-to-string
          resource->bytevector uri-normalize uri-directory
          version-split version-compare version>? version>=?
          topological-sort
          known-implementations impl->version impl->features)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme lazy)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (srfi 1)
          (chibi config)
          (chibi char-set)
          (chibi net http)
          (chibi pathname)
          (chibi process)
          (chibi string)
          (chibi uri))
  (cond-expand
   (chibi (import (chibi io)))
   (chicken
    (begin
      (define (port->bytevector in) (read-bytevector #f in))
      (define (file->bytevector in)
        (call-with-input-file in port->bytevector))
      (define (call-with-output-string proc)
        (let ((out (open-output-string)))
          (proc out)
          (get-output-string out))))))
  (include "utils.scm"))
