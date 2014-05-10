
(define-library (chibi tar)
  (import (scheme base) (scheme file) (scheme time) (srfi 1) (srfi 33)
          (chibi string) (chibi binary-record)
          (chibi pathname) (chibi filesystem) (chibi system))
  (export
   ;; basic
   tar make-tar tar? read-tar write-tar
   ;; utilities
   tar-safe? tar-files tar-fold tar-extract tar-extract-file tar-create
   ;; accessors
   tar-path tar-mode tar-uid tar-gid
   tar-owner tar-group tar-size
   tar-time tar-type tar-link-name
   tar-path-set! tar-mode-set! tar-uid-set! tar-gid-set!
   tar-owner-set! tar-group-set! tar-size-set!
   tar-time-set! tar-type-set! tar-link-name-set!
   tar-device-major tar-device-major-set!
   tar-device-minor tar-device-minor-set!
   tar-ustar tar-ustar-set!)
  (include "tar.scm"))
