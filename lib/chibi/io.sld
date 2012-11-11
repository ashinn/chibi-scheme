
(define-library (chibi io)
  (export read-string read-string! read-line write-line
          port-fold port-fold-right port-map
          port->list port->string-list port->sexp-list port->string
          file-position set-file-position! seek/set seek/cur seek/end
          make-custom-input-port make-custom-output-port
          make-null-output-port make-null-input-port
          make-broadcast-port make-concatenated-port
          make-generated-input-port make-filtered-output-port
          make-filtered-input-port string-count
          open-input-bytevector open-output-bytevector get-output-bytevector
          string->utf8 utf8->string
          write-string write-u8 read-u8 peek-u8)
  (import (chibi) (chibi ast))
  (include-shared "io/io")
  (include "io/io.scm"))
