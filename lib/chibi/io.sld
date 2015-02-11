
(define-library (chibi io)
  (export read-string read-string! read-line write-line
          port-fold port-fold-right port-map
          port->list port->string-list port->sexp-list
          port->string port->bytevector
          file->string file->bytevector
          file-position set-file-position! seek/set seek/cur seek/end
          make-custom-input-port make-custom-output-port
          make-custom-binary-input-port make-custom-binary-output-port
          make-null-output-port make-null-input-port
          make-broadcast-port make-concatenated-port
          make-generated-input-port make-filtered-output-port
          make-filtered-input-port string-count-chars
          open-input-bytevector open-output-bytevector get-output-bytevector
          string->utf8 utf8->string
          write-string write-u8 read-u8 peek-u8 send-file
          is-a-socket?
          call-with-input-file call-with-output-file)
  (import (chibi) (chibi ast))
  (include-shared "io/io")
  (include "io/io.scm"))
