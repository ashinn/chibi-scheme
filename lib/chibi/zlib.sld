
(define-library (chibi zlib)
  (export gzip-file gunzip-file gzip gunzip maybe-gunzip)
  (import (scheme base) (scheme write)
          (chibi io) (chibi process) (chibi temp-file))
  (include "zlib.scm"))
