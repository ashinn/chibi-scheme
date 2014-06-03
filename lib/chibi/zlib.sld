
(define-library (chibi zlib)
  (export gzip-file gunzip-file gzip gunzip maybe-gunzip)
  (import (scheme base) (chibi io) (chibi process))
  (include "zlib.scm"))
