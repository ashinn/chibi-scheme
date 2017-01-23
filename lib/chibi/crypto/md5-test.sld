(define-library (chibi crypto md5-test)
  (export run-tests)
  (import (scheme base) (chibi crypto md5) (chibi test))
  (begin
    (define (run-tests)
      (test-begin "md5")
      (test "d41d8cd98f00b204e9800998ecf8427e"
          (md5 ""))
      (test "900150983cd24fb0d6963f7d28e17f72"
          (md5 "abc"))
      (test "9e107d9d372bb6826bd81d3542a419d6"
          (md5 "The quick brown fox jumps over the lazy dog"))
      (test-end))))
