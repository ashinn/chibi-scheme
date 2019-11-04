
(define-library (chibi edit-distance-test)
  (export run-tests)
  (import (scheme base) (chibi edit-distance) (chibi test))
  (begin
    (define (run-tests)
      (test-begin "(chibi edit-distance)")
      (test 0 (edit-distance "" ""))
      (test 0 (edit-distance "same" "same"))
      (test 1 (edit-distance "same" "game"))
      (test 2 (edit-distance "same" "sand"))
      (test 3 (edit-distance "kitten" "sitting"))
      (test 3 (edit-distance "Saturday" "Sunday"))
      (test-end))))
