
(define-library (chibi optimize profile)
  (export optimize-profile increment-cdr! profile-reset profile-report)
  (import (chibi) (srfi 1) (srfi 69) (srfi 95)
          (chibi ast) (chibi match) (chibi optimize))
  (include-shared "profile")
  (include "profile.scm"))
