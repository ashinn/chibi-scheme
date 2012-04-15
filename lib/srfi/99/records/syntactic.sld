
(define-library (srfi 99 records syntactic)
  (export define-record-type)
  (import (scheme) (srfi 99 records inspection))
  (include "syntactic.scm"))
