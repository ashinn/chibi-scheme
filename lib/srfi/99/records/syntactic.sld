
(define-library (srfi 99 records syntactic)
  (export define-record-type)
  (import (chibi) (srfi 99 records inspection))
  (include "syntactic.scm"))
