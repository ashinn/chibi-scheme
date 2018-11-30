
(define-library (srfi 99 records syntactic)
  (export define-record-type)
  (import (chibi) (chibi syntax-case) (srfi 99 records inspection))
  (include "syntactic.scm"))
