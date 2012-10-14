
(define-library (chibi type-inference)
  (export type-analyze-module type-analyze procedure-signature)
  (import (chibi) (srfi 1) (srfi 38) (srfi 69)
          (chibi modules) (chibi ast) (chibi match))
  (include "type-inference.scm"))

