
;; Adaptation of John Cowan's reference impl for chibi, using the
;; our own char-set:title-case.

(define-library (srfi 129)
  (import (scheme base) (scheme char)
          (srfi 1)
          (chibi char-set) (chibi char-set full) (chibi string))
  (export char-title-case? char-titlecase string-titlecase)
  (include "129/titlemaps.scm" "129/titlecase.scm"))
