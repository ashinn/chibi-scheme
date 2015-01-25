
(define-library (chibi match)
  (export match match-lambda match-lambda* match-let match-letrec match-let*)
  (cond-expand
   (chibi (import (chibi)))
   (else (import (scheme base))))
  (include "match/match.scm"))
