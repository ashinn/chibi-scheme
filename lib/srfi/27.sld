
(define-library (srfi 27)
  (export random-integer random-real default-random-source
          make-random-source random-source?
          random-source-state-ref random-source-state-set!
          random-source-randomize! random-source-pseudo-randomize!
          random-source-make-integers random-source-make-reals)
  (import (chibi))
  (include-shared "27/rand")
  (include "27/constructors.scm"))
