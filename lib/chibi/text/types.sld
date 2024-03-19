
(define-library (chibi text types)
  (import (scheme base))
  (export
   make-text text?
   text-source text-source-set!
   text-start text-start-set!
   text-end text-end-set!
   text-prev text-prev-set!
   text-next text-next-set!
   text-marks text-marks-set!
   text-data text-data-set!
   text-first text-last
   make-mark mark?
   mark-text mark-text-set!
   mark-offset mark-offset-set!
   mark-data mark-data-set!)
  (include "types.scm"))
