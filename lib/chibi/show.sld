
(define-library (chibi show)
  (export
   show fn fn-fork with update! each each-in-list call-with-output
   displayed written written-shared written-simply numeric nothing
   nl fl space-to tab-to
   padded padded/left padded/right padded/both
   trimmed trimmed/left trimmed/right trimmed/both trimmed/lazy
   fitted fitted/left fitted/right fitted/both
   joined joined/prefix joined/suffix joined/last joined/dot
   upcased downcased)
  (import (scheme base) (scheme char) (chibi show base) (scheme write))
  (include "show/show.scm"))
