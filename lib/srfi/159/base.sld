
(define-library (srfi 159 base)
  (import (chibi show))
  (export
   show fn fn-fork with update! each each-in-list call-with-output
   displayed written written-shared written-simply
   numeric numeric/comma numeric/si numeric/fitted
   nothing nl fl space-to tab-to escaped maybe-escaped
   padded padded/left padded/right padded/both
   trimmed trimmed/left trimmed/right trimmed/both trimmed/lazy
   fitted fitted/left fitted/right fitted/both
   joined joined/prefix joined/suffix joined/last joined/dot joined/range
   upcased downcased))
