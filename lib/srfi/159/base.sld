
(define-library (srfi 159 base)
  (import (chibi show))
  (export
   show fn forked with with! each each-in-list call-with-output
   displayed written written-shared written-simply
   numeric numeric/comma numeric/si numeric/fitted
   nothing nl fl space-to tab-to escaped maybe-escaped
   padded padded/right padded/both
   trimmed trimmed/right trimmed/both trimmed/lazy
   fitted fitted/right fitted/both
   joined joined/prefix joined/suffix joined/last joined/dot joined/range))
