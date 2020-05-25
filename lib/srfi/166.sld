
(define-library (srfi 166)
  (import (srfi 166 base)
          (srfi 166 pretty)
          (srfi 166 columnar)
          (srfi 166 unicode)
          (srfi 166 color))
  (export
   ;; basic
   show displayed written written-shared written-simply escaped maybe-escaped
   numeric numeric/comma numeric/si numeric/fitted
   nl fl space-to tab-to nothing each each-in-list
   joined joined/prefix joined/suffix joined/last joined/dot
   joined/range padded padded/right padded/both
   trimmed trimmed/right trimmed/both trimmed/lazy
   fitted fitted/right fitted/both output-default
   ;; computations
   fn with with! forked call-with-output
   ;; state variables
   port row col width output writer string-width pad-char ellipsis
   radix precision decimal-sep decimal-align sign-rule
   comma-sep comma-rule word-separator?
   ;; pretty
   pretty pretty-shared pretty-simply pretty-color
   ;; columnar
   columnar tabular wrapped wrapped/list wrapped/char
   justified from-file line-numbers show-columns
   ;; unicode
   as-unicode unicode-terminal-width
   upcased downcased
   ;; color
   as-red as-blue as-green as-cyan as-yellow
   as-magenta as-white as-black
   as-bold as-underline
   as-color as-true-color
   ))
