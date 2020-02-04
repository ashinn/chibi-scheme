
(define-library (srfi 159)
  (import (srfi 159 base) (srfi 159 columnar)
          (srfi 159 unicode) (srfi 159 color))
  (export
   ;; base
   show fn forked with with! each each-in-list call-with-output
   displayed written written-simply pretty pretty-simply
   numeric numeric/comma numeric/si numeric/fitted
   nothing nl fl space-to tab-to escaped maybe-escaped
   padded padded/right padded/both
   trimmed trimmed/right trimmed/both trimmed/lazy
   fitted fitted/right fitted/both
   joined joined/prefix joined/suffix joined/last joined/dot joined/range
   ;; columnar
   call-with-output-generator call-with-output-generators
   string->line-generator
   tabular columnar show-columns wrapped wrapped/list wrapped/char
   justified line-numbers from-file
   ;; unicode
   as-unicode unicode-terminal-width
   ;; color
   as-red as-blue as-green as-cyan as-yellow
   as-magenta as-white as-black
   as-bold as-underline
   ))
