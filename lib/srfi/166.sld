
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
   port row col width output writer pad-char ellipsis
   string-width substring/width substring/preserve
   radix precision decimal-sep decimal-align sign-rule
   comma-sep comma-rule word-separator? ambiguous-is-wide?
   pretty-environment
   ;; pretty
   pretty pretty-shared pretty-simply pretty-with-color
   ;; columnar
   columnar tabular wrapped wrapped/list wrapped/char
   justified from-file line-numbers
   ;; unicode
   terminal-aware
   string-terminal-width string-terminal-width/wide
   substring-terminal-width substring-terminal-width/wide
   substring-terminal-width substring-terminal-width/wide
   substring-terminal-preserve
   upcased downcased
   ;; color
   as-red as-blue as-green as-cyan as-yellow
   as-magenta as-white as-black
   as-bold as-italic as-underline
   as-color as-true-color
   on-red on-blue on-green on-cyan on-yellow
   on-magenta on-white on-black
   on-color on-true-color
   ))
