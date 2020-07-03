
(define-library (srfi 166 color)
  (import (scheme base) (srfi 130) (srfi 165) (srfi 166 base))
  (export
   ;; foreground
   as-red as-blue as-green as-cyan as-yellow
   as-magenta as-white as-black
   as-bold as-italic as-underline
   as-color as-true-color
   ;; background
   on-red on-blue on-green on-cyan on-yellow
   on-magenta on-white on-black
   on-color on-true-color
   )
  (begin
    (define color
      (make-computation-environment-variable 'color #f #f)))
  (include "color.scm"))
