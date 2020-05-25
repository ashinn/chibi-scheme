
(define-library (srfi 166 color)
  (import (scheme base) (srfi 130) (srfi 165) (srfi 166 base))
  (export as-red as-blue as-green as-cyan as-yellow
          as-magenta as-white as-black
          as-bold as-underline
          as-color as-true-color)
  (begin
    (define color
      (make-computation-environment-variable 'color #f #f)))
  (include "../../chibi/show/color.scm"))
