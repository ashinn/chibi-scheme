
(define-library (srfi 159 color)
  (import (scheme base) (srfi 159 base))
  (export as-red as-blue as-green as-cyan as-yellow
          as-magenta as-white as-black
          as-bold as-underline)
  (include "../166/color.scm"))
