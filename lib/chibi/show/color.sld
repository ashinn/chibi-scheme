
(define-library (chibi show color)
  (import (scheme base) (chibi show base))
  (export as-red as-blue as-green as-cyan as-yellow
          as-magenta as-white as-black
          as-bold as-underline)
  (include "color.scm"))
