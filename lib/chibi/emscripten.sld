(define-library (chibi emscripten)
  (cond-expand
    (emscripten
      (import (chibi) (chibi ast))
      (export eval-script! integer-eval-script string-eval-script
              wait-on-event!)
      (include "emscripten.scm")
      (include-shared "emscripten"))))
