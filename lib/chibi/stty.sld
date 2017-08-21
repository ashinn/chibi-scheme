
(define-library (chibi stty)
  (export stty with-stty with-raw-io
          get-terminal-width get-terminal-dimensions
          TCSANOW TCSADRAIN TCSAFLUSH)
  (import (chibi) (srfi 69) (srfi 151))
  (include-shared "stty")
  (include "stty.scm"))
