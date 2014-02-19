
(define-library (chibi stty)
  (export stty with-stty with-raw-io
          get-terminal-width get-terminal-dimensions
          TCSANOW TCSADRAIN TCSAFLUSH)
  (import (chibi) (srfi 33) (srfi 69))
  (include-shared "stty")
  (include "stty.scm"))
