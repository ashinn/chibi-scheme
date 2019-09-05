
(define-library (chibi stty)
  (export stty with-stty with-raw-io
          get-terminal-width get-terminal-dimensions
          TCSANOW TCSADRAIN TCSAFLUSH
          winsize winsize? make-winsize winsize-row winsize-col
          termios term-attrs? make-term-attrs
          ;;term-attrs-iflag term-attrs-iflag-set!
          ;;term-attrs-oflag term-attrs-oflag-set!
          ;;term-attrs-cflag term-attrs-cflag-set!
          ;;term-attrs-lflag term-attrs-lflag-set!
          )
  (import (chibi) (srfi 69) (srfi 151))
  (include-shared "stty")
  (include "stty.scm"))
