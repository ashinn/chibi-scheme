
(define-library (chibi channel)
  (import (chibi) (srfi 9) (srfi 18))
  (export Channel make-channel channel? channel-empty?
          channel-send! channel-receive!)
  (include "channel.scm"))
