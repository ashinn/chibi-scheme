
(define-library (chibi channel)
  (cond-expand
   (chibi (import (chibi) (srfi 9)))
   (else (import (scheme base))))
  (import (srfi 18))
  (export Channel make-channel channel? channel-empty?
          channel-send! channel-receive!)
  (include "channel.scm"))
