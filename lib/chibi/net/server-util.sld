
(define-library (chibi net server-util)
  (import (chibi) (chibi io) (chibi net) (chibi string) (chibi uri)
          (chibi process) (chibi time) (chibi pathname) (chibi filesystem)
          (srfi 33) (srfi 69))
  (export line-handler command-handler parse-command
          get-host file-mime-type call-with-temp-file)
  (include "server-util.scm"))
