
(define-library (chibi snow commands)
  (export command/package
          command/gen-key
          command/reg-key
          command/sign
          command/verify
          command/upload
          command/install
          command/remove
          command/search
          command/show
          command/status
          command/update
          command/upgrade)
  (import (scheme base)
          (scheme eval)
          (scheme file)
          (scheme process-context)
          (scheme time)
          (scheme write)
          (srfi 1)
          (srfi 27)
          (srfi 33)
          (srfi 95)
          (chibi snow interface)
          (chibi snow package)
          (chibi snow utils)
          (chibi bytevector)
          (chibi config)
          (chibi crypto md5)
          (chibi crypto rsa)
          (chibi crypto sha2)
          (chibi filesystem)
          (chibi io)
          (chibi match)
          (chibi net http)
          (chibi process)
          (chibi pathname)
          (chibi show)
          (chibi show pretty)
          (chibi string)
          (chibi sxml)
          (chibi system)
          (chibi tar)
          (chibi zlib))
  (include "commands.scm"))
