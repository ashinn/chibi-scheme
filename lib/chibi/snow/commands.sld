
(define-library (chibi snow commands)
  (export command/package
          command/gen-key
          command/reg-key
          command/sign
          command/verify
          command/upload
          command/implementations
          command/index
          command/install
          command/remove
          command/search
          command/show
          command/status
          command/update
          command/upgrade)
  (import (scheme base)
          (scheme char)
          (scheme eval)
          (scheme file)
          (scheme lazy)
          (scheme load)
          (scheme process-context)
          (scheme time)
          (scheme read)
          (scheme write)
          (srfi 1)
          (srfi 27)
          (srfi 33)
          (srfi 95)
          (chibi snow interface)
          (chibi snow package)
          (chibi snow utils)
          (chibi ast)
          (chibi bytevector)
          (chibi config)
          (chibi crypto md5)
          (chibi crypto rsa)
          (chibi crypto sha2)
          (chibi doc)
          (chibi filesystem)
          (chibi io)
          (chibi match)
          (chibi modules)
          (chibi net http)
          (chibi process)
          (chibi pathname)
          (chibi regexp)
          (chibi show)
          (chibi show pretty)
          (chibi string)
          (chibi sxml)
          (chibi system)
          (chibi tar)
          (chibi temp-file)
          (chibi uri)
          (chibi zlib))
  (include "commands.scm"))
