
(define-library (snow commands)
  (export
   command/package
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
   command/upgrade
   die
   installed-libraries)
  (import (except (chibi) equal? write display)
          (scheme base)
          (scheme eval)
          (scheme write)
          (scheme file)
          (scheme time)
          (srfi 1)
          (srfi 27)
          (srfi 33)
          (srfi 95)
          (srfi 98)
          (snow interface)
          (snow package)
          (snow utils)
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
