
(define-library (chibi snow package)
  (export package? library? program?
          package-name package-email package-url package-version
          package-libraries package-programs package-data-files
          package-provides? package-dependencies package-test-dependencies
          package-installed-files package-author package-maintainer
          package-digest-mismatches package-signature-mismatches
          package-digest-ok? package-signature-ok?
          package->path package-name->meta-file
          package-file-meta package-file? package-file-top-directory
          package-file-unzipped
          get-package-meta-file get-library-meta-file
          library-name->path library->path get-library-file find-library-file
          library-url library-name parse-library-name library-name->path
          library-analyze library-include-files library-dependencies
          library-shared-include-files library-foreign-dependencies
          library-rewrite-includes library-file-name
          get-program-file program-name program-install-name
          invalid-package-reason valid-package?
          invalid-library-reason valid-library?
          invalid-program-reason valid-program?
          repo-find-publisher lookup-digest rsa-identity=?
          extract-rsa-private-key extract-rsa-public-key)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme read)
          (scheme write)
          (srfi 1)
          (srfi 115)
          (chibi snow interface)
          (chibi bytevector)
          (chibi config)
          (chibi crypto md5)
          (chibi crypto rsa)
          (chibi crypto sha2)
          (chibi pathname)
          (chibi string)
          (chibi tar)
          (chibi uri)
          (chibi zlib))
  (include "package.scm"))
