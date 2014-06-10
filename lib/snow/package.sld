
(define-library (snow package)
  (export package? library?
          package-name package-email package-url package-version
          package-libraries package-provides? package-dependencies
          package-installed-files
          package-digest-mismatches package-signature-mismatches
          package-digest-ok? package-signature-ok?
          package->path package-name->meta-file
          get-package-meta-file get-library-meta-file
          library-name->path library->path get-library-file
          library-url library-name parse-library-name library-name->path
          library-analyze library-include-files library-dependencies
          library-rewrite-includes
          repo-find-publisher lookup-digest rsa-identity=?
          extract-rsa-private-key extract-rsa-public-key)
  (import (chibi)
          (srfi 1)
          (snow interface)
          (chibi config)
          (chibi crypto md5)
          (chibi crypto rsa)
          (chibi crypto sha2)
          (chibi pathname)
          (chibi string)
          (chibi uri))
  (include "package.scm"))
