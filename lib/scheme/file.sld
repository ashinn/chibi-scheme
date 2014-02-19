
(define-library (scheme file)
  (import (chibi) (only (chibi filesystem) delete-file file-exists?))
  (export
   call-with-input-file call-with-output-file
   delete-file file-exists?
   open-binary-input-file open-binary-output-file
   open-input-file open-output-file
   with-input-from-file with-output-to-file))
