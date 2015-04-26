
(define-library (chibi crypto md5)
  (import (scheme base) (chibi bytevector))
  (cond-expand
   ((library (srfi 60)) (import (srfi 60)))
   (else (import (srfi 33))))
  (export md5)
  (include "md5.scm"))
