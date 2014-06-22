
(define-library (chibi bytevector)
  (export
   bytevector-u16-ref-le bytevector-u16-ref-be
   bytevector-u32-ref-le bytevector-u32-ref-be
   bytevector-pad-left
   integer->bytevector bytevector->integer
   integer->hex-string hex-string->integer
   bytevector->hex-string hex-string->bytevector)
  (import (scheme base) (srfi 33))
  (include "bytevector.scm"))
