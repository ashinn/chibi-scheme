
(define-library (chibi binary-record)
  (import (scheme base)
          (srfi 1)
          (chibi string))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (export define-binary-record-type
          u8 u16/le u16/be padded-string fixed-string
          octal decimal hexadecimal)
  (include "binary-record.scm"))
