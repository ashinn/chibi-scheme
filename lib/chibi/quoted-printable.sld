
(define-library (chibi quoted-printable)
  (export quoted-printable-encode quoted-printable-encode-string
          quoted-printable-encode-bytevector
          quoted-printable-encode-header
          quoted-printable-decode quoted-printable-decode-string
          quoted-printable-decode-bytevector)
  (import (scheme base))
  (cond-expand
   ((library (srfi 33)) (import (srfi 33)))
   (else (import (srfi 60))))
  (cond-expand
   (chibi (import (chibi io)))
   (else
    (begin
      (define (port->string in)
        (let ((out (open-output-string)))
          (let lp ()
            (let ((ch (read-char in)))
              (cond
               ((eof-object? ch)
                (get-output-string out))
               (else
                (write-char ch out)
                (lp))))))))))
  (include "quoted-printable.scm"))
