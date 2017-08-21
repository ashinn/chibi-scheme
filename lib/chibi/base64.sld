
(define-library (chibi base64)
  (export base64-encode base64-encode-string base64-encode-bytevector
          base64-decode base64-decode-string base64-decode-bytevector
          base64-encode-header)
  (import (scheme base)
          (chibi string))
  (cond-expand
   ((library (srfi 151))
    (import (srfi 151)))
   ((library (srfi 33))
    (import (srfi 33))
    (begin
      (define (%mask size) (bitwise-not (arithmetic-shift -1 size)))
      (define (bit-field n start end)
        (bitwise-and (arithmetic-shift n (- start)) (mask (- end start))))))
   (else
    (import (srfi 60))
    (begin
      (define (%mask size) (bitwise-not (arithmetic-shift -1 size)))
      (define (bit-field n start end)
        (bitwise-and (arithmetic-shift n (- start)) (mask (- end start)))))))
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
  (include "base64.scm"))
