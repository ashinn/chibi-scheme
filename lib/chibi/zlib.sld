
(define-library (chibi zlib)
  (export gzip-file gunzip-file gzip gunzip maybe-gunzip)
  (import (scheme base)
          (chibi temp-file))
  (cond-expand
   (chibi (import (chibi process)))
   (chicken
    (import (rename (chicken) (system %system))
            (only (data-structures) intersperse)
            (only (posix) process))
    (begin
      (define (system . args)
        (%system (apply string-append (intersperse args " "))))
      (define (process->bytevector cmd)
        (call-with-values (lambda ()
                            (if (pair? cmd)
                                (process (car cmd) (cdr cmd))
                                (process cmd)))
          (lambda (in out pid)
            (read-bytevector #f in)))))))
  (include "zlib.scm"))
