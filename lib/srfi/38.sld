
(define-library (srfi 38)
  (import (chibi) (srfi 69) (chibi ast))
  (export write-with-shared-structure write/ss
          read-with-shared-structure read/ss)
  (include "38.scm")
  (cond-expand
   (uvector
    )
   (else
    (begin
      (define (list->uvector etype ls)
        (if (eq? etype U8)
            (let* ((len (length ls))
                   (bv (make-bytevector len)))
              (do ((i 0 (+ i 1)) (ls ls (cdr ls)))
                  ((null? ls) bv)
                (bytevector-u8-set! bv i (car ls))))
            (list->vector ls)))))))
