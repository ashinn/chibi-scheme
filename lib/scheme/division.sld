
(define-library (scheme division)
  (import (scheme base))
  (export ceiling-quotient ceiling-remainder ceiling/
          euclidean-quotient euclidean-remainder euclidean/
          round-quotient round-remainder round/
          balanced-quotient balanced-remainder balanced/)
  ;; The second definition is always valid, but the first is simpler
  ;; and faster if exact ratios are supported and handled correctly
  ;; but floor/ceil/round.
  (cond-expand
   (ratios
    (begin
      (define-syntax copy-exactness2
        (syntax-rules ()
          ((copy-exactness2 src1 src2 expr)
           expr)))))
   (else
    (begin
      (define-syntax copy-exactness2
        (syntax-rules ()
          ((copy-exactness2 src1 src2 expr)
           (let ((tmp expr))
             (if (and (exact? src1) (exact? src2))
                 (inexact->exact tmp)
                 tmp))))))))
  (include "division.scm"))
