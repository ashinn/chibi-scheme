
(define-library (scheme lazy)
  (import (chibi))
  (export delay force delay-force make-promise promise?)
  (begin
    (define (make-promise x)
      (delay x)))
  (cond-expand
   (auto-force
    )
   (else
    (begin
      (define (promise? x)
        (and (pair? x)
             (null? (cdr x))
             (pair? (car x))
             (or (eq? #t (caar x))
                 (and (eq? #f (caar x))
                      (procedure? (cdar x))))))))))
