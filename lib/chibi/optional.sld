
(define-library (chibi optional)
  (export let-optionals let-optionals* opt-lambda
          let-keywords let-keywords* keyword-ref keyword-ref*)
  (cond-expand
   (chibi
    (import (chibi)))
   (else
    (import (scheme base))
    (begin
      (define-syntax let-optionals*
        (syntax-rules ()
          ((let-optionals* opt-ls () . body)
           (begin . body))
          ((let-optionals* (op . args) vars . body)
           (let ((tmp (op . args)))
             (let-optionals* tmp vars . body)))
          ((let-optionals* tmp ((var default) . rest) . body)
           (let ((var (if (pair? tmp) (car tmp) default))
                 (tmp2 (if (pair? tmp) (cdr tmp) '())))
             (let-optionals* tmp2 rest . body)))
          ((let-optionals* tmp tail . body)
           (let ((tail tmp)) . body)))))))
  (include "optional.scm"))
