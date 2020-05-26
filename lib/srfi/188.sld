
(define-library (srfi 188)
  (import (rename (chibi)
                  (let-syntax splicing-let-syntax)
                  (letrec-syntax splicing-letrec-syntax)))
  (export splicing-let-syntax splicing-letrec-syntax))
