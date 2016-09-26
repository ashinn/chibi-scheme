
(define-library (chibi memoize)
  (import (chibi optional) (chibi pathname) (chibi string)
          (srfi 9) (srfi 38) (srfi 69) (srfi 98))
  (cond-expand
   (chibi
    (import (chibi) (chibi ast) (chibi system) (chibi filesystem))
    (begin
      (define (i-am-root?)
        (zero? (current-user-id)))))
   (else
    (import (scheme base) (scheme char) (scheme file))
    (begin
      (define (i-am-root?)
        (equal? "root" (get-environment-variable "USER")))
      (define (procedure-name x) #f)
      (define (procedure-arity x) #f)
      (define (procedure-variadic? x) #f))))
  (export define-memoized memoize memoize-to-file memoize-file-loader
          make-lru-cache lru-cache? lru-ref lru-ref! lru-set!
          hash-table-ref!)
  (include "memoize.scm"))
