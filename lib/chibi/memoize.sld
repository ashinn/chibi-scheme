
(define-library (chibi memoize)
  (import (chibi) (chibi optional) (chibi time) (chibi io)
          (chibi pathname) (chibi filesystem) (chibi system)
          (srfi 9) (srfi 38) (srfi 69))
  (cond-expand
   (chibi
    (import (chibi ast)))
   (else
    (begin
      (define (procedure-name x) #f)
      (define (procedure-arity x) #f)
      (define (procedure-variadic? x) #f))))
  (export define-memoized memoize memoize-to-file memoize-file-loader
          make-lru-cache lru-cache? lru-ref lru-ref! lru-set!
          hash-table-ref!)
  (include "memoize.scm"))
