
(define-library (scheme write)
  (import (rename (scheme) (write write-simple) (display display-simple))
          (rename (srfi 38) (write/ss write)))
  (export display write write-simple)
  (begin
    (define (display x . o)
      (apply (if (or (string? x) (char? x)) display-simple write) x o))))
