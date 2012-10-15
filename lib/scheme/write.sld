
(define-library (scheme write)
  (import (rename (chibi) (write write-simple) (display display-simple))
          (rename (srfi 38) (write/ss write-shared)))
  (export display write write-shared write-simple)
  (begin
    (define (display x . o)
      (apply (if (or (string? x) (char? x)) display-simple write) x o))
    (define (write x . o)
      (write-shared x (if (pair? o) (car o) (current-output-port)) #t))))
