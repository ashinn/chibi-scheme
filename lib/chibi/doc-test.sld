(define-library (chibi doc-test)
  (export run-tests)
  (import (scheme base) (chibi doc) (chibi test))
  (begin
    (define (run-tests)
      (test-begin "doc")
      (test '(spec (args config))
          (get-optionals-signature
           '(spec . o)
           '(let ((args (or (and (pair? o) (car o)) (command-line)))
                  (config (and (pair? o) (pair? (cdr o)) (cadr o))))
              (foo))))
      (test-end))))
