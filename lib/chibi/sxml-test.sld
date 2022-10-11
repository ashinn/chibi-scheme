
(define-library (chibi sxml-test)
  (import (scheme base) (chibi sxml) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "sxml")
      (test "<html><body><div><p>hello, world</p><br></div></body></html>"
          (sxml->xml '(*TOP* (html (body (div (p "hello, world") (br)))))))
      (test-end))))
