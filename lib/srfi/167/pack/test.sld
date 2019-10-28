(define-library (srfi 167 pack test)

  (export run-tests)

  (import (scheme base))
  (import (chibi test))
  (import (srfi 167 pack))

  (begin

    (define expected
      (list *null*
            #t
            #f
            0
            #vu8(42 101 255)
            "hello world"
            'symbol
            42
            (expt 2 64)
            -42
            (- (expt 2 64))))

    (define (run-tests)
      (test expected (unpack (apply pack expected))))))
