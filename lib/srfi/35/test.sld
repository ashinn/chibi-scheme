(define-library (srfi 35 test)
  (import (scheme base)
          (srfi 35 internal)
          (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "srfi-35: condition types")
      (test-group "Adapted from the SRFI 35 examples"
        (define-condition-type &c &condition
          c?
          (x c-x))

        (define-condition-type &c1 &c
          c1?
          (a c1-a))

        (define-condition-type &c2 &c
          c2?
          (b c2-b))
        (define v1 (make-condition &c1 'x "V1" 'a "a1"))
        (define v2 (condition (&c2
                               (x "V2")
                               (b "b2"))))
        (define v3 (condition (&c1
                               (x "V3/1")
                               (a "a3"))
                              (&c2
                               (b "b3"))))
        (define v4 (make-compound-condition v1 v2))
        (define v5 (make-compound-condition v2 v3))

        (test #t (c? v1))
        (test #t (c1? v1))
        (test #f (c2? v1))
        (test "V1" (c-x v1))
        (test "a1" (c1-a v1))

        (test #t (c? v2))
        (test #f (c1? v2))
        (test #t (c2? v2))
        (test "V2" (c-x v2))
        (test "b2" (c2-b v2))

        (test #t (c? v3))
        (test #t (c1? v3))
        (test #t (c2? v3))
        (test "V3/1" (c-x v3))
        (test "a3" (c1-a v3))
        (test "b3" (c2-b v3))

        (test #t (c? v4))
        (test #t (c1? v4))
        (test #t (c2? v4))
        (test "V1" (c-x v4))
        (test "a1" (c1-a v4))
        (test "b2" (c2-b v4))

        (test #t (c? v5))
        (test #t (c1? v5))
        (test #t (c2? v5))
        (test "V2" (c-x v5))
        (test "a3" (c1-a v5))
        (test "b2" (c2-b v5)))

      (test-group "Standard condition hierarchy"
        (let ((mc (make-message-condition "foo!")))
          (test #t (message-condition? mc))
          (test "foo!" (condition-message mc))

          (let ((ec (make-error)))
            (test #t (error? ec))
            (test #t (serious-condition? ec))

            (let ((cc (make-compound-condition ec mc)))
              (test #t (error? cc))
              (test #t (serious-condition? cc))
              (test #t (message-condition? cc))
              (test "foo!" (condition-message mc))))))

      (test-group "R6RS extension: shadowing field names"
        (define-condition-type/constructor &a &condition
          make-a a?
          (val a-val))
        (define-condition-type/constructor &b &a
          make-b b?
          (val b-val))

        (define c (make-b 'a 'b))

        (test 'a (a-val c))
        (test 'b (b-val c)))

      (test-end))))
