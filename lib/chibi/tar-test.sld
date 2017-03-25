(define-library (chibi tar-test)
  (export run-tests)
  (import (scheme base)
          (chibi tar)
          (chibi test))
  (begin
    ;; Utility to flatten bytevectors, strings and individual bytes
    ;; (integers) into a single bytevector for generating readable test
    ;; data.  (<byte> . <repetition>) can be used to repeat a byte.
    (define (bv . args)
      (apply bytevector-append
             (map (lambda (x)
                    (cond ((string? x) (string->utf8 x))
                          ((pair? x) (make-bytevector (cdr x) (car x)))
                          ((integer? x) (bytevector x))
                          (else x)))
                  args)))
    (define (run-tests)
      (test-begin "tar")

      (let ((b (bv "foo" '(0 . 97)
                   "000644 " 0
                   "000765 " 0
                   "000765 " 0
                   "00000000016 "
                   "12302104616 "
                   "011512" 0 " "
                   "0"
                   '(0 . 100)
                   "ustar" 0 "00"
                   "bob" '(0 . 29)
                   "bob" '(0 . 29)
                   "000000 " 0
                   "000000 " 0
                   '(0 . 155)
                   '(0 . 12)
                   )))
        (let ((x (read-tar (open-input-bytevector b))))
          (test "foo" (tar-path x))
          (test 501 (tar-uid x))
          (test "bob" (tar-owner x)))
        (let ((x (make-tar "bar" #o644 501 502 123 456 "0")))
          (test "bar" (tar-path x))
          (test "" (tar-path-prefix x))
          (tar-owner-set! x "john")
          (tar-group-set! x "smith")
          (test "john" (tar-owner x))
          (test "smith" (tar-group x))
          (test "bar" (tar-path x))
          (test "" (tar-path-prefix x))
          ;;(test-error (tar-mode-set! x "r"))
          (let ((out (open-output-bytevector)))
            (write-tar x out)
            (let ((bv2 (get-output-bytevector out)))
              (test-assert (bytevector? bv2))
              (let ((x2 (read-tar (open-input-bytevector bv2))))
                (test-assert "bar" (tar-path x2))
                (test-assert #o644 (tar-mode x2))
                (test-assert 501 (tar-uid x2))
                (test-assert 502 (tar-gid x2))
                (test-assert "john" (tar-owner x2)))))))

      (test-end))))
