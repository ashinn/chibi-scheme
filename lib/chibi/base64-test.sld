(define-library (chibi base64-test)
  (export run-tests)
  (import (scheme base) (chibi base64) (chibi string) (chibi test))
  (begin
    (define (run-tests)
      (test-begin "base64")

      (test "YW55IGNhcm5hbCBwbGVhc3VyZS4="
          (base64-encode-string "any carnal pleasure."))
      (test "YW55IGNhcm5hbCBwbGVhc3VyZQ=="
          (base64-encode-string "any carnal pleasure"))
      (test "YW55IGNhcm5hbCBwbGVhc3Vy"
          (base64-encode-string "any carnal pleasur"))
      (test "YW55IGNhcm5hbCBwbGVhc3U="
          (base64-encode-string "any carnal pleasu"))
      (test "YW55IGNhcm5hbCBwbGVhcw=="
          (base64-encode-string "any carnal pleas"))

      (test "any carnal pleas"
          (base64-decode-string "YW55IGNhcm5hbCBwbGVhcw=="))
      (test "any carnal pleasu"
          (base64-decode-string "YW55IGNhcm5hbCBwbGVhc3U="))
      (test "any carnal pleasur"
          (base64-decode-string "YW55IGNhcm5hbCBwbGVhc3Vy"))
      (test "any carnal pleas"
          (base64-decode-string "YW55IGNhcm5hbCBwbGVhcw"))
      (test "any carnal pleasu"
          (base64-decode-string "YW55IGNhcm5hbCBwbGVhc3U"))

      (test "YW55IGNhcm5hbCBwbGVhc3VyZS4="
          (call-with-output-string
            (lambda (out)
              (call-with-input-string "any carnal pleasure."
                (lambda (in) (base64-encode in out))))))

      (test "any carnal pleasure."
          (call-with-output-string
            (lambda (out)
              (call-with-input-string "YW55IGNhcm5hbCBwbGVhc3VyZS4="
                (lambda (in) (base64-decode in out))))))

      (test-end))))
