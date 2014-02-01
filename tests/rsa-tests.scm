
(import (scheme base) (scheme write) (chibi crypto rsa) (chibi test))

(test-begin "rsa")

;; Verify an explicit key.

;; p = 61, q = 53
(define priv-key (rsa-key-gen-from-primes 8 61 53))
(define pub-key (rsa-pub-key priv-key))

(test 439 (rsa-sign priv-key 42))
(test #t (rsa-verify? pub-key 42 (rsa-sign priv-key 42)))

(let ((msg 42))
  (test msg (rsa-decrypt priv-key (rsa-encrypt pub-key msg))))

(define priv-key2 (rsa-key-gen-from-primes 32 2936546443 3213384203))
(define pub-key2 (rsa-pub-key priv-key2))

(let ((msg 42))
  (test msg (rsa-decrypt priv-key2 (rsa-encrypt pub-key2 msg))))

(let ((msg #u8(42)))
  (test msg (rsa-decrypt priv-key2 (rsa-encrypt pub-key2 msg))))

(let ((msg "*"))
  (test msg (utf8->string (rsa-decrypt priv-key2 (rsa-encrypt pub-key2 msg)))))

(let ((msg "*"))
  (test #t (rsa-verify? pub-key2 msg (rsa-sign priv-key2 msg))))

(let ((msg #u8(42)))
  (test #t (rsa-verify? pub-key2 msg (rsa-sign priv-key2 msg))))

;; Key generation.

(define (test-key key)
  (test #t (rsa-key? key))
  (test #t (positive? (rsa-key-n key)))
  (test #t (positive? (rsa-key-e key)))
  (test #t (positive? (rsa-key-d key)))
  (test 5 (rsa-decrypt key (rsa-encrypt (rsa-pub-key key) 5))))

(test-key (rsa-key-gen 8))
(test-key (rsa-key-gen 16))
(test-key (rsa-key-gen 32))
(test-key (rsa-key-gen-from-primes 32 2936546443 3213384203))

;; These are expensive to test.  Times with -h1G:
;; (test-key (rsa-key-gen 128))   ; 0.04s
;; (test-key (rsa-key-gen 256))   ; 0.4s
;; (test-key (rsa-key-gen 512))   ; 4s
;; (test-key (rsa-key-gen 1024))  ; 92s

(test-end)
