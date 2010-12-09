;; These tests are only valid if chibi-scheme is compiled with Unicode
;; support (SEXP_USE_UTF8_STRINGS).

(import (chibi test))

(test-begin "unicode")

(test #\Р (string-ref "Русский" 0))
(test #\и (string-ref "Русский" 5))
(test #\й (string-ref "Русский" 6))

(test 7 (string-length "Русский"))

(test #\二 (string-ref "二本語" 0))
(test #\本 (string-ref "二本語" 1))
(test #\語 (string-ref "二本語" 2))

(test 3 (string-length "二本語"))

(test '(#\二 #\本 #\語) (string->list "二本語"))
(test "二本語" (list->string '(#\二 #\本 #\語)))

(test "二本" (substring "二本語" 0 2))
(test "本語" (substring "二本語" 1 3))

(test "二-語"
      (let ((s (substring "二本語" 0 3)))
        (string-set! s 1 #\-)
        s))

(test "二本人"
      (let ((s (substring "二本語" 0 3)))
        (string-set! s 2 #\人)
        s))

(test "字字字" (make-string 3 #\字))

(test "字字字"
      (let ((s (make-string 3)))
        (string-fill! s #\字)
        s))

(test-end)
