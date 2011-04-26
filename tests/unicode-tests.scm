;; These tests are only valid if chibi-scheme is compiled with Unicode
;; support (SEXP_USE_UTF8_STRINGS).

(cond-expand
 (modules (import (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "unicode")

(test #\Р (string-ref "Русский" 0))
(test #\и (string-ref "Русский" 5))
(test #\й (string-ref "Русский" 6))

(test 7 (string-length "Русский"))

(test #\日 (string-ref "日本語" 0))
(test #\本 (string-ref "日本語" 1))
(test #\語 (string-ref "日本語" 2))

(test 3 (string-length "日本語"))

(test '(#\日 #\本 #\語) (string->list "日本語"))
(test "日本語" (list->string '(#\日 #\本 #\語)))

(test "日本" (substring "日本語" 0 2))
(test "本語" (substring "日本語" 1 3))

(test "日-語"
      (let ((s (substring "日本語" 0 3)))
        (string-set! s 1 #\-)
        s))

(test "日本人"
      (let ((s (substring "日本語" 0 3)))
        (string-set! s 2 #\人)
        s))

(test "字字字" (make-string 3 #\字))

(test "字字字"
      (let ((s (make-string 3)))
        (string-fill! s #\字)
        s))

(cond-expand (modules (import (chibi loop))) (else #f))

(test "in-string"
 '(#\日 #\本 #\語)
 (loop ((for c (in-string "日本語")) (for res (listing c))) => res))

(test "in-string-reverse"
 '(#\語 #\本 #\日)
 (loop ((for c (in-string-reverse "日本語")) (for res (listing c))) => res))

(test-end)
