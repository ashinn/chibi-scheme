(define-library (chibi string-test)
  (export run-tests)
  (import (scheme base) (scheme char)
          (chibi test) (chibi string))
  (cond-expand
   (chibi
    (import (only (chibi) string-cursor->index)))
   (else
    (begin
      (define (string-cursor->index str i) i))))
  (begin
    (define (digit-value ch)
      (case ch
        ((#\0) 0) ((#\1) 1) ((#\2) 2) ((#\3) 3) ((#\4) 4)
        ((#\5) 5) ((#\6) 6) ((#\7) 7) ((#\8) 8) ((#\9) 9) (else #f)))
    (define (string-find/index str pred)
      (string-cursor->index str (string-find str pred)))
    (define (string-find-right/index str pred)
      (string-cursor->index str (string-find-right str pred)))
    (define (string-skip/index str pred)
      (string-cursor->index str (string-skip str pred)))
    (define (string-skip-right/index str pred)
      (string-cursor->index str (string-skip-right str pred)))
    (define (run-tests)
      (test-begin "strings")

      (test #t (string-null? ""))
      (test #f (string-null? " "))

      (test #t (string-every char-alphabetic? "abc"))
      (test #f (string-every char-alphabetic? "abc0"))
      (test #f (string-every char-alphabetic? " abc"))
      (test #f (string-every char-alphabetic? "a.c"))

      (test 3 (string-any digit-value "a3c"))
      (test #f (string-any digit-value "abc"))

      (test 0 (string-find/index "abc" char-alphabetic?))
      (test 3 (string-find/index "abc0" char-numeric?))
      (test 3 (string-find/index "abc" char-numeric?))

      (test 3 (string-find-right/index "abc" char-alphabetic?))
      (test 4 (string-find-right/index "abc0" char-numeric?))
      (test 0 (string-find-right/index "abc" char-numeric?))

      (test 0 (string-skip/index "abc" char-numeric?))
      (test 3 (string-skip/index "abc0" char-alphabetic?))
      (test 3 (string-skip/index "abc" char-alphabetic?))

      (test 3 (string-skip-right/index "abc" char-numeric?))
      (test 4 (string-skip-right/index "abc0" char-alphabetic?))
      (test 0 (string-skip-right/index "abc" char-alphabetic?))

      (test "foobarbaz" (string-join '("foo" "bar" "baz")))
      (test "foo bar baz" (string-join '("foo" "bar" "baz") " "))

      (test '() (string-split ""))
      (test '("" "") (string-split " "))
      (test '("foo" "bar" "baz") (string-split "foo bar baz"))
      (test '("foo" "bar" "baz" "") (string-split "foo bar baz "))
      (test '("foo" "bar" "baz") (string-split "foo:bar:baz" #\:))
      (test '("" "foo" "bar" "baz") (string-split ":foo:bar:baz" #\:))
      (test '("foo" "bar" "baz" "") (string-split "foo:bar:baz:" #\:))
      (test '("foo" "bar:baz") (string-split "foo:bar:baz" #\: 2))

      (test "abc" (string-trim-left "  abc"))
      (test "abc  " (string-trim-left "abc  "))
      (test "abc  " (string-trim-left "  abc  "))

      (test "  abc" (string-trim-right "  abc"))
      (test "abc" (string-trim-right "abc  "))
      (test "  abc" (string-trim-right "  abc  "))

      (test "abc" (string-trim "  abc"))
      (test "abc" (string-trim "abc  "))
      (test "abc" (string-trim "  abc  "))
      (test "" (string-trim ""))
      (test "" (string-trim " "))
      (test "" (string-trim "  "))

      (test #t (string-prefix? "abc" "abc"))
      (test #t (string-prefix? "abc" "abcde"))
      (test #f (string-prefix? "abcde" "abc"))

      (test #t (string-suffix? "abc" "abc"))
      (test #f (string-suffix? "abc" "abcde"))
      (test #f (string-suffix? "abcde" "abc"))
      (test #f (string-suffix? "abcde" "cde"))
      (test #t (string-suffix? "cde" "abcde"))

      (test 3 (string-count "!a0 bc /.," char-alphabetic?))

      (test "ABC" (string-map char-upcase "abc"))

      (test-end))))
