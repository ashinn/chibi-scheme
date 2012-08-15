
(cond-expand
 (modules (import (only (chibi test) test-begin test test-end)
                  (chibi string)))
 (else #f))

(test-begin "strings")

(test #t (string-null? ""))
(test #f (string-null? " "))

(test #t (string-every char-alphabetic? "abc"))
(test #f (string-every char-alphabetic? "abc0"))
(test #f (string-every char-alphabetic? " abc"))
(test #f (string-every char-alphabetic? "a.c"))

(define (digit-value ch)
  (case ch
    ((#\0) 0) ((#\1) 1) ((#\2) 2) ((#\3) 3) ((#\4) 4)
    ((#\5) 5) ((#\6) 6) ((#\7) 7) ((#\8) 8) ((#\9) 9) (else #f)))

(test 3 (string-any digit-value "a3c"))
(test #f (string-any digit-value "abc"))

(test 0 (string-find "abc" char-alphabetic?))
(test 3 (string-find "abc0" char-numeric?))
(test 3 (string-find "abc" char-numeric?))

(test 3 (string-find-right "abc" char-alphabetic?))
(test 4 (string-find-right "abc0" char-numeric?))
(test 0 (string-find-right "abc" char-numeric?))

(test 0 (string-skip "abc" char-numeric?))
(test 3 (string-skip "abc0" char-alphabetic?))
(test 3 (string-skip "abc" char-alphabetic?))

(test 3 (string-skip-right "abc" char-numeric?))
(test 4 (string-skip-right "abc0" char-alphabetic?))
(test 0 (string-skip-right "abc" char-alphabetic?))

(test "foobarbaz" (string-join '("foo" "bar" "baz")))
(test "foo bar baz" (string-join '("foo" "bar" "baz") " "))

(test '("") (string-split ""))
(test '("foo" "bar" "baz") (string-split "foo bar baz"))
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

(test-end)
