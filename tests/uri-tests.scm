
(import (chibi) (chibi test) (chibi uri))

(test-begin "uri")

(test-assert (uri? (make-uri 'http)))
(test 'http (uri-scheme (make-uri 'http)))
(test "r" (uri-user (make-uri 'http "r")))
(test "google.com" (uri-host (make-uri 'http "r" "google.com")))
(test 80 (uri-port (make-uri 'http "r" "google.com" 80)))
(test "/search" (uri-path (make-uri 'http "r" "google.com" 80 "/search")))
(test "q=cats"
    (uri-query (make-uri 'http "r" "google.com" 80 "/search" "q=cats")))
(test "recent"
    (uri-fragment
     (make-uri 'http "r" "google.com" 80 "/search" "q=cats" "recent")))

(let ((str "http://google.com"))
  (test-assert (uri? (string->uri str)))
  (test 'http (uri-scheme (string->uri str)))
  (test "google.com" (uri-host (string->uri str)))
  (test #f (uri-port (string->uri str)))
  (test #f (uri-path (string->uri str)))
  (test #f (uri-query (string->uri str)))
  (test #f (uri-fragment (string->uri str))))

(let ((str "http://google.com/"))
  (test-assert (uri? (string->uri str)))
  (test 'http (uri-scheme (string->uri str)))
  (test "google.com" (uri-host (string->uri str)))
  (test #f (uri-port (string->uri str)))
  (test "/" (uri-path (string->uri str)))
  (test #f (uri-query (string->uri str)))
  (test #f (uri-fragment (string->uri str))))

(let ((str "http://google.com:80/search?q=cats#recent"))
  (test-assert (uri? (string->uri str)))
  (test 'http (uri-scheme (string->uri str)))
  (test "google.com" (uri-host (string->uri str)))
  (test 80 (uri-port (string->uri str)))
  (test "/search" (uri-path (string->uri str)))
  (test "q=cats" (uri-query (string->uri str)))
  (test "recent" (uri-fragment (string->uri str))))

(test "/%73" (uri-path (string->uri "http://google.com/%73")))
(test "/s" (uri-path (string->uri "http://google.com/%73" #t)))
(test "a=1&b=2;c=3"
    (uri-query (string->uri "http://google.com/%73?a=1&b=2;c=3" #t)))
(test '(("a" . "1") ("b" . "2") ("c" . "3"))
    (uri-query (string->uri "http://google.com/%73?a=1&b=2;c=3" #t #t)))
(test '(("a" . "1") ("b" . "2+2") ("c" . "3"))
    (uri-query (string->uri "http://google.com/%73?a=1&b=2+2;c=%33" #f #t)))
(test '(("a" . "1") ("b" . "2 2") ("c" . "3"))
    (uri-query (string->uri "http://google.com/%73?a=1&b=2+2;c=%33" #t #t)))

(test-end)
