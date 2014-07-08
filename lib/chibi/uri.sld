
(define-library (chibi uri)
  (export uri? uri->string make-uri string->uri string->path-uri uri-has-scheme?
          uri-scheme uri-user uri-host uri-port uri-path uri-query uri-fragment
          uri-with-scheme uri-with-user uri-with-host uri-with-path
          uri-with-query uri-with-fragment uri-resolve
          uri-encode uri-decode uri-query->alist uri-alist->query)
  (import (chibi) (chibi string) (chibi pathname) (srfi 9))
  (include "uri.scm"))
