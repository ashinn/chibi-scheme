
(define-library (chibi net http-server)
  (export
   ;; main interface
   run-http-server
   ;; basic servlets
   http-chain-servlets http-default-servlet http-wrap-default
   http-file-servlet http-procedure-servlet http-ext-servlet
   http-regexp-servlet http-path-regexp-servlet http-uri-regexp-servlet
   http-host-regexp-servlet http-redirect-servlet http-rewrite-servlet
   http-cgi-bin-dir-servlet http-scheme-script-dir-servlet)
  (import (scheme time) (srfi 39) (srfi 95)
          (chibi) (chibi mime) (chibi regexp) (chibi pathname) (chibi uri)
          (chibi filesystem) (chibi io) (chibi string) (chibi process)
          (chibi net server) (chibi net server-util) (chibi net servlet)
          (chibi app) (chibi ast) (chibi config) (chibi log) (chibi memoize))
  (include "http-server.scm"))
