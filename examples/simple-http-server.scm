#! /usr/bin/env chibi-scheme

; Simple HTTP server
; Returns a minimal HTML page with a single number incremented
; every request. Binds to localhost port 8000.

(import (chibi) (chibi net http-server) (chibi net servlet) (chibi sxml))

(let ((count 0))
   (run-http-server
    8000
    (lambda (cfg request next restart)
      (set! count (+ 1 count))
      (servlet-write request (sxml->xml `(html (body 
        (p "Count: \n")
        (p ,count))))))))
