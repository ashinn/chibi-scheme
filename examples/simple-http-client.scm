#! /usr/bin/env chibi-scheme

; Simple HTTP client 
; Retrieves the contents of the URL argument:

; Usage:
; simple-http-client.scm [URL]
;
; Example:
; simple-http-client.scm http://localhost:8000

(import (chibi) (chibi net) (chibi net http) (chibi io))

(if (> (length (command-line)) 1) 
  (let ((url (car (cdr (command-line)))))
        (if (> (string-length url) 0) 
          (begin
            (display (read-string 65536 (http-get url)))  
            (newline))))
  (let ((progname (car (command-line))))
    (display "Retrieve the contents of a URL.")
    (newline)
    (display "Usage:")
    (newline)
    (newline)
    (display progname)
    (display " [URL]")
    (newline)))








