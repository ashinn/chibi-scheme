
;; An uncaught raise aborts loading the file, rather than returning the
;; exception as the value of the failed expression.

(define five 5)
(display "before")
(newline)
(car five)
(display "after")
(newline)
