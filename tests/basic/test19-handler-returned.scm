
;; A handler returning from a non-continuable raise is itself an error,
;; which with no outer handler aborts loading the file.

(define five 5)
(display "before")
(newline)
(with-exception-handler
 (lambda (e) 'ignored)
 (lambda () (car five)))
(display "after")
(newline)
