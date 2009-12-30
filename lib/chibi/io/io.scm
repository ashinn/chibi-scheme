
(define (write-line str . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (display str out)
    (newline out)))

