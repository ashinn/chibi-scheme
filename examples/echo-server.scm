
;; Simple R7RS echo server, using (srfi 18) threads and the
;; run-net-server utility from (chibi net server).

(import (scheme base) (scheme write) (srfi 18) (chibi net server))

;; Copy each input line to output.
(define (echo-handler in out sock addr)
  (let ((line (read-line in)))
    (cond
     ((not (or (eof-object? line) (equal? line "")))
      (display "read: ") (write line) (newline)
      (display line out)
      (newline out)
      (flush-output-port out)
      (thread-yield!)
      (echo-handler in out sock addr)))))

;; Start the server on localhost:5556 dispatching clients to echo-handler.
(run-net-server 5556 echo-handler)
