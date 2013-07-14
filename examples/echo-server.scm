
;; Simple R7RS echo server, using the run-net-server utility from
;; (chibi net server).

(import (scheme base) (scheme write) (chibi net) (chibi net server))

;; Copy each input line to output.
(define (echo-handler in out sock addr)
  (let ((line (read-line in)))
    (cond
     ((not (or (eof-object? line) (equal? line "")))
      ;; log the request to stdout
      (display "read: ") (write line)
      (display " from ") (display (sockaddr-name (address-info-address addr)))
      (newline)
      ;; write and flush the response
      (display line out)
      (newline out)
      (flush-output-port out)
      (echo-handler in out sock addr)))))

;; Start the server on *:5556 dispatching clients to echo-handler.
(run-net-server 5556 echo-handler)
