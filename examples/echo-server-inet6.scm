#!/usr/bin/env chibi-scheme

(import (scheme base) (scheme write) (chibi net) (chibi net server))

;; Copy each input line to output.
(define (echo-handler in out sock addr)
  (let ((line (read-line in)))
    (cond
     ((not (or (eof-object? line) (equal? line "")))
      ;; log the request to stdout
      (display "read: ") (write line)
      (display " from ")
      (display (sockaddr-name (address-info-address addr)))
      (display " port ") (write (sockaddr-port (address-info-address addr)))
      (newline)
      ;; write and flush the response
      (display line out)
      (newline out)
      (flush-output-port out)
      (echo-handler in out sock addr)))))

(define (get-inet6-address-info host service)
  (let ((hints (make-address-info address-family/inet6
                                  socket-type/stream
                                  ip-proto/tcp)))
    (get-address-info host service hints)))

;; Start the server on local ipv6 addresses on port 5556.
(run-net-server (get-inet6-address-info #f 5556) echo-handler)
