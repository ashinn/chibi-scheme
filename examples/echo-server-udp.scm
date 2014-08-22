#!/usr/bin/env chibi-scheme

(import (scheme base) (chibi net))

(define (get-udp-address-info host service)
  (let ((hints (make-address-info address-family/inet
                                  socket-type/datagram
                                  ip-proto/udp)))
    (get-address-info host service hints)))

;; create and bind a udp socket
(let* ((addr (get-udp-address-info #f 5556))
       (sock (socket (address-info-family addr)
                     (address-info-socket-type addr)
                     (address-info-protocol addr))))
  (bind sock (address-info-address addr) (address-info-address-length addr))
  ;; for every packet we receive, just send it back
  (let lp ()
    (cond
     ((receive sock 512 0 addr)
      => (lambda (bv) (send sock bv 0 addr))))
    (lp)))
