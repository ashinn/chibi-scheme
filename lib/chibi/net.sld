
(define-library (chibi net)
  (export sockaddr? address-info? get-address-info make-address-info
          socket connect bind accept listen open-socket-pair
          sockaddr-name sockaddr-port make-sockaddr
          with-net-io open-net-io make-listener-socket
          send receive! receive
          address-info-family address-info-socket-type address-info-protocol
          address-info-address address-info-address-length address-info-next
          address-family/unix address-family/inet address-family/unspecified
          socket-type/stream socket-type/datagram socket-type/raw
          ip-proto/ip ip-proto/tcp ip-proto/udp
          ai/passive ai/canonname ai/numeric-host
          get-socket-option set-socket-option! level/socket
          socket-opt/debug socket-opt/broadcast socket-opt/reuseaddr
          socket-opt/keepalive socket-opt/oobinline socket-opt/sndbuf
          socket-opt/rcvbuf socket-opt/dontroute socket-opt/rcvlowat
          socket-opt/sndlowat
          get-peer-name
          ;; C structs
          sockaddr addrinfo)
  (import (chibi) (chibi filesystem))
  (include-shared "net")
  (include "net.scm"))
