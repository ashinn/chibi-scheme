
(define-library (chibi net)
  (export sockaddr? address-info? get-address-info make-address-info
          socket connect bind accept listen
          with-net-io open-net-io make-listener-socket
          address-info-family address-info-socket-type address-info-protocol
          address-info-address address-info-address-length address-info-next
          address-family/unix address-family/inet
          socket-type/stream socket-type/datagram socket-type/raw
          ip-proto/tcp ip-proto/udp)
  (import (scheme) (chibi filesystem))
  (include-shared "net")
  (include "net.scm"))
