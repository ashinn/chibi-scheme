;; net.scm -- the high-level network interface
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (with-net-io host service proc)
  (let lp ((addr (get-address-info host service #f)))
    (if (not addr)
        (error "couldn't find address" host service)
        (let ((sock (socket (address-info-family addr)
                            (address-info-socket-type addr)
                            (address-info-protocol addr))))
          (if (negative? sock)
              (lp (address-info-next addr))
              (if (negative?
                   (connect sock
                            (address-info-address addr)
                            (address-info-address-length addr)))
                  (lp (address-info-next addr))
                  (let ((in (open-input-file-descriptor sock))
                        (out (open-output-file-descriptor sock)))
                    (let ((res (proc in out)))
                      (close-input-port in)
                      res))))))))
