;; net.scm -- the high-level network interface
;; Copyright (c) 2010 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (open-net-io host service)
  (let lp ((addr (get-address-info host
                                   (if (integer? service)
                                       (number->string service)
                                       service)
                                   #f)))
    (if (not addr)
        (error "couldn't find address" host service)
        (let ((sock (socket (address-info-family addr)
                            (address-info-socket-type addr)
                            (address-info-protocol addr))))
          (if (negative? sock)
              (lp (address-info-next addr))
              (cond
               ((negative?
                 (connect sock
                          (address-info-address addr)
                          (address-info-address-length addr)))
                (lp (address-info-next addr)))
               (else
                (cond-expand
                 (threads (set-file-descriptor-flags! sock open/non-block))
                 (else #f))
                (list (open-input-file-descriptor sock)
                      (open-output-file-descriptor sock)))))))))

(define (with-net-io host service proc)
  (let ((io (open-net-io host service)))
    (if (not (pair? io))
        (error "couldn't find address" host service)
        (let ((res (proc (car io) (car (cdr io)))))
          (close-input-port (car io))
          res))))
