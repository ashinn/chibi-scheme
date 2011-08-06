;; Copyright (c) 2010-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> @subsubsubsection{@scheme{(get-address-info host service [addrinfo])}}

;;> Create and return a new addrinfo structure for the given host
;;> and service.  @var{host} should be a string and @var{service} a
;;> string or integer.  The optional @var{addrinfo} defaults to
;;> a TCP/IP stream setting.

(define (get-address-info host service . o)
  (%get-address-info host
                     (if (integer? service) (number->string service) service)
                     (make-address-info address-family/inet
                                        socket-type/stream
                                        ip-proto/tcp)))

;;> Opens a client net connection to @var{host}, a string,
;;> on port @var{service}, which can be a string such as
;;> @scheme{"http"} or an integer.  Returns a list of two
;;> values on success - an input port and output port -
;;> or @scheme{#f} on failure.

(define (open-net-io host service)
  (let lp ((addr (get-address-info host service #f)))
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

;;> Convenience wrapper around @scheme{open-net-io}, opens
;;> the connection then calls @var{proc} with two arguments,
;;> the input port and the output port connected to the
;;> service, then closes the connection.  Returns the result
;;> of @var{proc}.  Raises an error if a connection can't
;;> be made.

(define (with-net-io host service proc)
  (let ((io (open-net-io host service)))
    (if (not (pair? io))
        (error "couldn't find address" host service)
        (let ((res (proc (car io) (car (cdr io)))))
          (close-input-port (car io))
          res))))
