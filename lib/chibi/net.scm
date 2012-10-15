;; Copyright (c) 2010-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> @subsubsubsection{@scheme{(get-address-info host service [addrinfo])}}

;;> Create and return a new addrinfo structure for the given host
;;> and service.  @var{host} should be a string and @var{service} a
;;> string or integer.  The optional @var{addrinfo} defaults to
;;> a TCP/IP stream setting.

(define (get-address-info host service . o)
  (%get-address-info host
                     (if (integer? service) (number->string service) service)
                     (if (and (pair? o) (car o))
                         (car o)
                         (make-address-info address-family/inet
                                            socket-type/stream
                                            ip-proto/tcp))))

;;> Opens a client net connection to @var{host}, a string,
;;> on port @var{service}, which can be a string such as
;;> @scheme{"http"} or an integer.  Returns a list of three
;;> values on success - the socket, an input port, and an
;;> output port - or @scheme{#f} on failure.

(define (open-net-io host service)
  (let lp ((addr (get-address-info host service)))
    (if (not addr)
        (error "couldn't find address" host service)
        (let ((sock (socket (address-info-family addr)
                            (address-info-socket-type addr)
                            (address-info-protocol addr))))
          (if (not (fileno? sock))
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
                (list sock
                      (open-input-file-descriptor sock #t)
                      (open-output-file-descriptor sock #t)))))))))

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
        (let ((res (proc (cadr io) (car (cddr io)))))
          (close-input-port (cadr io))
          (close-output-port (car (cddr io)))
          (close-file-descriptor (car io))
          res))))

;;> @subsubsubsection{@scheme{(make-listener-socket addrinfo [max-conn])}}

;;> Convenience wrapper to call socket, bind and listen to return
;;> a socket suitable for accepting connections on the given
;;> @var{addrinfo}.  @var{max-conn} is the maximum number of pending
;;> connections, and defaults to 128.  Automatically specifies
;;> @scheme{socket-opt/reuseaddr}.

(define (make-listener-socket addrinfo . o)
  (let* ((max-connections (if (pair? o) (car o) 128))
         (sock (socket (address-info-family addrinfo)
                       (address-info-socket-type addrinfo)
                       (address-info-protocol addrinfo))))
    (cond
     ((not sock)
      (error "couldn't create socket for: " addrinfo))
     ((not (set-socket-option! sock level/socket socket-opt/reuseaddr 1))
      (error "couldn't set the socket to be reusable" addrinfo))
     ((not (bind sock
                 (address-info-address addrinfo)
                 (address-info-address-length addrinfo)))
      (close-file-descriptor sock)
      (error "couldn't bind socket" sock addrinfo))
     ((not (listen sock max-connections))
      (close-file-descriptor sock)
      (error "couldn't listen on socket" sock addrinfo))
     (else
      sock))))

;;> Returns the socket option of the given @var{name} for @var{socket}.
;;> @var{socket} should be a file descriptor, level the constant
;;> @scheme{level/socket}, and name one of the constants beginning with
;;> "socket-opt/".

(define (get-socket-option socket level name)
  (let ((res (getsockopt socket level name)))
    (and (pair? res) (car res))))
