
(import (scheme) (srfi 18) (chibi net) (chibi io) (chibi filesystem))

;; Copy each input line to output.
(define (echo-handler in out)
  (let ((line (read-line in)))
    (cond
     ((not (eof-object? line))
      (display line out)
      (newline out)
      (flush-output out)
      (echo-handler in out)))))

;; Run a handler in a separate thread on the input and output ports,
;; then cleanup.
(define (run-io-handler sock handler)
  (let ((in (open-input-file-descriptor sock))
        (out (open-output-file-descriptor sock)))
    (thread-start!
     (make-thread
      (lambda ()
        (handler in out)
        (close-input-port in)
        (close-output-port out)
        (close-file-descriptor sock))))))

;; Basic server loop - repeatedly call accept, and dispatch the new
;; socket to a handler.
(define (serve host port)
  (let* ((addrinfo (get-address-info host port))
         (sock (make-listener-socket addrinfo)))
    (do () (#f)
      (let ((fd (accept sock
                        (address-info-address addrinfo)
                        (address-info-address-length addrinfo))))
        (run-io-handler fd echo-handler)))))

(serve "localhost" 5556)
