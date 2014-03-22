
;; Temporary hack, need to rewrite in pure Scheme.

;;> Gzip compress a file in place, renaming with a .gz suffix.

(define (gzip-file path)
  (system "gzip" path))

;;> Gunzip decompress a file in place, removing any .gz suffix.

(define (gunzip-file path)
  (system "gzip" "-d" path))

;; Utility to filter a bytevector to a process and return the
;; accumulated output as a new bytevector.
(define (process-pipe-bytevector cmd bvec)
  (call-with-process-io
   cmd
   (lambda (pid proc-in proc-out proc-err)
     ;; This could overflow the pipe.
     (write-bytevector bvec proc-in)
     (close-output-port proc-in)
     (let ((res (port->bytevector proc-out)))
       (waitpid pid 0)
       res))))

;;> Gzip compress a string or bytevector in memory.

(define (gzip x)
  (if (string? x)
      (gzip (string->utf8 x))
      (process-pipe-bytevector '("gzip" "-c") x)))

;;> Gunzip decompress a bytevector in memory.

(define (gunzip bvec)
  (process-pipe-bytevector '("gzip" "-c" "-d") bvec))
