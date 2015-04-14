
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
  (let ((input (open-input-bytevector bvec))
        (output (open-output-bytevector)))
    (process-pipe cmd input output #f)
    (get-output-bytevector output)))

;;> Gzip compress a string or bytevector in memory.

(define (gzip x)
  (if (string? x)
      (gzip (string->utf8 x))
      (process-pipe-bytevector '("gzip" "-c") x)))

;;> Gunzip decompress a bytevector in memory.

(define (gunzip bvec)
  (process-pipe-bytevector '("gzip" "-c" "-d") bvec))

;;> Gunzip decompress a bytevector in memory if it has been
;;> compressed, or return as-is otherwise.

(define (maybe-gunzip bvec)
  (if (and (>= (bytevector-length bvec) 10)
           (eqv? #x1f (bytevector-u8-ref bvec 0))
           (eqv? #x8b (bytevector-u8-ref bvec 1)))
      (gunzip bvec)
      bvec))
