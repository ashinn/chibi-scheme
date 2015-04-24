
;; Temporary hack, need to rewrite in pure Scheme.

;;> Gzip compress a file in place, renaming with a .gz suffix.

(define (gzip-file path)
  (system "gzip" path))

;;> Gunzip decompress a file in place, removing any .gz suffix.

(define (gunzip-file path)
  (system "gzip" "-d" path))

;; Utility to filter a bytevector to a process and return the
;; accumulated output as a new bytevector.
;; (define (process-pipe-bytevector cmd bvec)
;;   (call-with-process-io
;;    cmd
;;    (lambda (pid proc-in proc-out proc-err)
;;      (let ((len (bytevector-length bvec))
;;            (out (open-output-bytevector)))
;;        (let lp ((i 0))
;;          (cond
;;           ((u8-ready? proc-out)
;;            (let ((u8 (read-u8 proc-out)))
;;              (cond
;;               ((eof-object? u8)
;;                (get-output-bytevector out))
;;               (else
;;                (write-u8 u8 out)
;;                (lp i)))))
;;           ((< i len)
;;            (write-u8 (bytevector-u8-ref bvec i) proc-in)
;;            (lp (+ i 1)))
;;           (else
;;            ;; Once we've completed sending the input we busy wait
;;            ;; until all output has been read.  We can't just waitpid
;;            ;; here because the remaining output may still overflow the
;;            ;; pipe buffer.
;;            (close-output-port proc-in)
;;            (let lp ()
;;              (let ((u8 (read-u8 proc-out)))
;;                (cond
;;                 ((eof-object? u8)
;;                  (get-output-bytevector out))
;;                 (else
;;                  (write-u8 u8 out)
;;                  (lp))))))))))))

;; Use a temp file to avoid dead-lock issues with pipes.
(define (process-run-bytevector cmd bvec)
  (call-with-temp-file "bvec"
    (lambda (path out preserve)
      (write-bytevector bvec out)
      (close-output-port out)
      (process->bytevector (append cmd (list path))))))

;;> Gzip compress a string or bytevector in memory.

(define (gzip x)
  (if (string? x)
      (gzip (string->utf8 x))
      (process-run-bytevector '("gzip" "-c") x)))

;;> Gunzip decompress a bytevector in memory.

(define (gunzip bvec)
  (process-run-bytevector '("gzip" "-c" "-d") bvec))

;;> Gunzip decompress a bytevector in memory if it has been
;;> compressed, or return as-is otherwise.

(define (maybe-gunzip bvec)
  (if (and (>= (bytevector-length bvec) 10)
           (eqv? #x1f (bytevector-u8-ref bvec 0))
           (eqv? #x8b (bytevector-u8-ref bvec 1)))
      (gunzip bvec)
      bvec))
