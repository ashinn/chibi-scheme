
;;> Copies the file \var{from} to \var{to}.

(define (copy-file from to)
  (let ((in (open-binary-input-file from))
        (out (open-binary-output-file to)))
    (let lp ()
      (let ((n (read-u8 in)))
        (cond ((eof-object? n) (close-input-port in) (close-output-port out))
              (else (write-u8 n out) (lp)))))))
