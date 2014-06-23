
;;> Copies the file \var{from} to \var{to}.

(define (copy-file from to)
  (let ((in (open-binary-input-file from))
        (out (open-binary-output-file to)))
    (let lp ()
      (let ((n (read-u8 in)))
        (cond ((eof-object? n) (close-input-port in) (close-output-port out))
              (else (write-u8 n out) (lp)))))))

(define (call-with-temp-file template proc)
  (let ((base (string-append
               "/tmp/" (path-strip-extension template)
               "-" (number->string (current-process-id)) "-"
               (number->string (exact (round (current-second)))) "-"))
        (ext (or (path-extension template) "tmp")))
    (let lp ((i 0))
      (let ((path (string-append base (number->string i) "." ext)))
        (cond
         ((> i 100)  ;; give up after too many tries regardless
          (die 2 "Repeatedly failed to generate temp file in /tmp"))
         ((file-exists? path)
          (lp (+ i 1)))
         (else
          (let ((fd (open path
                          (bitwise-ior open/write open/create open/exclusive))))
            (if (not fd)
                (if (file-exists? path) ;; created between test and open
                    (lp (+ i 1))
                    (die 2 "Couldn't generate temp file in /tmp " path))
                (let* ((out (open-output-file-descriptor fd #o700))
                       (res (proc path out)))
                  (close-output-port out)
                  (delete-file path)
                  res)))))))))

(define (call-with-temp-dir template proc)
  (let ((base (string-append
               "/tmp/" template
               "-" (number->string (current-process-id)) "-"
               (number->string (exact (round (current-second)))) "-")))
    (let lp ((i 0))
      (let ((path (string-append base (number->string i))))
        (cond
         ((> i 100)  ;; give up after too many tries
          (die 2 "Repeatedly failed to generate temp dir in /tmp " path))
         ((file-exists? path)
          (lp (+ i 1)))
         ((create-directory path #o700)
          (let ((res (proc path)))
            (delete-file-hierarchy path)
            res)))))))
