
;;> Runs a procedure on a temporary file.  \var{proc} should be a
;;> procedure of three values: \scheme{(path out preserve)}, where
;;> \scheme{path} is the path to the temporary file, \scheme{out} is
;;> an output port opened on the file, and \scheme{preserve} is a
;;> thunk to disable deleting the file.  The file name will be in a
;;> temp directory, based on \var{template} and having the same
;;> extension if present, with permissions from the optional
;;> \var{mode} which defaults to \scheme{#o700}.  Returns the result
;;> of \var{proc}, after first deleting the file if the
;;> \scheme{preserve} thunk was not called.

(define (call-with-temp-file template proc . o)
  (let* ((mode (if (pair? o) (car o) #o700))
         (pid (current-process-id))
         (base (string-append
                "/tmp/" (path-strip-extension template)
                "-" (number->string pid) "-"
                (number->string (exact (round (current-second)))) "-"))
         (ext (or (path-extension template) "tmp")))
    (let lp ((i 0))
      (let ((path (string-append base (number->string i) "." ext)))
        (cond
         ((> i 100)  ;; give up after too many tries regardless
          (error "Repeatedly failed to generate temp file in /tmp"))
         ((file-exists? path)
          (lp (+ i 1)))
         (else
          (let ((fd (open path
                          (bitwise-ior open/write open/create open/exclusive)
                          mode)))
            (if (not fd)
                (if (file-exists? path) ;; created between test and open
                    (lp (+ i 1))
                    (error "Couldn't generate temp file in /tmp " path))
                (let* ((out (open-output-file-descriptor fd))
                       (preserve? #f)
                       (res (proc path out (lambda () (set! preserve? #t)))))
                  (close-output-port out)
                  (if (and (not preserve?) (equal? pid (current-process-id)))
                      (delete-file path))
                  res)))))))))

;;> Runs a procedure on a temporary directory.  \var{proc} should be a
;;> procedure of two values: \scheme{(path preserve)}, where
;;> \scheme{path} is the path to the temporary directory and
;;> \scheme{preserve} is a thunk to disable deleting the dir.  The
;;> directory name will be in a temp directory, based on
;;> \var{template}, with permissions from the optional \var{mode}
;;> which defaults to \scheme{#o700}.  Returns the result of
;;> \var{proc}, after first deleting the file hierarchy rooted at
;;> \scheme{path} if the \scheme{preserve} thunk was not called.

(define (call-with-temp-dir template proc . o)
  (let* ((mode (if (pair? o) (car o) #o700))
         (pid (current-process-id))
         (base (string-append
                "/tmp/" template "-" (number->string pid) "-"
                (number->string (exact (round (current-second)))) "-")))
    (let lp ((i 0))
      (let ((path (string-append base (number->string i))))
        (cond
         ((> i 100)  ;; give up after too many tries
          (error "Repeatedly failed to generate temp dir in /tmp " path))
         ((file-exists? path)
          (lp (+ i 1)))
         ((create-directory path mode)
          (let* ((preserve? #f)
                 (res (proc path (lambda () (set! preserve? #t)))))
            ;; sanity check for host threading issues and broken forks
            (if (and (not preserve?) (equal? pid (current-process-id)))
                (delete-file-hierarchy path))
            res))
         (else
          (error "failed to create directory" path)))))))
