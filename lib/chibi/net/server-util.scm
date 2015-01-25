
(define (get-host uri headers)
  (cond
   ((assq 'host headers)
    => (lambda (x)
         (let ((s (string-trim (cdr x))))
           (substring-cursor s 0 (string-find s #\:)))))
   ((uri-host uri))
   (else "localhost")))

(define (line-handler handler)
  (lambda (in out sock addr)
    (let ((line (read-line in)))
      (if (eof-object? line)
          #f
          (handler line in out sock addr)))))

(define (parse-command line)
  (let ((ls (string-split line #\space)))
    (cons (string->symbol (car ls)) (cdr ls))))

(define (command-handler handler)
  (line-handler
   (cond
    ((hash-table? handler)
     (lambda (line in out sock addr)
       (let ((ls (parse-command line)))
         (cond
          ((hash-table-ref/default handler (car ls))
           => (lambda (handler)
                (handler (car ls) (cdr ls) in out sock addr)))))))
    ((list? handler)
     (lambda (line in out sock addr)
       (let ((ls (parse-command line)))
         (cond
          ((assq (car ls) handler)
           => (lambda (cell)
                ((cdr cell) (car ls) (cdr ls) in out sock addr)))))))
    ((procedure? handler)
     (lambda (line in out sock addr)
       (let ((ls (parse-command line)))
         (handler (car ls) (cdr ls) in out sock addr))))
    (else
     (error "invalid handler" handler)))))

(define (load-mime-types ht file)
  (protect
      (exn
       (else
        (display "couldn't load mime types from " (current-error-port))
        (write file (current-error-port))
        (newline (current-error-port))
        (print-exception exn)))
    (call-with-input-file file
      (lambda (in)
        (let lp ()
          (let ((line (read-line in)))
            (cond
             ((not (eof-object? line))
              (let ((ls (string-split
                         (cond ((string-find line #\#)
                                => (lambda (i) (substring line 0 i)))
                               (else line)))))
                (if (and (pair? ls) (pair? (cdr ls)))
                    (for-each
                     (lambda (x)
                       (hash-table-set! ht (string->symbol x) (car ls)))
                     (cdr ls)))
                (lp))))))))))

(define file-mime-type
  (let ((ext-types #f))
    (lambda (file . o)
      ;; set mime types on first use
      (if (not ext-types)
          (let ((ht (make-hash-table eq?)))
            (cond
             ((any file-exists? '("/etc/mime.types"
                                  "/etc/httpd/mime.types"
                                  "/etc/apache2/mime.types"))
              => (lambda (file) (load-mime-types ht file))))
            (set! ext-types ht)))
      (let* ((ext (path-extension file))
             (mtype (or (and ext (hash-table-ref/default
                                  ext-types
                                  (string->symbol
                                   (string-downcase-ascii ext))
                                  #f))
                        "application/octet-stream")))
        ;; TODO: auto-detect charset
        (if (equal? mtype "text/html")
            (string-append mtype "; charset=UTF-8")
            mtype)))))

(define (call-with-temp-file template proc)
  (let ((base (string-append
               "/tmp/" (path-strip-extension template)
               "-" (number->string (current-process-id)) "-"
               (number->string (round (current-seconds))) "-"))
        (ext (path-extension template)))
    (let lp ((i 0))
      (let ((path (string-append base (number->string i) "." ext)))
        (cond
         ((> i 100)  ;; give up after too many tries regardless
          (error "Repeatedly failed to generate temp file in /tmp"))
         ((file-exists? path)
          (lp (+ i 1)))
         (else
          (let ((fd (open path
                          (bitwise-ior open/write open/create open/exclusive))))
            (if (not fd)
                (if (file-exists? path) ;; created between test and open
                    (lp (+ i 1))
                    (error "Couldn't generate temp file in /tmp " path))
                (let* ((out (open-output-file-descriptor fd #o700))
                       (res (proc path out)))
                  (close-output-port out)
                  (delete-file path)
                  res)))))))))
