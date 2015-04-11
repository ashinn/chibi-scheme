
(cond-expand
 (plan9
  (define (exit . o)
    (%exit (if (pair? o)
               (if (string? (car o))
                   (car o)
                   (if (eq? #t (car o)) "" "chibi error"))
               ""))))
 (else
  (define (exit . o)
    (%exit (if (pair? o)
               (if (integer? (car o)) (car o) (if (eq? #t (car o)) 0 1))
               0)))))

(cond-expand
 (bsd
  (define (process-command-line pid)
    (let ((res (%process-command-line pid)))
      ;; TODO: get command-line arguments
      (if (string? res) (list res) res))))
 (else
  (define (process-command-line pid)
    (call-with-current-continuation
     (lambda (return)
       (with-exception-handler
        (lambda (exn) (return #f))
        (lambda ()
          (let ((file (string-append "/proc/" (number->string pid) "/cmdline")))
            (call-with-input-file file
              (lambda (in)
                (let lp ((arg '()) (res '()))
                  (let ((ch (read-char in)))
                    (if (or (eof-object? ch) (eqv? (char->integer ch) 0))
                        (let ((res (cons (list->string (reverse arg)) res))
                              (ch2 (peek-char in)))
                          (if (or (eof-object? ch2)
                                  (eqv? (char->integer ch2) 0))
                              (reverse res)
                              (lp '() res)))
                        (lp (cons ch arg) res))))))))))))))

(define (process-running? pid . o)
  (let ((cmdline (process-command-line pid)))
    (and (pair? cmdline)
         (or (null? o)
             (not (car o))
             (equal? (car o) (car cmdline))))))

(define (execute cmd args)
  (define (->string x)
    (cond ((symbol? x) (symbol->string x))
          ((number? x) (number->string x))
          (else x)))
  (execvp (->string cmd) (map ->string args)))

(define (system cmd . args)
  (let ((pid (fork)))
    (cond
     ((zero? pid)
      (let* ((res (execute cmd (cons cmd args)))
             (err (current-error-port)))
        ;; we only arrive here if execute fails
        (cond
         ((output-port? err)
          (display "ERROR: couldn't execute: " (current-error-port))
          (write cmd (current-error-port))
          (newline (current-error-port))))
        (exit 1)))
     (else
      (waitpid pid 0)))))

(define (call-with-process-io command proc)
  (define (set-non-blocking! fd)
    (cond-expand
     (threads
      (set-file-descriptor-status!
       fd
       (bitwise-ior open/non-block (get-file-descriptor-status fd))))
     (else
      #f)))
  (let ((command-ls (if (string? command) (string-split command) command))
        (in-pipe (open-pipe))
        (out-pipe (open-pipe))
        (err-pipe (open-pipe)))
    (and in-pipe out-pipe err-pipe
         (let ((pid (fork)))
           (cond
            ((not pid)
             (error "couldn't fork"))
            ((zero? pid)  ;; child
             (close-file-descriptor (cadr in-pipe))
             (close-file-descriptor (car out-pipe))
             (close-file-descriptor (car err-pipe))
             (duplicate-file-descriptor-to (car in-pipe) 0)
             (duplicate-file-descriptor-to (cadr out-pipe) 1)
             (duplicate-file-descriptor-to (cadr err-pipe) 2)
             (close-file-descriptor (car in-pipe))
             (close-file-descriptor (cadr out-pipe))
             (close-file-descriptor (cadr err-pipe))
             (execute (car command-ls) command-ls))
            (else         ;; parent
             (close-file-descriptor (car in-pipe))
             (close-file-descriptor (cadr out-pipe))
             (close-file-descriptor (cadr err-pipe))
             (set-non-blocking! (cadr in-pipe))
             (set-non-blocking! (car out-pipe))
             (set-non-blocking! (car err-pipe))
             (proc pid
                   (open-output-file-descriptor (cadr in-pipe))
                   (open-input-file-descriptor (car out-pipe))
                   (open-input-file-descriptor (car err-pipe)))))))))

;;> Executes \var{command} with its input stream fed from \var{in-port}
;;> and its output and error streams forwarded into \var{out-port} and
;;> \var{err-port}. Returns the integer exit status of \var{command}.
;;>
;;> \var{command} must be a space-separated command string or a list of
;;> strings for the command and its arguments. \var{in-port} must be an
;;> open input port, \var{out-port} and \var{err-port} must be ports open
;;> for output. Any port arugment can also be specified as \scheme{#f}
;;> which connects the respective stream with /dev/null, providing no
;;> input for the command or ignoring its output.

(define (process-pipe command in-port out-port err-port)
  (define (set-non-blocking! fd)
    (set-file-descriptor-status! fd
      (bitwise-ior open/non-block (get-file-descriptor-status fd))))
  (let ((in-port  (or in-port  (make-null-input-port)))
        (out-port (or out-port (make-null-output-port)))
        (err-port (or err-port (make-null-output-port))))
    (call-with-process-io
      command
      (lambda (pid in-pipe out-pipe err-pipe)
        (set-non-blocking! in-pipe)
        (set-non-blocking! out-pipe)
        (set-non-blocking! err-pipe)
        (let loop ((in-done #f) (out-done #f) (err-done #f))
          (if (and in-done out-done err-done)
              (cadr (waitpid pid 0))
              (loop (or in-done  (feed-pipe in-port  in-pipe  #f #t))
                    (or out-done (feed-pipe out-pipe out-port #t #f))
                    (or err-done (feed-pipe err-pipe err-port #t #f)))))))))

(define (process->string command)
  (call-with-process-io
   command
   (lambda (pid in out err)
     (close-output-port in)
     (let ((res (port->string out)))
       (waitpid pid 0)
       res))))

(define (process->sexp command)
  (call-with-input-string (process->string command) read))

(define (process->output+error+status command)
  (call-with-process-io
   command
   (lambda (pid in out err)
     (close-output-port in)
     (let* ((out (port->string out))
            (err (port->string err))
            (res (waitpid pid 0)))
       (list out err (cadr res))))))

(define (process->output+error command)
  (let ((res (process->output+error+status command)))
    (list (car res) (cadr res))))

(define (process->string-list command)
  (call-with-process-io
   command
   (lambda (pid in out err)
     (close-output-port in)
     (let ((res (port->string-list out)))
       (waitpid pid 0)
       res))))
