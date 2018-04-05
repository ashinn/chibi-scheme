
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
               (if (integer? (car o))
                   (inexact->exact (car o))
                   (if (eq? #t (car o)) 0 1))
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

;;> Replaces the current process with a new image running the program
;;> \var{cmd}, with arguments in the list \var{args}.  The first
;;> argument, by convention, should be the file name being executed -
;;> an error is signaled if \var{args} is null.  The command and
;;> arguments may be symbols or numbers in addition to strings for
;;> convenience.  Equivalent to \ccode{execvp}.
(define (execute cmd args)
  (define (->string x)
    (cond ((symbol? x) (symbol->string x))
          ((eqv? -i x) "-i")
          ((number? x) (number->string x))
          (else x)))
  (if (null? args)
      (error "execute requires a non-empty argument list (command-name comes first)"))
  (execvp (->string cmd) (map ->string args)))

(define (execute-returned cmd)
  ;; we only arrive here if execute fails
  (let ((err (current-error-port)))
    (cond
     ((output-port? err)
      (display "ERROR: couldn't execute: " (current-error-port))
      (write cmd (current-error-port))
      (newline (current-error-port))))
    (exit 1)))

;;> Runs the given command \var{cmd} in a subprocess, with arguments
;;> \var{args}.  Uses a flat representation of arguments to avoid
;;> duplicates, so unlike \scheme{execute} automatically includes
;;> \var{cmd} as the first argument program name.  As a convenience,
;;> \var{cmd} itself may be a list which is appended to any arguments.
;;>
;;> The \ccode{stdin}, \ccode{stdout} and \ccode{stderr} will be
;;> inherited from the current process.  Use
;;> \scheme{call-with-process-io} if you need to capture or manipulate
;;> the subprocess IO.
;;>
;;> \emph{Examples:}
;;>
;;> \schemeblock{
;;> (system "date")
;;> Mon Aug 28 23:25:11 JST 2017
;;> }
;;>
;;> \schemeblock{
;;> (system "ls" "/usr/")
;;> bin  games  include  lib  local  sbin  share  src
;;> }
;;>
;;> \schemeblock{
;;> (system '(dc -e "2 2 + p"))
;;> 4
;;> }
(define (system cmd . args)
  (let ((pid (fork)))
    (cond
     ((zero? pid)
      (let ((cmd ((if (pair? cmd) append cons) cmd args)))
        (execute (car cmd) cmd)
        (execute-returned cmd)))
     (else
      (waitpid pid 0)))))

;;> Equivalent to \scheme{system}, but returns \scheme{#t} on success
;;> and \scheme{#f} on failure.
(define (system? cmd . args)
  (let ((res (apply system cmd args)))
    (and (pair? res) (zero? (cadr res)))))

;;> Runs the program \var{command} in a subprocess and calls
;;> \var{proc} on 4 arguments: the \var{pid}, \var{stdin},
;;> \var{stdout} and \var{stderr} of the subprocess.  \var{command}
;;> should be a list beginning with the program name followed by any
;;> args, which may be symbols or numbers for convenience as with
;;> \scheme{system}, or a string which is split on white-space.
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
             (execute (car command-ls) command-ls)
             (execute-returned command-ls))
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

;;> Utility to run \var{command} and return the accumulated output as
;;> a bytevector.
(define (process->bytevector command)
  (call-with-process-io
   command
   (lambda (pid in out err)
     (close-output-port in)
     (let ((res (port->bytevector out)))
       (waitpid pid 0)
       res))))

;;> Utility to run \var{command} and return the accumulated output as
;;> a string.
(define (process->string command)
  (call-with-process-io
   command
   (lambda (pid in out err)
     (close-output-port in)
     (let ((res (port->string out)))
       (waitpid pid 0)
       res))))

;;> Utility to run \var{command} and return the accumulated output as
;;> a sexp, as from \scheme{read}.
(define (process->sexp command)
  (call-with-input-string (process->string command) read))

;;> Utility to run \var{command} and return a list of three values:
;;> the accumulated output as a string, the error output as a string,
;;> and the exit status as an integer.
(define (process->output+error+status command)
  (call-with-process-io
   command
   (lambda (pid in out err)
     (close-output-port in)
     (let* ((out (port->string out))
            (err (port->string err))
            (res (waitpid pid 0)))
       (list out err (cadr res))))))

;;> Utility to run \var{command} and return a list of two values:
;;> the accumulated output as a string, the error output as a string.
(define (process->output+error command)
  (let ((res (process->output+error+status command)))
    (list (car res) (cadr res))))

;;> Utility to run \var{command} and return the output as a list of
;;> strings, one for each line (trailing newlines not included).
(define (process->string-list command)
  (call-with-process-io
   command
   (lambda (pid in out err)
     (close-output-port in)
     (let ((res (port->string-list out)))
       (waitpid pid 0)
       res))))
