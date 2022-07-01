
;;> \section{Process Combinators}
;;>
;;> Running a command in a subprocess basically amounts to fork+exec.
;;> What becomes interesting is combining together multiple commands,
;;> conditionally based on exit codes and/or connecting their inputs
;;> and outputs.  More generally a variety of parameters or resources
;;> of the subprocess may be configured before the command is executed,
;;> including:
;;>
;;> * fileno configuration
;;> * environment variables
;;> * signal masks
;;> * running user
;;> * process groups
;;> * resource limits (CPU, memory, disk I/O, network)
;;> * prioritization
;;> * namespace isolation
;;> * virtual filesystems
;;>
;;> Some of these can be specified by posix_spawn(3), but the more
;;> general features come from cgroups.
;;>
;;> We can build process combinators by abstracting this configuration
;;> from the execution.  The most basic case is a single command:
;;>
;;> \scheme{(shell-command (list <command> <args> ...))}
;;>
;;> This returns a procedure of two arguments, both thunks to run in
;;> the child process after the fork but before exec (one for input and
;;> one for output).  For example,
;;>
;;> \scheme{((shell-command '("ls")) (lambda () #t) (lambda () #t))}
;;>
;;> would run the ls command in a subprocess with no changes from the
;;> parent process, i.e. it would write to the parent process' stdout.
;;>
;;> Redirecting stdio to or from files is achieved by opening the file
;;> in the child process and calling dup() to match to the appropriate
;;> stdio fileno:
;;>
;;> \schemeblock{
;;> ((shell-command '("ls"))
;;>  (lambda () #t)
;;>  (lambda ()
;;>    (duplicate-file-descriptor-to
;;>     (open "out" (bitwise-ior open/write open/create open/truncate))
;;>     1)))}
;;>
;;> \schemeblock{
;;> ((shell-command '("grep" "define"))
;;>  (lambda ()
;;>    (duplicate-file-descriptor-to
;;>     (open "shell.scm" open/read)
;;>     0))
;;>  (lambda () #t))}
;;>
;;> This looks like a common pattern, so let's provide some utilities:
;;>
;;> \schemeblock{
;;> (define (redirect file mode fileno)
;;>   (duplicate-file-descriptor-to (open file mode) fileno))}
;;>
;;> \schemeblock{
;;> (define (in< file) (redirect file open/read 0))
;;> (define (out> file)
;;>   (redirect file (bitwise-ior open/write open/create open/truncate) 1))
;;> (define (err> file)
;;>   (redirect file (bitwise-ior open/write open/create open/truncate) 2))}
;;>
;;> so we can rewrite the examples as:
;;>
;;> \schemeblock{
;;> ((shell-command '("ls")) (lambda () #t) (lambda () (out> "out")))
;;> ((shell-command '("grep" "define"))
;;>  (lambda () (in< "shell.scm")) (lambda () #t))}
;;>
;;> Another resource we may want to change is the user, e.g. via
;;> setuid.  Since we control the order of resource changes we can do
;;> things like the following example.  Here we run as root, providing
;;> access to the secret data in /etc/shadow, but extract only the row
;;> relevant to a specific user and write to a file owned by them:
;;>
;;> \schemeblock{
;;> (let ((user "alice"))
;;>   ((shell-command (list "grep" (string-append "^" user ":")))
;;>    (lambda ()
;;>      (in< "/etc/shadow")   ; read as root
;;>      (set-current-user-id! (user-id (user-information user))))
;;>    (lambda ()
;;>      (out> "my-shadow")))) ; written as user}
;;>
;;> This is already something not possible in bash (or posix_spawn)
;;> without resorting to additional subprocesses.
;;>
;;> We can in a similar manner also modify priority with nice, the
;;> filesystem with chroot, and change the cgroup, which otherwise is
;;> generally done with a wrapper script.
;;>
;;> Things get more interesting when we want to combine multiple
;;> commands.  We can connect the output of one process as the input
;;> to another with a pipe.  The following pipes the output of echo to
;;> tr, outputting "HELLO" to stdout:
;;>
;;> \schemeblock{
;;> ((shell-pipe '(echo "hello") '(tr "a-z" "A-Z")) (lambda () #t)  (lambda () #t))
;;> }
;;>
;;> We can continue to build on these combinators, but for practical
;;> use a concise syntax is handy.  We provide the syntax
;;> \scheme{shell}, similar to SCSH's \scheme{run}, except that a
;;> single top-level pipe is implied.  The above becomes:
;;>
;;> \schemeblock{(shell (echo "hello") (tr "a-z" "A-Z"))}
;;>
;;> As a convenience, to collect the output to a string we have
;;> \scheme{shell->string};
;;>
;;> \schemeblock{(shell->string (echo "hello") (tr "a-z" "A-Z")) => "HELLO"}

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (shell-object->string x)
  (if (string? x) x (call-with-output-string (lambda (out) (display x out)))))

(define (shell-command cmd)
  (cond
   ((procedure? cmd)
    cmd)
   ((not (pair? cmd))
    (shell-command (list cmd)))
   (else
    (case (car cmd)
      ((shell) (apply shell-pipe (cdr cmd)))
      ((if) (apply shell-if (cdr cmd)))
      ((and) (apply shell-and (cdr cmd)))
      ((or) (apply shell-or (cdr cmd)))
      ((do) (apply shell-do (cdr cmd)))
      (else
       (lambda (child-in child-out)
         (let ((pid (shell-fork)))
           (cond
            ((not pid)
             (error "couldn't fork"))
            ((zero? pid)                ; child
             (child-in)
             (child-out)
             (let ((ls (map shell-object->string cmd)))
               (shell-exec (car ls) ls)
               (exit 0)))
            (else                       ; parent
             (list pid))))))))))

(define (shell-stdout-to-pipe pipe . o)
  (let ((fileno (if (pair? o) (car o) 1)))
    (close-file-descriptor (car pipe))
    (duplicate-file-descriptor-to (cdr pipe) fileno)
    (close-file-descriptor (cdr pipe))))

(define (shell-stdin-from-pipe pipe)
  (close-file-descriptor (cdr pipe))
  (duplicate-file-descriptor-to (car pipe) 0)
  (close-file-descriptor (car pipe)))

(define (shell-pipe cmd . cmds)
  (let ((cmd1 (shell-command cmd)))
    (if (null? cmds)
        cmd1
        (let ((cmd2 (apply shell-pipe cmds)))
          (lambda (child-in child-out)
            (cmd2
             (lambda ()
               (let ((pipe (shell-create-pipe)))
                 (let* ((pids
                         (cmd1
                          child-in
                          (lambda ()
                            (shell-stdout-to-pipe pipe)
                            (close-file-descriptors-in-range 3 +inf.0)))))
                   (shell-stdin-from-pipe pipe))))
             (lambda ()
               (child-out)
               (close-file-descriptors-in-range 3 +inf.0))))))))

;;;; variant starting the input process first
;; (define (shell-pipe cmd1 . cmds)
;;   (let ((cmd1 (shell-command cmd1)))
;;     (if (null? cmds)
;;         cmd1
;;         (let ((cmd2 (apply shell-pipe cmds)))
;;           (lambda (child-in child-out)
;;             (cmd1
;;              child-in
;;              (lambda ()
;;                (let ((pipe (shell-create-pipe)))
;;                  (let* ((pids
;;                          (cmd2
;;                           (lambda () (shell-stdin-from-pipe pipe))
;;                           (lambda ()
;;                             (child-out)
;;                             (close-file-descriptors-in-range 3 +inf.0)))))
;;                    (shell-stdout-to-pipe pipe)
;;                    (close-file-descriptors-in-range 3 +inf.0))))))))))

;;;; variant creating the pipe in the parent
;; (define (shell-pipe cmd1 . cmds)
;;   (let ((cmd1 (shell-command cmd1)))
;;     (if (null? cmds)
;;         cmd1
;;         (let ((cmd2 (apply shell-pipe cmds)))
;;           (lambda (child-in child-out)
;;             (let* ((pipe (shell-create-pipe))
;;                    (pid1
;;                     (cmd1 child-in
;;                           (lambda ()
;;                             (shell-stdout-to-pipe pipe)
;;                             (close-file-descriptors-in-range 3 +inf.0))))
;;                    (pid2
;;                     (cmd2 (lambda ()
;;                             (shell-stdin-from-pipe pipe))
;;                           (lambda ()
;;                             (child-out)
;;                             (close-file-descriptors-in-range 3 +inf.0)))))
;;               (close-file-descriptor (car pipe))
;;               (close-file-descriptor (cdr pipe))
;;               (append pid1 pid2)))))))

(define (shell-wait pid)
  (waitpid pid 0))

(define (shell-if test pass . o)
  (let ((fail (and (pair? o) (shell-command (car o)))))
    (lambda (child-in child-out)
      (let ((pids ((shell-command test) child-in child-out)))
        (if (every (lambda (pid) (zero? (cadr (shell-wait pid)))) pids)
            ((shell-command pass) child-in child-out)
            (if fail (fail child-in child-out) '()))))))

(define (shell-seq pred cmd . cmds)
  (lambda (child-in child-out)
    (let lp ((cmds (map shell-command (cons cmd cmds))))
      (cond
       ((null? cmds)
        '())
       ((null? (cdr cmds))
        ((car cmds) child-in child-out))
       (else
        (let ((pids ((car cmds) child-in child-out)))
          (if (pred (every (lambda (pid) (zero? (cadr (shell-wait pid)))) pids))
              (lp (cdr cmds))
              '())))))))

(define (shell-and cmd . cmds)
  (apply shell-seq values cmd cmds))

(define (shell-or cmd . cmds)
  (apply shell-seq not cmd cmds))

(define (shell-do cmd . cmds)
  (apply shell-seq (lambda (res) #t) cmd cmds))

(define (redirect file mode fileno)
  (duplicate-file-descriptor-to (open file mode) fileno))

(define (in< file) (redirect file open/read 0))
(define (out> file)
  (redirect file (bitwise-ior open/write open/create open/truncate) 1))
(define (err> file)
  (redirect file (bitwise-ior open/write open/create open/truncate) 2))

(define (shell&* cmd)
  ((shell-command cmd) (lambda () #f) (lambda () #f)))

(define (shell* cmd)
  (for-each shell-wait (shell& cmd)))

(define (shell->string* cmd)
  (let ((cmd (if (procedure? cmd) cmd (apply shell-command cmd)))
        (pipe (shell-create-pipe)))
    (let ((pids
           (cmd (lambda () #f)
                (lambda () (shell-stdout-to-pipe pipe)))))
      (close-file-descriptor (cdr pipe))
      (let ((res (port->string (open-input-file-descriptor (car pipe)))))
        (for-each shell-wait pids)
        res))))

(define-syntax shell
  (syntax-rules ()
    ((shell cmd ...)
     (shell* (shell-pipe `cmd ...)))))

(define-syntax shell&
  (syntax-rules ()
    ((shell& cmd ...)
     (shell&* (shell-pipe `cmd ...)))))

(define-syntax shell->string
  (syntax-rules ()
    ((shell->string cmd ...)
     (shell->string* (shell-pipe `cmd ...)))))

(define (close-file-descriptors-in-range lo hi)
  (cond
   ((find file-directory? '("/proc/self/fd" "/dev/df"))
    => (lambda (dir)
         (for-each
          (lambda (file)
            (cond ((string->number file)
                   => (lambda (fd)
                        (when (<= lo fd hi)
                          (close-file-descriptor fd))))))
          (directory-files dir))))))
