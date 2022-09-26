
;;> \section{Process Combinators}
;;>
;;> Running a command in a subprocess basically amounts to fork+exec.
;;> What becomes interesting is combining together multiple commands,
;;> conditionally based on exit codes and/or connecting their inputs
;;> and outputs.  More generally a variety of parameters or resources
;;> of the subprocess may be configured before the command is executed,
;;> including:
;;>
;;> \itemlist[
;;> \item{fileno configuration }
;;> \item{environment variables }
;;> \item{signal masks }
;;> \item{running user }
;;> \item{process groups }
;;> \item{resource limits (CPU, memory, disk I/O, network) }
;;> \item{prioritization }
;;> \item{namespace isolation }
;;> \item{virtual filesystems }
;;> ]
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
;;> We can use these combinators for more than I/O redirection.  For
;;> example, we can change the current working directory.  The
;;> semantics of many commands depends on the current working
;;> directory, so much so that some commands provide options to change
;;> the directory on startup (e.g. -C for git and make).  For commands
;;> which don't offer this convenience we can use process combinators
;;> to change directory only in the child without invoking extra
;;> processes:
;;>
;;> \schemeblock{
;;> ((shell-command '("cmake"))
;;>  (lambda () (change-directory project-dir))
;;>  (lambda () #t))}
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
;;> ((shell-pipe (shell-command '(echo "hello"))
;;>              (shell-command '(tr "a-z" "A-Z")))
;;>  (lambda () #t)
;;>  (lambda () #t))}
;;>
;;> We can continue to build on these combinators, but for practical
;;> use a concise syntax is handy.  We provide the syntax
;;> \scheme{shell}, similar to SCSH's \scheme{run}, except that a
;;> single top-level pipe is implied.  The above becomes:
;;>
;;> \schemeblock{(shell (echo "hello") (tr "a-z" "A-Z"))}
;;>
;;> A command without any arguments can be written as a single symbol
;;> without a list:
;;>
;;> \schemeblock{(shell (echo "hello") rev)} => "olleh\n"
;;>
;;> You can chain together any number of commands, implicitly joined
;;> in a pipe.  I/O redirection works by putting the redirection
;;> operator after the command it modifies:
;;>
;;> \schemeblock{(shell cat (< "input.txt") (tr "a-z" "A-Z") (> "out"))}
;;>
;;> for the following operators:
;;>
;;> \itemlist[
;;> \item{ \scheme{(< input)}: redirect stdin from the file input }
;;> \item{ \scheme{(<< obj)}: redirect stdin from the displayed output of obj }
;;> \item{ \scheme{(> output)}: redirect stdout to the file output }
;;> \item{ \scheme{(>> output)}: append stdout to the file output }
;;> \item{ \scheme{(err> output)}: redirect stderr to the file output }
;;> \item{ \scheme{(err>> output)}: append stderr to the file output }
;;>  ]
;;>
;;> Commands can also be combined logically with several operators:
;;>
;;> \itemlist[
;;> \item{ \scheme{(do cmd1 cmd2 ...)}: run the commands in sequence }
;;> \item{ \scheme{(and cmd1 cmd2 ...)}: run the commands in sequence until the first fails }
;;> \item{ \scheme{(or cmd1 cmd2 ...)}: run the commands in sequence until the first succeeds }
;;> \item{ \scheme{(>< cmd1 cmd2 ...)}: pipe the output of each command to the input of the next }
;;> \item{ \scheme{(if test pass fail)}: if test succeeds run pass, else fail }
;;>  ]
;;>
;;> Note although piping is implicit in the \scheme{shell} syntax
;;> itself, the \scheme{><} operator can be useful for nested
;;> pipelines, or to structure a pipeline in one expression so you can
;;> group all I/O modifiers for it as a whole, e.g.
;;>
;;> \schemeblock{(shell (< x) cat rev (> y))}
;;>
;;> could also be written as
;;>
;;> \schemeblock{(shell (>< cat rev) (< x) (> y))}
;;>
;;> As a convenience, to collect the output to a string we have
;;> \scheme{shell->string};
;;>
;;> \schemeblock{(shell->string (echo "hello") (tr "a-z" "A-Z")) => "HELLO"}
;;>
;;> Similarly, the following variants are provided:
;;>
;;> \scheme{shell->string-list}: returns a list of one string per line
;;> \scheme{shell->sexp}: returns the output parsed as a sexp
;;> \scheme{shell->sexp-list}: returns a list of one sexp per line

(define-auxiliary-syntax ><)
(define-auxiliary-syntax <<)
(define-auxiliary-syntax >>)

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (close-file-descriptors-in-range lo hi)
  (cond
   ((find file-directory? '("/proc/self/fd" "/dev/fd"))
    => (lambda (dir)
         (for-each
          (lambda (file)
            (cond ((string->number file)
                   => (lambda (fd)
                        (when (<= lo fd hi)
                          (close-file-descriptor fd))))))
          (directory-files dir))))))

(define (shell-object->string x)
  (if (string? x) x (call-with-output-string (lambda (out) (display x out)))))

(define (shell-command cmd)
  (cond
   ((procedure? cmd)
    cmd)
   ((not (pair? cmd))
    (shell-command (list cmd)))
   (else
    (lambda (child-in child-out)
      (let ((pid (shell-fork)))
        (cond
         ((not pid)
          (error "couldn't fork"))
         ((zero? pid)                   ; child
          (child-in)
          (child-out)
          (let ((ls (map shell-object->string cmd)))
            (shell-exec (car ls) ls)
            (exit 0)))
         (else                          ; parent
          (list pid))))))))

(define (shell-scheme-command proc)
  (lambda (child-in child-out)
    (let ((pid (shell-fork)))
      (cond
       ((not pid)
        (error "couldn't fork"))
       ((zero? pid)                     ; child
        (child-in)
        (child-out)
        (proc)
        (exit 0))
       (else                            ; parent
        (list pid))))))

(define (shell-stdout-to-pipe pipe . o)
  (let ((fileno (if (pair? o) (car o) 1)))
    (close-file-descriptor (car pipe))
    (duplicate-file-descriptor-to (cdr pipe) fileno)
    (close-file-descriptor (cdr pipe))))

(define (shell-stderr-to-pipe pipe . o)
  (let ((fileno (if (pair? o) (car o) 2)))
    (close-file-descriptor (car pipe))
    (duplicate-file-descriptor-to (cdr pipe) fileno)
    (close-file-descriptor (cdr pipe))))

(define (shell-stdin-from-pipe pipe . o)
  (let ((fileno (if (pair? o) (car o) 0)))
    (close-file-descriptor (cdr pipe))
    (duplicate-file-descriptor-to (car pipe) fileno)
    (close-file-descriptor (car pipe))))

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
(define (out>> file)
  (redirect file (bitwise-ior open/write open/create open/append) 1))
(define (err> file)
  (redirect file (bitwise-ior open/write open/create open/truncate) 2))
(define (err>> file)
  (redirect file (bitwise-ior open/write open/create open/append) 2))

(define (with-in< file cmd)
  (lambda (in out)
    (cmd (lambda () (in) (in< file)) out)))
(define (with-out> file cmd)
  (lambda (in out)
    (cmd in (lambda () (out) (out> file)))))
(define (with-out>> file cmd)
  (lambda (in out)
    (cmd in (lambda () (out) (out>> file)))))
(define (with-err> file cmd)
  (lambda (in out)
    (cmd in (lambda () (out) (err> file)))))
(define (with-err>> file cmd)
  (lambda (in out)
    (cmd in (lambda () (out) (err>> file)))))

(define (shell&* cmd)
  ((shell-command cmd) (lambda () #f) (lambda () #f)))

(define (call-with-shell-io cmd proc)
  (let ((cmd (if (procedure? cmd) cmd (apply shell-command cmd)))
        (in-pipe (shell-create-pipe))
        (out-pipe (shell-create-pipe))
        (err-pipe (shell-create-pipe)))
    (let ((pids
           (cmd (lambda ()
                  (shell-stdin-from-pipe in-pipe))
                (lambda ()
                  (shell-stdout-to-pipe out-pipe)
                  (shell-stderr-to-pipe err-pipe)))))
      (close-file-descriptor (car in-pipe))
      (close-file-descriptor (cdr out-pipe))
      (close-file-descriptor (cdr err-pipe))
      (let ((res (proc pids
                       (open-output-file-descriptor (cdr in-pipe))
                       (open-input-file-descriptor (car out-pipe))
                       (open-input-file-descriptor (car err-pipe)))))
        (for-each shell-wait pids)
        res))))

(define (shell-with-output cmd proc)
  (call-with-shell-io cmd (lambda (pids in out err) (proc out))))

(define-syntax shell-analyze
  (syntax-rules (< << > >> err> err>>)
    ;; I/O operators before any commands - accumulate in cur.
    ((shell-analyze join ((< file) . rest) () (cur ...))
     (shell-analyze join rest () (cur ... (< file))))
    ((shell-analyze join ((<< str) . rest) () (cur ...))
     (shell-analyze join rest () (cur ... (<< str))))
    ((shell-analyze join ((> file) . rest) () (cur ...))
     (shell-analyze join rest () (cur ... (> file))))
    ((shell-analyze join ((>> file) . rest) () (cur ...))
     (shell-analyze join rest () (cur ... (>> file))))
    ((shell-analyze join ((err> file) . rest) () (cur ...))
     (shell-analyze join rest () (cur ... (err> file))))
    ((shell-analyze join ((err>> file) . rest) () (cur ...))
     (shell-analyze join rest () (cur ... (err>> file))))

    ;; I/O operators after a command - append to the last command.
    ((shell-analyze join ((< file) . rest) (cmds ... (cmd ...)) x)
     (shell-analyze join rest (cmds ... (cmd ... (< file))) x))
    ((shell-analyze join ((<< str) . rest) (cmds ... cmd) x)
     (shell-analyze join rest (cmds ... ((apply (lambda () (display `str)))) cmd) x))
    ((shell-analyze join ((> file) . rest) (cmds ... (cmd ...)) x)
     (shell-analyze join rest (cmds ... (cmd ... (> file))) x))
    ((shell-analyze join ((>> file) . rest) (cmds ... (cmd ...)) x)
     (shell-analyze join rest (cmds ... (cmd ... (>> file))) x))
    ((shell-analyze join ((err> file) . rest) (cmds ... (cmd ...)) x)
     (shell-analyze join rest (cmds ... (cmd ... (err> file))) x))
    ((shell-analyze join ((err>> file) . rest) (cmds ... (cmd ...)) x)
     (shell-analyze join rest (cmds ... (cmd ... (err>> file))) x))

    ;; Anything but an I/O operator is a normal command.
    ((shell-analyze join (cmd . rest) (cmds ...) (cur ...))
     (shell-analyze join rest (cmds ... (cmd cur ...)) ()))

    ;; Join the analyzed results.
    ((shell-analyze join () ((cmd . ops) ...) x)
     (join (shell-analyze-io (shell-analyze-one cmd) ops) ...))
    ))

(define-syntax shell-analyze-one
  (syntax-rules (>< do and or if apply)
    ((shell-analyze-one (do cmds ...))
     (shell-analyze shell-do (cmds ...) () ()))
    ((shell-analyze-one (if cmds ...))
     (shell-analyze shell-if (cmds ...) () ()))
    ((shell-analyze-one (and cmds ...))
     (shell-analyze shell-and (cmds ...) () ()))
    ((shell-analyze-one (or cmds ...))
     (shell-analyze shell-or (cmds ...) () ()))
    ((shell-analyze-one (>< cmds ...))
     (shell-analyze shell-pipe (cmds ...) () ()))
    ((shell-analyze-one (apply proc))
     (shell-scheme-command proc))
    ((shell-analyze-one cmd)
     (shell-command `cmd))
    ))

(define-syntax shell-analyze-io
  (syntax-rules (< > >> err> err>>)
    ((shell-analyze-io cmd ((< file) . rest))
     (shell-analyze-io (with-in< (shell-object->string `file) cmd) rest))
    ((shell-analyze-io cmd ((> file) . rest))
     (shell-analyze-io (with-out> (shell-object->string `file) cmd) rest))
    ((shell-analyze-io cmd ((>> file) . rest))
     (shell-analyze-io (with-out>> (shell-object->string `file) cmd) rest))
    ((shell-analyze-io cmd ((err> file) . rest))
     (shell-analyze-io (with-err> (shell-object->string `file) cmd) rest))
    ((shell-analyze-io cmd ((err>> file) . rest))
     (shell-analyze-io (with-err>> (shell-object->string `file) cmd) rest))
    ((shell-analyze-io cmd ())
     cmd)))

(define-syntax shell&
  (syntax-rules ()
    ((shell& cmd ...)
     ((shell-analyze shell-pipe (cmd ...) () ())
      (lambda () #f)
      (lambda () #f)))))

;;> Returns the exit status of the last command in the pipeline.
(define-syntax shell
  (syntax-rules ()
    ((shell cmd ...)
     (map shell-wait (shell& cmd ...)))))

(define-syntax shell->string
  (syntax-rules ()
    ((shell->string cmd ...)
     (shell-with-output (shell-analyze shell-pipe (cmd ...) () ())
                        port->string))))

(define-syntax shell->string-list
  (syntax-rules ()
    ((shell->string cmd ...)
     (shell-with-output (shell-analyze shell-pipe (cmd ...) () ())
                        port->string-list))))

(define-syntax shell->sexp
  (syntax-rules ()
    ((shell->string cmd ...)
     (shell-with-output (shell-analyze shell-pipe (cmd ...) () ())
                        read))))

(define-syntax shell->sexp-list
  (syntax-rules ()
    ((shell->string cmd ...)
     (shell-with-output (shell-analyze shell-pipe (cmd ...) () ())
                        port->sexp-list))))
