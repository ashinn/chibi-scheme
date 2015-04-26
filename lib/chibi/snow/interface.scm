
;; Abstract user interface for the snow command.  This could be
;; substituted with a different implementation to provide a GUI.

(define (message . args)
  (for-each display args)
  (newline))

(define (info . args)
  (apply message args))

(define (warn msg . args)
  (let ((err (current-error-port)))
    (display "WARNING: " err)
    (display msg err)
    (if (pair? args) (display ": " err))
    (if (and (pair? args) (null? (cdr args)))
        (write (car args) err)
        (for-each (lambda (x) (display "\n  " err) (write x err)) args))
    (newline err)))

(define (die x . args)
  (let ((n (if (number? x) x 2))
        (args (if (number? x) args (cons x args)))
        (err (current-error-port)))
    (for-each (lambda (x) (display x err)) args)
    (newline err)
    (flush-output-port err)
    (exit n)))

(define input-history #f)

(define (conf-input-history-file cfg)
  (or (conf-get cfg 'input-history)
      (string-append (or (conf-get cfg 'snow-dir)
                         (string-append (get-environment-variable "HOME")
                                        "/.snow"))
                     "/input-history.scm")))

(define (restore-history cfg)
  (let ((history-file (conf-input-history-file cfg)))
    (set! input-history
          (or (guard (exn (else #f))
                (list->history (call-with-input-file history-file read)))
              (make-history)))))

(define (save-history cfg)
  (let ((history-file (conf-input-history-file cfg)))
    (guard (exn (else (warn "couldn't save history to " history-file)))
      (call-with-output-file history-file
        (lambda (out)
          (write (remove (lambda (x) (equal? x ""))
                         (history->list input-history))
                 out))))))

(define (input cfg name prompt . o)
  (let ((proc (or (and (pair? o) (car o)) (lambda (x) x)))
        (check (or (and (pair? o) (pair? (cdr o)) (cadr o))
                   (lambda (str res lp) res))))
    (let lp ((reason #f))
      (cond
       ((and (not reason) (conf-get cfg name))
        => (lambda (res) (check "" res lp)))
       (else
        (if reason
            (show #t reason fl))
        (let ((str (edit-line 'prompt: (lambda () (show #f prompt))
                              'history: input-history)))
          (history-insert! input-history str)
          (check str (proc str) lp)))))))

(define (input-hidden prompt)
  (show #t prompt)
  (flush-output-port)
  (let ((res (with-stty '(not echo) (lambda () (read-line)))))
    (show #t "\n")
    res))

(define (input-password cfg name prompt1 . o)
  (let ((prompt2 (or (and (pair? o) (car o))
                     (string-append prompt1 " (confirmation): "))))
    (let lp ()
      (let ((password (input-hidden prompt1)))
        (cond
         ((equal? password "")
          (show #t "password must be non-empty\n")
          (lp))
         (else
          (let ((conf (input-hidden prompt2)))
            (cond
             ((not (equal? password conf))
              (show #t "password didn't match\n")
              (lp))
             (else
              password)))))))))

(define (input-number cfg name prompt . o)
  (let* ((default (and (pair? o) (car o)))
         (lo (and (pair? o) (pair? (cdr o)) (cadr o)))
         (hi (and (pair? o) (pair? (cdr o)) (pair? (cddr o)) (car (cddr o))))
         (prompt
          (if default (each prompt " [default=" default "]: ") prompt))
         (proc (lambda (str)
                 (if (and default (equal? str ""))
                     default
                     (string->number str))))
         (check
          (lambda (str res fail)
            (cond
             ((not (number? res))
              (fail "not a valid number"))
             ((equal? res default)
              res)
             ((and lo (< res lo))
              (fail (each "too low, must be greater than " lo)))
             ((and hi (> res hi))
              (fail (each "too high, must be less than " hi)))
             (else
              res)))))
    (input cfg name prompt proc check)))

(define (yes-or-no? cfg . prompt)
  (define (is-true? str)
    (and (string? str) (member (string-downcase str) '("#t" "y" "yes")) #t))
  (if (conf-get cfg 'always-no?)
      #f
      (input cfg 'always-yes? (each (each-in-list prompt) " [y/n]: ")
             is-true?)))
