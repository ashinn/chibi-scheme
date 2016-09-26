;; log.scm -- customizable logging with levels
;; Copyright (c) 2005-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-record-type Logger
  (make-logger levels level-abbrevs current-level prefix prefix-spec
               counts file port locked? zipped?)
  logger?
  (levels logger-levels logger-levels-set!)
  (level-abbrevs logger-level-abbrevs logger-level-abbrevs-set!)
  (current-level logger-current-level %logger-current-level-set!)
  (prefix logger-prefix %logger-prefix-set!)
  (prefix-spec logger-prefix-spec logger-prefix-spec-set!)
  (counts logger-counts logger-counts-set!)
  (file logger-file logger-file-set!)
  (port logger-port logger-port-set!)
  (locked? logger-locked? logger-locked?-set!)
  (zipped? logger-zipped? logger-zipped?-set!))

(define (logger-prefix-set! logger prefix)
  (%logger-prefix-set! logger (log-compile-prefix prefix))
  (logger-prefix-set! logger prefix))

(define (logger-current-level-set! logger level)
  (%logger-current-level-set! logger (log-level-index logger level)))

(define-syntax define-logger
  (syntax-rules ()
    ((define-logger logger (levels ...))
     (def-logger logger (levels ...) log-default-prefix 0 () ()))))

(define-syntax def-logger
  (syntax-rules ()
    ((def-logger logger ((#f name) . rest) prefix n (names ...) defs)
     (def-logger logger rest prefix (+ n 1) (names ... name) defs))
    ((def-logger logger ((level name) . rest) prefix n (names ...) (defs ...))
     (def-logger logger rest prefix (+ n 1)
       (names ... name)
       (defs ...
         (define-syntax level
           (syntax-rules ()
             ((level . args)
              (if (<= n (logger-current-level logger))
                  (log-show logger n . args))))))))
    ((def-logger logger ((level name . x) . rest) . y)
     (syntax-error "bad logger level: " (level name . x)))
    ((def-logger logger (level . rest) prefix n names defs)
     (def-logger logger ((level (log-normalize-name 'level)) . rest)
       prefix n names defs))
    ((def-logger logger () prefix n (names ...) (defs ...))
     (begin
       defs ...
       (define logger
         (let ((names-vec (vector names ...)))
           (make-logger
            names-vec
            (log-generate-abbrevs names-vec)
            n
            (log-compile-prefix prefix)
            prefix
            '() #f (current-error-port) #f #f)))))))

(define (log-normalize-name name)
  (let ((str (symbol->string name)))
    (if (string-prefix? "log-" str)
        (string->symbol (substring str 4))
        name)))

(define (log-level-index logger level)
  (if (integer? level)
      level
      (let ((len (vector-length (logger-levels logger))))
        (let lp ((i 0))
          (cond
           ((= i len)
            (error "unknown log level" (logger-levels logger) level))
           ((eq? level (vector-ref (logger-levels logger) i)) i)
           (else (lp (+ i 1))))))))

(define (log-level-name logger level)
  (cond
   ((symbol? level)
    level)
   ((< level (vector-length (logger-levels logger)))
    (vector-ref (logger-levels logger) level))
   (else
    (let ((len (vector-length (logger-levels logger))))
      (string->symbol
      (string-append
       (symbol->string (vector-ref (logger-levels logger) (- len 1)))
       "-" (number->string (- level len))))))))

(define (log-level-abbrev logger level)
  (cond
   ((symbol? level)
    (log-level-abbrev logger (log-level-index logger level)))
   ((< level (vector-length (logger-level-abbrevs logger)))
    (vector-ref (logger-level-abbrevs logger) level))
   (else
    (number->string level))))

(define (log-generate-abbrevs abbrevs)
  (let* ((len (vector-length abbrevs))
         (res (make-vector len)))
    (do ((i 0 (+ i 1)))
        ((= i len) res)
      (let ((name (symbol->string (vector-ref abbrevs i))))
        (vector-set! res i (string (char-upcase (string-ref name 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedural interface

(define (log-generate-output logger level args)
  (let ((prefix ((logger-prefix logger) logger level))
        (message (show #f (each-in-list args))))
    (string-append
     prefix
     (string-concatenate (string-split message #\newline)
                         (string-append "\n" prefix))
     "\n")))

(define (log-compile-prefix spec)
  (define (pad2 n)
    (if (< n 10)
        (string-append "0" (number->string n))
        (number->string n)))
  (define (log-compile-one-prefix x)
    (if (string? x)
        (lambda (lg time level) x)
        (case x
          ((year)
           (lambda (lg time level) (number->string (+ 1900 (time-year time)))))
          ((month) (lambda (lg time level) (pad2 (+ 1 (time-month time)))))
          ((day) (lambda (lg time level) (pad2 (time-day time))))
          ((hour) (lambda (lg time level) (pad2 (time-hour time))))
          ((minute) (lambda (lg time level) (pad2 (time-minute time))))
          ((second) (lambda (lg time level) (pad2 (time-second time))))
          ((level)
           (lambda (lg time level) (symbol->string (log-level-name lg level))))
          ((level-abbrev)
           (lambda (lg time level) (log-level-abbrev lg level)))
          ((pid) (lambda (lg time level) (number->string (current-process-id))))
          ((uid) (lambda (lg time level) (number->string (current-group-id))))
          ((gid) (lambda (lg time level) (number->string (current-user-id))))
          (else (error "unknown logging spec" x)))))
  (let ((procs (map log-compile-one-prefix spec)))
    (lambda (logger level)
      (let ((time (seconds->time (current-seconds))))
        (let lp ((ls procs) (res '()))
          (if (null? ls)
              (string-concatenate (reverse res))
              (lp (cdr ls) (cons ((car ls) logger time level) res))))))))

(define log-default-prefix
  '(year "-" month "-" day " " hour ":" minute ":" second " " level-abbrev " "))

(define (log-open logger . o)
  (if (pair? o)
      (logger-file-set! logger (car o)))
  (if (string? (logger-file logger))
      (logger-port-set! logger (open-output-file/append (logger-file logger)))
      (logger-port-set! logger (current-error-port))))

(define (log-close logger)
  (if (output-port? (logger-port logger))
      (close-output-port (logger-port logger))))

;; Use file-locking to let multiple processes write to the same log
;; file.  On error try to re-open the log file.  We keep the port open
;; so that even if you mv the file (e.g. when rotating logs) we keep
;; writing to it in the new location.  To force writing to a new file
;; in the original location, use cp+rm instead of mv, so that the
;; logging will error and try to re-open.
(define (log-show logger level . args)
  (cond
   ((<= level (logger-current-level logger))
    (let ((str (log-generate-output logger level args)))
      (let lp ((first? #t))
        (let ((out (logger-port logger)))
          (protect (exn
                    (else
                     (cond
                      (first?  ; try to re-open log-file once
                       (log-close logger)
                       (log-open logger)
                       (lp #f))
                      (else    ; fall back to stderr
                       (display str (current-error-port))))))
            (let ((locked? (and (logger-locked? logger)
                                (output-port? out)
                                (file-lock out lock/exclusive))))
              ;; this is redundant with POSIX O_APPEND
              ;; (set-file-position! out 0 seek/end)
              (display str out)
              (flush-output out)
              (if locked? (file-lock out lock/unlock))))))))))

(define (log-show-every-n logger level id n . args)
  (cond
   ((assq id (logger-counts logger))
    => (lambda (cell)
         (if (zero? (modulo (cdr cell) n))
             (apply log-show logger level args))))
   (else
    (logger-counts-set! logger (cons (cons id 0) (logger-counts logger)))
    (apply log-show logger level args))))

;; http://httpd.apache.org/docs/2.2/mod/core.html#loglevel

(define-logger default-logger
  (log-emergency ; the server is on fire!!!           
   log-alert     ; couldn't write to user mailbox     
   log-critical  ; couldn't run 'dig' executable      
   log-error     ; error loading user filter          
   log-warn      ; invalid smtp command; relay failed 
   log-notice    ; saved to file/relayed to address   
   log-info      ; loaded alias file                  
   log-debug))   ; spam-probability: 0.5

(define-syntax with-logged-errors
  (syntax-rules ()
    ((with-logged-errors . body)
     (protect (exn (else (log-error exn)))
       . body))))

(define-syntax with-logged-and-reraised-errors
  (syntax-rules ()
    ((with-logged-errors . body)
     (protect (exn (else (log-error exn) (raise exn)))
       . body))))
