
(define-library (chibi log)
  (export
   ;; Logger record
   Logger logger?
   logger-levels logger-levels-set!
   logger-level-abbrevs logger-level-abbrevs-set!
   logger-current-level logger-current-level-set!
   logger-prefix logger-prefix-set!
   logger-counts logger-counts-set!
   logger-file logger-file-set!
   logger-port logger-port-set!
   logger-locked? logger-locked?-set!
   logger-zipped? logger-zipped?-set!
   ;; syntax
   define-logger with-logged-errors with-logged-and-reraised-errors
   ;; procedural interface
   log-open log-close log-show log-show-every-n log-compile-prefix
   ;; levels introspection
   log-level-index log-level-name log-level-abbrev
   ;; the default logger
   default-logger log-emergency log-alert log-critical log-error
   log-warn log-notice log-info log-debug log-trace
   with-log-level)
  (import (chibi time) (chibi string) (chibi show base))
  (cond-expand
   (chibi
    (import (chibi) (chibi filesystem) (chibi process) (chibi string)
            (chibi system) (srfi 9))
    (begin
      (define write-string display)))
   (else
    (import (scheme base) (scheme char) (scheme file) (chibi string))
    (begin
      (define-syntax protect
        (syntax-rules ()
          ((protect . x) (guard . x))))
      (define open-output-file/append open-output-file)
      (define flush-output flush-output-port)
      (define (file-lock port-or-fileno mode) 'unsupported)
      (define lock/exclusive 'unsupported)
      (define lock/unlock 'unsupported)
      (define (current-process-id) -1)
      (define (current-user-id) -1)
      (define (current-group-id) -1))))
  (cond-expand
   (debug
    (begin (define default-initial-level 'debug)))
   ((library (srfi 98))
    (import (srfi 98))
    (begin
      (define default-initial-level
        (or (cond ((get-environment-variable "SCHEME_LOG_LEVEL") =>
                   (lambda (level)
                     (or (string->number level)
                         (and (not (equal? level ""))
                              (string->symbol level)))))
                  (else #f))
            'info))))
   (else
    (begin (define default-initial-level 'info))))
  (include "log.scm"))
