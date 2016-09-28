
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
   log-warn log-notice log-info log-debug)
  (import (chibi time) (chibi string) (chibi show base))
  (cond-expand
   (chibi
    (import (chibi) (chibi filesystem) (chibi process) (chibi string)
            (chibi system) (srfi 9))
    (begin
      (define write-string display)
      (define (open-output-file/append path)
        (let ((fd (open path
                        (+ open/create open/write open/append open/non-block))))
          (open-output-file-descriptor fd)))))
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
  (include "log.scm"))
