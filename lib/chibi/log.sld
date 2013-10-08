
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
  (import (chibi) (srfi 9) (chibi time) (chibi process) (chibi system)
          (chibi filesystem) (chibi string) (chibi show))
  (include "log.scm"))
