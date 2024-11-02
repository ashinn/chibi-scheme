(define-library (srfi 35 internal)
  (import (except (scheme base)
                  define-record-type
                  ;; exclude (srfi 1 immutable) duplicate imports:
                  map cons list append reverse)
          (scheme write)
          (only (chibi)
                slot-ref
                is-a?)
          (only (chibi repl) repl-print-exception)
          (only (chibi generic) define-method)
          ;; don’t let people go messing with a compound condition
          ;; components list:
          (srfi 1 immutable)
          (srfi 99)
          (srfi 133))
  (export make-condition-type
          condition?
          condition-type?
          condition-subtype?
          make-condition
          make-compound-condition
          condition-has-type?
          condition-ref
          simple-conditions
          extract-condition
          condition-predicate
          condition-accessor
          define-condition-type/constructor
          define-condition-type
          condition

          &condition

          &message
          make-message-condition
          message-condition?
          condition-message

          &serious
          make-serious-condition
          serious-condition?

          &error
          make-error
          error?)

  (include "internal.scm"))
