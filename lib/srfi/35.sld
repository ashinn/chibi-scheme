(define-library (srfi 35)
  (import (srfi 35 internal))
  (export make-condition-type
          condition-type?
          make-condition
          condition?
          condition-has-type?
          condition-ref
          make-compound-condition
          extract-condition
          define-condition-type
          condition

          &condition

          &message
          message-condition?
          condition-message

          &serious
          serious-condition?

          &error
          error?))
