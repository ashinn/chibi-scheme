
(define-library (srfi 99 records)
  (import (srfi 99 records procedural)
          (srfi 99 records inspection)
          (srfi 99 records syntactic))
  (export make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator
          record? record-rtd rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?
          define-record-type))
