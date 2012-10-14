
(define-library (srfi 99 records inspection)
  (export record? record-rtd rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?)
  (import (chibi) (chibi ast))
  (include "inspection.scm"))
