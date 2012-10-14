
;;> Simple generic function interface.

(define-library (chibi generic)
  (export define-generic define-method make-generic generic-add!)
  (import (chibi))
  (include "generic.scm"))
