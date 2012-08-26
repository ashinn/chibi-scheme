;; param.scm -- SRFI-39 parameters
;; Copyright (c) 2010-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (parameter-convert param value)
  (let ((proc (parameter-converter param)))
    (if (procedure? proc)
        (proc value)
        value)))

(define (make-parameter init . o)
  (let ((conv (and (pair? o) (car o))))
    (%make-parameter (if conv (conv init) init) conv)))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ("step")
                   ((param value p old new) ...)
                   ()
                   body)
     (let ((p param) ...)
       (let ((old (p)) ...
             (new (parameter-convert p value)) ...)
         (dynamic-wind
          (lambda () (p new) ...)
          (lambda () . body)
          (lambda () (p old) ...)))))
    ((parameterize ("step")
                   args
                   ((param value) . rest)
                   body)
     (parameterize ("step")
                   ((param value p old new) . args)
                   rest
                   body))
    ((parameterize ((param value) ...) . body)
     (parameterize ("step")
                   ()
                   ((param value) ...)
                   body))))
