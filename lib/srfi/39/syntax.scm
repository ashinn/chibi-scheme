;; param.scm -- SRFI-39 parameters
;; Copyright (c) 2010 Alex Shinn.  All rights reserved.
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
    ((parameterize ("step") old cons-new ((param value ptmp vtmp) ...) () body)
     (let ((ptmp param) ...)
       (let ((vtmp (parameter-convert ptmp value)) ...)
         (let ((old (thread-parameters)))
           (let ((new cons-new))
             (dynamic-wind
                 (lambda () (thread-parameters-set! new))
                 (lambda () . body)
                 (lambda () (thread-parameters-set! old))))))))
    ((parameterize ("step") old cons-new args ((param value) . rest) body)
     (parameterize ("step") old (cons (cons ptmp vtmp) cons-new) ((param value ptmp vtmp) . args) rest body))
    ((parameterize ((param value) ...) . body)
     (parameterize ("step") old (thread-parameters) () ((param value) ...) body))))
