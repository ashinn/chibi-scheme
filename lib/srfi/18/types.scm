;; types.scm -- thread types
;; Copyright (c) 2010-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-record-type Mutex
  (%make-mutex name specific thread lock)
  mutex?
  (name mutex-name)
  (specific mutex-specific mutex-specific-set!)
  (thread %mutex-thread %mutex-thread-set!)
  (lock %mutex-lock %mutex-lock-set!))

(define (make-mutex . o)
  (%make-mutex (and (pair? o) (car o)) #f #f #f))

(define-record-type Condition-Variable
  (%make-condition-variable name specific threads)
  condition-variable?
  (name condition-variable-name)
  (specific condition-variable-specific condition-variable-specific-set!)
  (threads %condition-variable-threads %condition-variable-threads-set!))

(define (make-condition-variable . o)
  (%make-condition-variable (and (pair? o) (car o)) #f #f))
