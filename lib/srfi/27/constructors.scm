;; constructors.scm -- random function constructors
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (random-source-make-integers rs)
  (lambda (n) (%random-integer rs n)))

(define (random-source-make-reals rs . o)
  (lambda () (%random-real rs)))

