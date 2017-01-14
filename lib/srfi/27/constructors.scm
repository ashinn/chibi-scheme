;; constructors.scm -- random function constructors
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (random-source-make-integers rs)
  (if (not (random-source? rs))
      (error "not a random source" rs))
  (lambda (n) (%random-integer rs n)))

(define (random-source-make-reals rs . o)
  (if (not (random-source? rs))
      (error "not a random source" rs))
  (lambda () (%random-real rs)))

