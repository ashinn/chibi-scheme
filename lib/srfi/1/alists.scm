;; alist.scm -- association list utilities
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (alist-cons key value ls) (cons (cons key value) ls))

(define (alist-copy ls) (map (lambda (x) (cons (car x) (cdr x))) ls))

(define (alist-delete key ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (remove (lambda (x) (eq (car x) key)) ls)))

(define alist-delete! alist-delete)

