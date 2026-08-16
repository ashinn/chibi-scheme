(library (rnrs lists)
  (export find
          (rename every for-all)
          (rename any exists)
          filter partition
          fold-left
          fold-right
          (rename remove remp)
          (rename rnrs:remove remove)
          remv
          remq
          (rename find-tail memp)
          member
          memv
          memq
          assp
          assoc
          assv
          assq
          cons*)
  (import (scheme base)
          (srfi 1))

  (define (fold-left kons knil . lss)
    (apply fold
           (lambda args
             (apply kons (last args) (drop-right args 1)))
           knil lss))

  (define (rnrs:remove obj ls) (remove (lambda (x) (equal? x obj)) ls))
  (define (remv obj ls) (remove (lambda (x) (eqv? x obj)) ls))
  (define (remq obj ls) (remove (lambda (x) (eq? x obj)) ls))

  (define (assp proc alist)
    (find (lambda (x) (proc (car x))) alist)))
