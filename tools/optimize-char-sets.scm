#!/usr/bin/env chibi-scheme

;; Simple tool to generate libraries of optimized char-sets.
;;
;; Usage:
;;   optimize-char-sets.scm [--ascii] module.name > out
;;
;; Imports (module name) and writes optimized versions of all exported
;; char-sets to stdout.

(import (chibi) (srfi 1) (srfi 69)
        (chibi io) (chibi string) (chibi modules)
        (chibi char-set) (chibi iset) (chibi iset optimize)
        (only (meta) load-module))

;; Use a hash table for speedup of huge sets instead of O(n^2)
;; srfi-1 implementation.
(define (lset-diff ls1 ls2)
  (let ((ls2-tab (make-hash-table eq?)))
    (for-each (lambda (i) (hash-table-set! ls2-tab i #t)) ls2)
    (remove (lambda (i) (hash-table-exists? ls2-tab i)) ls1)))

(let ((args (command-line)))
  (let lp ((ls (cdr args)) (ascii? #f))
    (cond
     ((and (pair? ls) (not (equal? "" (car ls)))
           (eqv? #\- (string-ref (car ls) 0)))
      (cond
       ((member (car ls) '("-a" "--ascii"))
        (lp (cdr ls) #t))
       (else (error "unknown option" (car ls)))))
     ((or (null? ls) (pair? (cdr ls)))
      (error "usage: optimize-char-sets.scm [--ascii] module.name"))
     (else
      (let ((mod (load-module
                  (map (lambda (x) (or (string->number x) (string->symbol x)))
                       (string-split (car ls) #\.)))))
        (for-each
         (lambda (exp)
           (display ";; ") (write exp) (newline)
           (let ((value (module-ref mod exp)))
             (cond
              ((char-set? value)
               (write `(optimize ,exp) (current-error-port)) (newline (current-error-port))
               (if (not (equal? (iset->list value)
                                (do ((cur (iset-cursor value)
                                          (iset-cursor-next value cur))
                                     (res '() (cons (iset-ref value cur) res)))
                                    ((end-of-iset? cur) (reverse res)))))
                   (error "error in iset cursors"))
               (display "  computing intersection\n" (current-error-port))
               (let* ((iset1 (if ascii?
                                 (iset-intersection char-set:ascii value)
                                 value))
                      (_ (display "  optimizing\n" (current-error-port)))
                      (iset-opt (iset-optimize iset1))
                      (_ (display "  balancing\n" (current-error-port)))
                      (iset2 (iset-balance iset-opt)))
                 (if (and (not ascii?) (not (iset= iset1 iset2)))
                     (begin
                       (display "  different!\n" (current-error-port))
                       (let* ((ls1 (iset->list iset1))
                              (ls2 (iset->list iset2))
                              (diff1 (lset-diff ls1 ls2))
                              (diff2 (lset-diff ls2 ls1)))
                         (display "  original: " (current-error-port))
                         (write (length ls1) (current-error-port))
                         (display " elements, missing: " (current-error-port))
                         (write diff1 (current-error-port))
                         (newline (current-error-port))
                         (display "  optimized: " (current-error-port))
                         (write (length ls2) (current-error-port))
                         (display " elements, missing: " (current-error-port))
                         (write diff2 (current-error-port))
                         (newline (current-error-port))
                         (error "optimized iset is different"))))
                 (display "  writing\n" (current-error-port))
                 (write `(define ,exp
                           (immutable-char-set ,(iset->code iset2))))
                 (newline)
                 (newline))))))
         (module-exports mod)))))))
