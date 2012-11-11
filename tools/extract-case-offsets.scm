#!/usr/bin/env chibi-scheme

;; Extract sets of char case offsets.
;;
;; Usage:
;;   extract-case-offsets.scm options CaseFolding.txt > out
;;
;; Recognized options are:
;;
;;   -c <min-count>     - the minimum required count to output a char-set
;;                        for an offset, default 32
;;   -m <max-char-sets> - the maximum number of character sets to output
;;                        regardless of min-count, default umlimited
;;   -n <name>          - the name for char-sets, defaults to
;;                        "char-downcase-offsets"
;;   -o <output-file>   - the output file, defaults to stdout

(import (chibi) (srfi 1) (srfi 69) (srfi 95) (chibi io) (chibi string)
        (chibi iset) (chibi iset optimize))

(define (warn . args)
  (let ((err (current-error-port)))
    (for-each (lambda (x) (display x err)) args)
    (newline err)))

(define (write-offsets offset-map out min-count max-char-sets name)
  (let lp ((ls (sort (hash-table->alist offset-map)
                     (lambda (a b) (> (iset-size (cdr a)) (iset-size (cdr b))))))
           (i 0)
           (res '()))
    (cond
     ((and (pair? ls)
           (or (not max-char-sets) (< i max-char-sets))
           (or (not min-count) (>= (iset-size (cdar ls)) min-count)))
      (lp (cdr ls)
          (+ i 1)
          (cons `(cons ,(iset->code (iset-balance (iset-optimize (cdar ls))))
                       ,(caar ls))
                res)))
     (else
      (write `(define ,(string->symbol name) (list ,@(reverse res))) out)
      (newline out)
      (newline out)
      (let ((pairs
             (sort
              (append-map
               (lambda (x)
                 (map (lambda (y) (list y (+ y (car x))))
                      (iset->list (cdr x))))
               ls)
              (lambda (a b) (< (car a) (car b))))))
        (write `(define char-downcase-map
                  ',(list->vector (append-map (lambda (x) x) pairs)))
               out)
        (newline out)
        (newline out)
        (write `(define char-upcase-map
                  ',(list->vector
                     (append-map (lambda (x) (list (cadr x) (car x)))
                                 (sort pairs
                                       (lambda (a b) (< (cadr a) (cadr b)))))))
               out)
        (newline out))))))

(define (extract-case-folding in out min-count max-char-sets name)
  (define (string-trim-comment str comment-ch)
    (car (string-split str comment-ch 2)))
  (let ((offset-map (make-hash-table eq?)))
    (let lp ()
      (let ((line (read-line in)))
        (cond
         ((eof-object? line)
          (write-offsets offset-map out min-count max-char-sets name))
         ((or (equal? line "") (eqv? #\# (string-ref line 0)))
          (lp))
         (else
          (let ((ls (map string-trim
                         (string-split (string-trim-comment line #\#) #\;))))
            (cond
             ((< (length ls) 3)
              (warn "invalid CaseFolding.txt line: " line))
             (else
              (let ((upper (string->number (car ls) 16))
                    (status (string->symbol (cadr ls))))
                (cond
                 ((not upper)
                  (warn "invalid upper char in CaseFolding.txt: " line))
                 ((eqv? 'C status)
                  (let ((lower (string->number (car (cddr ls)) 16)))
                    (if (not lower)
                        (warn "invalid lower char in CaseFolding.txt: " line)
                        (hash-table-update!
                         offset-map
                         (- lower upper)
                         (lambda (is) (iset-adjoin! is upper))
                         (lambda () (make-iset))))))))))
            (lp))))))))

(let ((args (command-line)))
  (let lp ((ls (cdr args))
           (min-count 26)
           (max-char-sets #f)
           (name "char-downcase-offsets")
           (out (current-output-port)))
    (cond
     ((and (pair? ls) (not (equal? "" (car ls)))
           (eqv? #\- (string-ref (car ls) 0)))
      (cond
       ((member (car ls) '("-c" "--min-count"))
        (lp (cddr ls) (cadr ls) max-char-sets name out))
       ((member (car ls) '("-m" "--max-char-sets"))
        (lp (cddr ls) min-count (cadr ls) name out))
       ((member (car ls) '("-n" "--name"))
        (lp (cddr ls) min-count max-char-sets (cadr ls) out))
       ((member (car ls) '("-o" "--output"))
        (lp (cddr ls) min-count max-char-sets name
            (open-output-file (cadr ls))))
       (else
        (error "unknown option: " (car ls)))))
     ((null? ls)
      (error "usage: extract-case-offsets <CaseFolding.txt>"))
     (else
      (if (equal? "-" (car ls))
          (extract-case-folding
           (current-input-port) out min-count max-char-sets name)
          (call-with-input-file (car ls)
            (lambda (in)
              (extract-case-folding in out min-count max-char-sets name))))
      (close-output-port out)))))
