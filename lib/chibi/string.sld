
;;> A cursor-oriented string library.  Provides efficient string
;;> utilities for implementations with or without fast random-access
;;> strings.

(define-library (chibi string)
  (export
   string-cursor?
   string-cursor-start string-cursor-end string-cursor-ref
   string-cursor<? string-cursor<=? string-cursor>? string-cursor>=?
   string-cursor=? string-cursor-next string-cursor-prev substring-cursor
   string-cursor->index string-index->cursor
   string-cursor-forward string-cursor-back
   string-null? string-every string-any
   string-join string-split string-count
   string-trim string-trim-left string-trim-right
   string-mismatch string-mismatch-right
   string-prefix? string-suffix?
   string-find string-find-right string-find? string-skip string-skip-right
   string-fold string-fold-right string-map string-for-each
   string-contains make-string-searcher
   string-downcase-ascii string-upcase-ascii
   call-with-input-string call-with-output-string)
  (cond-expand
   (chibi
    (import (chibi) (chibi ast) (chibi char-set base))
    (begin
      (define (string-for-each proc str . los)
        (if (null? los)
            (string-fold (lambda (ch a) (proc ch)) #f str)
            (let ((los (cons str los)))
              (let lp ((is (map string-cursor-start los)))
                (cond
                 ((any (lambda (str i)
                         (string-cursor>=? i (string-cursor-end str)))
                       los is))
                 (else
                  (apply proc (map string-cursor-ref los is))
                  (lp (map string-cursor-next los is))))))))
      (define (string-map proc str . los)
        (call-with-output-string
          (lambda (out)
            (apply string-for-each
                   (lambda args (write-char (apply proc args) out))
                   str los))))))
   (else
    (import (scheme base) (scheme char) (srfi 14)
            (except (srfi 1) make-list list-copy))
    (begin
      (define (string-cursor->index str i) i)
      (define (string-index->cursor str i) i)
      (define string-cursor? integer?)
      (define string-cursor<? <)
      (define string-cursor>? >)
      (define string-cursor=? =)
      (define string-cursor<=? <=)
      (define string-cursor>=? >=)
      (define string-cursor-ref string-ref)
      (define (string-cursor-start s) 0)
      (define string-cursor-end string-length)
      (define (string-cursor-next s i) (+ i 1))
      (define (string-cursor-prev s i) (- i 1))
      (define (substring-cursor s start . o)
        (substring s start (if (pair? o) (car o) (string-length s))))
      (define (string-concatenate orig-ls . o)
        (let ((sep (if (pair? o) (car o) ""))
              (out (open-output-string)))
          (let lp ((ls orig-ls))
            (cond
             ((pair? ls)
              (if (and sep (not (eq? ls orig-ls)))
                  (write-string sep out))
              (write-string (car ls) out)
              (lp (cdr ls)))))
          (get-output-string out)))
      (define string-size string-length)
      (define (call-with-input-string str proc)
        (let* ((in (open-input-string str))
               (res (proc in)))
          (close-input-port in)
          res))
      (define (call-with-output-string proc)
        (let ((out (open-output-string)))
          (proc out)
          (let ((res (get-output-string out)))
            (close-output-port out)
            res))))))
  (cond-expand
   (chibi)
   ((library (srfi 13))
    (import (only (srfi 13) string-contains)))
   (else
    (begin
      (define (string-contains a b . o)  ; really, stupidly slow
        (let ((alen (string-length a))
              (blen (string-length b)))
          (let lp ((i (if (pair? o) (car o) 0)))
            (and (<= (+ i blen) alen)
                 (if (string=? b (substring a i (+ i blen)))
                     i
                     (lp (+ i 1))))))))))
  (include "string.scm"))
