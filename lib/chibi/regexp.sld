
(define-library (chibi regexp)
  (export regexp regexp? regexp-matches regexp-matches? regexp-search
          regexp-replace regexp-replace-all
          regexp-fold regexp-extract regexp-split
          regexp-match? regexp-match-num-matches
          regexp-match-submatch regexp-match-submatch/list
          regexp-match->list regexp-match->sexp)
  (import (srfi 33) (srfi 69))
  ;; Chibi's char-set library is more factored than SRFI-14.
  (cond-expand
   (chibi
    (import (chibi) (srfi 9) (chibi char-set) (chibi char-set full)))
   (else
    (import (scheme base) (srfi 14))))
  (import (chibi char-set boundary))
  ;; Use string-cursors where available.
  (begin
    (define string-cursor? integer?))
  (cond-expand
   (chibi
    (begin
      (define (string-start-arg s o)
        (if (pair? o) (string-index->offset s (car o)) (string-cursor-start s)))
      (define (string-end-arg s o)
        (if (pair? o) (string-index->offset s (car o)) (string-cursor-end s)))
      (define (string-concatenate-reverse ls)
        (string-concatenate (reverse ls)))))
   (else
    (begin
      (define (string-start-arg s o)
        (if (pair? o) (string-index->offset s (car o)) 0))
      (define (string-end-arg s o)
        (if (pair? o) (string-index->offset (car o)) (string-length s)))
      (define string-cursor=? =)
      (define string-cursor<? <)
      (define string-cursor<=? <=)
      (define string-cursor>? >)
      (define string-cursor>=? >=)
      (define string-cursor-ref string-ref)
      (define (string-cursor-next s i) (+ i 1))
      (define (string-cursor-prev s i) (- i 1))
      (define substring-cursor substring)
      (define (string-offset->index str off) off)
      (define (string-index->offset str i) i)
      (define (string-concatenate-reverse ls)
        (apply string-append (reverse ls))))))
  (include "regexp.scm"))
