
(define-library (chibi regexp)
  (export regexp regexp? valid-sre? rx regexp->sre char-set->sre
          regexp-matches regexp-matches? regexp-search
          regexp-replace regexp-replace-all
          regexp-fold regexp-extract regexp-split regexp-partition
          regexp-match? regexp-match-count
          regexp-match-submatch regexp-match-submatch/list
          regexp-match-submatch-start regexp-match-submatch-end
          regexp-match->list regexp-match->sexp)
  (import (srfi 33) (srfi 69))
  ;; Chibi's char-set library is more factored than SRFI-14.
  (cond-expand
   (chibi
    (import (rename (chibi)
                    (protect guard)
                    (char-downcase %char-downcase)
                    (char-upcase %char-upcase))
            (only (scheme char) char-downcase char-upcase)
            (srfi 9)
            (chibi char-set)
            (chibi char-set full)
            (prefix (chibi char-set ascii) %))
    (begin
      (define char-set:title-case
        (char-set-union
         (ucs-range->char-set #x1F88 #x1F90)
         (ucs-range->char-set #x1F98 #x1FA0)
         (ucs-range->char-set #x1FA8 #x1FB0)
         (char-set #\x01C5 #\x01C8 #\x01CB #\x01F2 #\x1FBC #\x1FCC #\x1FFC)))))
   (else
    (import (scheme base) (scheme char) (srfi 14))
    (begin
      (define %char-set:letter
        (char-set-intersection char-set:ascii char-set:letter))
      (define %char-set:lower-case
        (char-set-intersection char-set:ascii char-set:lower-case))
      (define %char-set:upper-case
        (char-set-intersection char-set:ascii char-set:upper-case))
      (define %char-set:digit
        (char-set-intersection char-set:ascii char-set:digit))
      (define %char-set:letter+digit
        (char-set-intersection char-set:ascii char-set:letter+digit))
      (define %char-set:punctuation
        (char-set-intersection char-set:ascii char-set:punctuation))
      (define %char-set:punctuation
        (char-set-intersection char-set:ascii char-set:symbol))
      (define %char-set:graphic
        (char-set-intersection char-set:ascii char-set:graphic))
      (define %char-set:whitespace
        (char-set-intersection char-set:ascii char-set:whitespace))
      (define %char-set:printing
        (char-set-intersection char-set:ascii char-set:printing))
      (define %char-set:iso-control
        (char-set-intersection char-set:ascii char-set:iso-control)))))
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
        (if (pair? o) (string-index->offset s (car o)) (string-length s)))
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
      (define (string-concatenate ls) (apply string-append ls))
      (define (string-concatenate-reverse ls)
        (string-concatenate (reverse ls))))))
  (include "regexp.scm"))
