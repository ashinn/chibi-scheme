
(define-library (srfi 14)
  (export
   char-set? char-set= char-set<=
   char-set-hash 
   char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
   char-set-fold char-set-unfold char-set-unfold!
   char-set-for-each char-set-map
   char-set-copy char-set

   list->char-set  string->char-set 
   list->char-set! string->char-set! 

   char-set-filter  ucs-range->char-set  ->char-set
   char-set-filter! ucs-range->char-set!

   char-set->list char-set->string

   char-set-size char-set-count char-set-contains?
   char-set-every char-set-any

   char-set-adjoin  char-set-delete 
   char-set-adjoin! char-set-delete!

   char-set-complement  char-set-union  char-set-intersection  
   char-set-complement! char-set-union! char-set-intersection! 

   char-set-difference  char-set-xor  char-set-diff+intersection
   char-set-difference! char-set-xor! char-set-diff+intersection!

   char-set:lower-case   char-set:upper-case    char-set:title-case
   char-set:letter       char-set:digit         char-set:letter+digit
   char-set:graphic      char-set:printing      char-set:whitespace
   char-set:iso-control  char-set:punctuation   char-set:symbol
   char-set:hex-digit    char-set:blank         char-set:ascii
   char-set:empty        char-set:full)
  (import (scheme base)
          (chibi char-set)
          (chibi char-set full)
          (chibi iset)
          (only (srfi 125) hash))
  (begin
    (define char-set= iset=)
    (define char-set<= iset<=)
    (define char-set-hash hash)
    (define char-set-cursor iset-cursor)
    (define char-set-cursor-next iset-cursor-next)
    (define (char-set-ref cs cur) (integer->char (iset-ref cs cur)))
    (define end-of-char-set? end-of-iset?)
    (define (char-set-fold kons knil cs)
      (iset-fold (lambda (i x) (kons (integer->char i) x)) knil cs))
    (define (char-set-unfold! p f g seed cs)
      (let lp ((seed seed) (cs cs))
        (if (p seed)
            cs
            (lp (g seed) (char-set-adjoin! cs (f seed))))))
    (define (char-set-unfold p f g seed . o)
      (let ((cs (if (pair? o) (char-set-copy (car o)) (char-set))))
        (char-set-unfold! p f g seed cs)))
    (define (char-set-map proc cs)
      (iset-map (lambda (i) (char->integer (proc (integer->char i)))) cs))
    (define list->char-set! list->char-set)
    (define string->char-set! string->char-set)
    (define ucs-range->char-set! ucs-range->char-set)
    (define (->char-set x)
      (cond ((char? x) (char-set x))
            ((pair? x) (list->char-set x))
            ((string? x) (string->char-set x))
            (else x)))
    (define (char-set-delete cs . o)
      (apply iset-delete cs (map char->integer o)))
    (define (char-set-delete! cs . o)
      (apply iset-delete! cs (map char->integer o)))
    (define char-set-complement! char-set-complement)
    (define (char-set-filter pred cs . o)
      (char-set-fold
       (lambda (ch res) (if (pred ch) (char-set-adjoin! res ch) res))
       (if (pair? o) (char-set-copy (car o)) (char-set))
       cs))
    (define char-set-filter! char-set-filter)
    (define (char-set-count pred cs)
      (char-set-fold (lambda (ch i) (if (pred ch) (+ i 1) i)) 0 cs))
    (define (char-set-any pred cs)
      (let lp ((cur (char-set-cursor cs)))
        (if (end-of-char-set? cur)
            #f
            (or (pred (char-set-ref cs cur))
                (lp (char-set-cursor-next cs cur))))))
    (define (char-set-every pred cs)
      (not (char-set-any (lambda (ch) (not (pred ch))) cs)))
    (define (char-set-xor2 cs1 cs2)
      (char-set-union (char-set-difference cs1 cs2)
                      (char-set-difference cs2 cs1)))
    (define (char-set-xor . o)
      (cond
       ((null? o) (char-set))
       ((null? (cdr o)) (char-set-copy (car o)))
       (else (apply char-set-xor (char-set-xor2 (car o) (cadr o)) (cddr o)))))
    (define char-set-xor! char-set-xor)
    (define (char-set-diff+intersection . o)
      (values (apply char-set-difference o)
              (apply char-set-intersection o)))
    (define char-set-diff+intersection! char-set-diff+intersection)))
