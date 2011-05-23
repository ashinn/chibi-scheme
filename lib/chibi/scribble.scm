;; scribble.scm - scribble parsing
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A library used for parsing "scribble" format, introduced
;;> by @hyperlink["http://www.racket-lang.org/"]{Racket} and
;;> the format used to write this manual.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general character utils

(define (char-mirror ch)
  (case ch ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else ch)))

(define (char-delimiter? ch)
  (or (eof-object? ch) (char-whitespace? ch)
      (memv ch '(#\( #\) #\[ #\] #\{ #\} #\" #\|))))

(define (char-punctuation? ch)
  (memv ch '(#\- #\+ #\! #\< #\> #\[ #\] #\|)))

(define (char-digit ch) (- (char->integer ch) (char->integer #\0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list utils

(define (drop ls n) (if (<= n 0) ls (drop (cdr ls) (- n 1))))

(define (drop-while pred ls)
  (if (or (null? ls) (not (pred (car ls)))) ls (drop-while pred (cdr ls))))

(define (list-prefix? prefix ls)
  (cond ((null? prefix) #t)
        ((null? ls) #f)
        ((equal? (car prefix) (car ls)) (list-prefix? (cdr prefix) (cdr ls)))
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scribble reader (standalone, don't use the native reader)

(define scribble-dot (list "."))
(define scribble-close (list ")"))

(define (if-peek-char ch in pass fail)
  (cond ((eqv? ch (peek-char in)) (read-char in) pass) (else fail)))

(define (skip-line in)
  (do ((c #f (read-char in))) ((or (eof-object? c) (eqv? c #\newline)))))

(define (read-float-tail in acc)
  (let lp ((res acc) (k 0.1))
    (let ((ch (read-char in)))
      (cond ((or (eof-object? ch) (char-delimiter? ch)) res)
            ((char-numeric? ch) (lp (+ res (* k (char-digit ch))) (* k 0.1)))
            (else (error "invalid numeric syntax"))))))

(define (read-number in acc base)
  (let lp ((acc acc))
    (let ((ch (peek-char in)))
      (cond
       ((or (eof-object? ch) (char-delimiter? ch)) acc)
       ((char-numeric? ch) (read-char in) (lp (+ (* acc base) (char-digit ch))))
       ((eqv? #\. ch)
        (read-char in)
        (if (= base 10)
            (begin (read-char in) (read-float-tail in (exact->inexact acc)))
            (error "non-base-10 floating point")))
       (else (error "invalid numeric syntax"))))))

(define (read-escaped in terminal)
  (let lp ((ls '()))
    (let ((ch (read-char in)))
      (cond
       ((or (eof-object? ch) (eqv? ch terminal)) (list->string (reverse ls)))
       ((eqv? ch #\\) (lp (cons (read-char in) ls)))
       (else (lp (cons ch ls)))))))

(define (read-symbol in ls)
  (do ((ls ls (cons c ls)) (c (peek-char in) (peek-char in)))
      ((char-delimiter? c) (string->symbol (list->string (reverse ls))))
    (read-char in)))

(define (scrib-read in)
  (define ch (read-char in))
  (cond
   ((eof-object? ch) ch)
   ((char-whitespace? ch) (scrib-read in))
   (else
    (case ch
      ((#\( #\[ #\{)
       (let lp ((res '()))
         (let ((x (scrib-read in)))
           (cond ((eof-object? x) (error "unterminated list" x))
                 ((eq? x scribble-close) (reverse res))
                 ((eq? x scribble-dot)
                  (let ((y (scrib-read in)))
                    (if (or (eof-object? y) (eq? y scribble-close))
                        (error "unterminated dotted list")
                        (let ((z (scrib-read in)))
                          (if (not (eq? z scribble-close))
                              (error "dot in non-terminal position in list" y z)
                              (append (reverse res) y))))))
                 (else (lp (cons x res)))))))
      ((#\} #\] #\)) scribble-close)
      ((#\.) (if (char-delimiter? (peek-char in)) scribble-dot (read-float-tail in 0.0)))
      ((#\') (list 'quote (scrib-read in)))
      ((#\`) (list 'quasiquote (scrib-read in)))
      ((#\,) (list (if-peek-char #\@ in 'unquote-splicing 'unquote) (scrib-read in)))
      ((#\@) (scribble-parse-escape in #\@))
      ((#\;) (skip-line in) (scrib-read in))
      ((#\|) (string->symbol (read-escaped in #\|)))
      ((#\") (read-escaped in #\"))
      ((#\+ #\-)
       (cond ((char-numeric? (peek-char in))
              ((if (eqv? ch #\+) + -) 0 (read-number in 0 10)))
             (else (read-symbol in (list ch)))))
      ((#\#)
       (case (peek-char in)
         ((#\t #\f) (eqv? (read-char in) #\t))
         ((#\() (list->vector (scrib-read in)))
         ((#\\)
          (read-char in)
          (if (char-alphabetic? (peek-char in))
              (let ((name (scrib-read in)))
                (case name
                  ((space) #\space) ((newline) #\newline)
                  (else (string-ref (symbol->string name) 0))))
              (read-char in)))
         (else (error "unknown # syntax"))))
      (else
       (if (char-numeric? ch)
           (read-number in (char-digit ch) 10)
           (read-symbol in (list ch))))))))

(define (scribble-read in)
  (let ((res (scrib-read in)))
    (cond ((eq? res scribble-dot) (error "invalid . in source"))
          ((eq? res scribble-close) (error "too many )'s"))
          (else res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scribble parser

(define (read-punctuation in)
  (if (not (eqv? #\| (peek-char in)))
      '()
      (let lp ((ls '()))
        (let ((c (peek-char in)))
          (cond ((or (eof-object? c) (not(char-punctuation? c))) ls)
                (else (lp (cons (char-mirror (read-char in)) ls))))))))

(define (read-prefix-wrapper in)
  (let lp ((wrap (lambda (x) x)))
    (case (peek-char in)
      ((#\') (read-char in) (lp (lambda (x) (wrap (list 'quote x)))))
      ((#\`) (read-char in) (lp (lambda (x) (wrap (list 'quasiquote x)))))
      ((#\,)
       (read-char in)
       (cond ((eqv? #\@ (peek-char in))
              (read-char in)
              (lp (lambda (x) (wrap (list 'unquote-splicing x)))))
             (else (lp (lambda (x) (wrap (list 'unquote x)))))))
      (else wrap))))

(define (scribble-parse-escape in ec)
  (define bracket-char #\[)
  (define brace-char #\{)
  (let* ((wrap (read-prefix-wrapper in))
         (c (peek-char in))
         (cmd (if (or (eqv? c bracket-char) (eqv? c brace-char)) '() (list (scribble-read in))))
         (data? (eqv? (peek-char in) bracket-char))
         (data (if data? (scribble-read in) '()))
         (punc (read-punctuation in))
         (body? (eqv? (peek-char in) brace-char))
         (body (cond (body? (read-char in) (scribble-parse in punc ec)) (else '()))))
    (wrap (if (or data? body?) (append cmd data body) (car cmd)))))

(define (scribble-parse in . o)
  (define init-punc (if (pair? o) (car o) '()))
  (define escape-char (if (and (pair? o) (pair? (cdr o))) (cadr o) #\@))
  (define comment-char #\;)
  (define bracket-char #\[)
  (define brace-char #\{)
  (define close-bracket-char (char-mirror bracket-char))
  (define close-brace-char (char-mirror brace-char))
  (define (collect str res)
    (if (pair? str) (cons (list->string (reverse str)) res) res))
  (define (skip-space in)
    (let ((ch (peek-char in)))
      (cond ((char-whitespace? ch) (read-char in) (skip-space in))
            ((eqv? ch #\;) (skip-line in) (skip-space in)))))
  (define (tok str res punc depth)
    (let ((c (read-char in)))
      (cond
        ((eof-object? c)
         (if (zero? depth)
             (reverse (collect str res))
             (error "unterminated expression" punc)))
        ((and (eqv? c escape-char) (list-prefix? punc str))
         (let ((c (peek-char in)))
           (cond
            ((eof-object? c)
             (tok str res punc depth))
            ((char-whitespace? c)
             (tok (cons c str) res punc depth))
            ((eqv? c comment-char)
             (read-char in)
             (cond ((eqv? brace-char (peek-char in))
                    (scribble-parse-escape in escape-char))
                   (else
                    (skip-line in)
                    ;; (let lp ()
                    ;;   (cond ((char-whitespace? (peek-char in)) (read-char in) (lp))))
                    ))
             (tok str res punc depth))
            ((eqv? c #\|)
             (read-char in)
             (let lp ((ls (collect str res)))
               (skip-space in)
               (cond ((eqv? #\| (peek-char in)) (read-char in) (tok '() ls punc depth))
                     (else (lp (cons (scribble-read in) ls))))))
            (else
             (let ((str (drop str (length punc)))
                   (x (scribble-parse-escape in escape-char)))
               (if (string? x)
                   (tok (append (reverse (string->list x)) str) res punc depth)
                   (tok '() (cons x (collect str res)) punc depth)))))))
        ((eqv? c brace-char)
         (tok (cons c str) res punc (+ depth 1)))
        ((eqv? c close-brace-char)
         (cond
          ((zero? depth)
           (let lp ((p punc) (ls '()))
             (cond ((null? p)
                    (reverse (collect str res)))
                   ((not (eqv? (car p) (peek-char in)))
                    (tok (append ls (cons c str)) res punc (- depth 1)))
                   (else
                    (lp (cdr p) (cons (read-char in) ls))))))
          (else (tok (cons c str) res punc (- depth 1)))))
        ((eqv? c #\newline)
         (let* ((res (collect str res))
                (res (if (and (null? res) (null? str))
                         res
                         (cons "\n" res))))
           (tok '() res punc depth)))
        (else
         (tok (cons c str) res punc depth)))))
  ;; begin
  (tok '() '() init-punc 0))
