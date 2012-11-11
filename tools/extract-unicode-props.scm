#!/usr/bin/env chibi-scheme

;; Simple tool to extract Unicode properties as character-sets.
;;
;; Usage:
;;   extract-unicode-props.scm Lowercase Punctuation=P Blank=Zs,0009 >out
;;
;; Accepts a list of char-set names with optional definitions as
;; arguments, and writes their Scheme definitions to stdout.  A
;; char-set can be of the form:
;;
;;   Name: equivalent to Name=Name
;;   Name=value,...:
;;
;; A value can be any of:
;;
;;   Property_Name: all unicode characters with the given derived property
;;   Xx: all unicode characters with the given general category
;;   X: all unicode characters with any general category X*
;;   NNNN: a single unicode value in hex format
;;   NNNN-NNNN: an inclusive range of unicode values in hex format
;;
;; The char-set names generated are always lowercased, replacing _
;; with -, for convenicence when the char-set name coincides with a
;; Unicode property name.
;;
;; Assumes the files UnicodeData.txt and DerivedCoreProperties.txt are
;; in the data/ current directory, unless overridden with the --data or
;; --derived options.

(import (chibi) (chibi io) (chibi string))

(define (warn . args)
  (let ((err (current-error-port)))
    (for-each (lambda (x) (display x err)) args)
    (newline err)))

;; Parse UnicodeData.txt for characters matching a given class.
(define (extract-char-set-category cat data)
  (define (join-to-range n ls)
    (cond
     ((null? ls)
      (list n))
     ((eqv? (car ls) (- n 1))
      (cons (cons (car ls) n) (cdr ls)))
     ((and (pair? (car ls)) (eqv? (- n 1) (cdar ls)))
      (cons (cons (caar ls) n) (cdr ls)))
     (else
      (cons n ls))))
  (call-with-input-file data
    (lambda (in)
      (let lp ((ranges '()))
        (let ((line (read-line in)))
          (cond
           ((eof-object? line)
            `(char-set-union
              ,@(map (lambda (x)
                       (if (pair? x)
                           `(ucs-range->char-set ,(car x) ,(+ 1 (cdr x)))
                           `(char-set ,(integer->char x))))
                     (reverse ranges))))
           ((or (equal? line "") (eqv? #\# (string-ref line 0)))
            (lp ranges))
           (else
            (let ((ls (string-split line #\; 4)))
              (cond
               ((< (length ls) 3)
                (warn "invalid UnicodeData line: " line)
                (lp ranges))
               (else
                (let ((ch (string->number (car ls) 16))
                      (name (cadr ls))
                      (ch-cat (car (cddr ls))))
                  (cond
                   ((or (not ch) (not (= 2 (string-length ch-cat))))
                    (warn "invalid UnicodeData line: " line))
                   ((if (char? cat)
                        (eqv? cat (string-ref ch-cat 0))
                        (equal? cat ch-cat))
                    (lp (join-to-range ch ranges)))
                   (else
                    (lp ranges))))))))))))))

;; Parse DerivedCoreProperties.txt for characters matching a given
;; property.
(define (extract-char-set-property prop derived)
  (define (string-trim-comment str comment-ch)
    (car (string-split str comment-ch 2)))
  (call-with-input-file derived
    (lambda (in)
      (let lp ((ranges '()))
        (let ((line (read-line in)))
          (cond
           ((eof-object? line)
            `(char-set-union ,@(reverse ranges)))
           ((or (equal? line "") (eqv? #\# (string-ref line 0)))
            (lp ranges))
           (else
            (let ((ls (string-split (string-trim-comment line #\#) #\;)))
              (cond
               ((< (length ls) 2)
                (warn "invalid DerivedCoreProperties line: " line)
                (lp ranges))
               ((string-ci=? prop (string-trim (cadr ls)))
                (cond
                 ((string-contains (car ls) "..")
                  => (lambda (i)
                       (let* ((str (string-trim (car ls)))
                              (start (string->number (substring str 0 i) 16))
                              (end (string->number (substring str (+ i 2)) 16)))
                         (if (and start end (<= 0 start end #x110000))
                             (lp (cons `(ucs-range->char-set ,start ,(+ end 1))
                                       ranges))
                             (error "invalid char range: " line)))))
                 ((string->number (cadr ls) 16)
                  => (lambda (n)
                       (lp (cons `(char-set ,(integer->char n)) ranges))))
                 (else
                  (lp ranges))))
               (else
                (lp ranges)))))))))))

(define (extract-char-set-simple def data derived)
  (let ((ls (string-split def #\- 2)))
    (cond
     ((= 2 (length ls))
      (let ((start (string->number (car ls) 16))
            (end (string->number (cadr ls) 16)))
        (if (and start end (<= start end))
            `(ucs-range->char-set ,start ,(+ end 1))
            (error "invalid character range, expected NNNN-MMMM, got: " def))))
     ((string->number def 16)
      => (lambda (start) `(char-set ,(integer->char start))))
     ((and (= 1 (string-length def))
           (char-upper-case? (string-ref def 0)))
      (extract-char-set-category (string-ref def 0) data))
     ((and (= 2 (string-length def))
           (char-upper-case? (string-ref def 0))
           (char-lower-case? (string-ref def 1)))
      (extract-char-set-category def data))
     (else  ;; derived property
      (extract-char-set-property def derived)))))

(define (extract-char-set def data derived)
  (let ((defs (string-split def #\,)))
    (cond
     ((= 1 (length defs))
      (extract-char-set-simple (car defs) data derived))
     (else
      `(char-set-union
        ,@(map (lambda (def) (extract-char-set-simple def data derived))
               defs))))))

(define (process-char-set name def data derived out)
  (define (normalize-char-set-name str)
    (string-append
     "char-set:"
     (string-map (lambda (ch) (if (eqv? ch #\_) #\- (char-downcase ch))) str)))
  (display ";; " out)
  (display def out)
  (newline out)
  (write
   `(define ,(string->symbol (normalize-char-set-name name))
      (immutable-char-set
       ,(extract-char-set def data derived)))
   out)
  (newline out)
  (newline out))

(define default-char-sets
  '("Lower-Case=Lowercase"
    "Upper-Case=Uppercase"
    "Title-Case=Lt"
    "Letter=Alphabetic"
    "Punctuation=P"
    "Symbol=S"
    "Blank=Zs,0009"
    "Whitespace=Zs,Zl,Zp,0009,000A,000B,000C,000D"
    "Digit=Nd"))

(let ((args (command-line)))
  (let lp ((ls (cdr args))
           (data "data/UnicodeData.txt")
           (derived "data/DerivedCoreProperties.txt")
           (out (current-output-port)))
    (cond
     ((and (pair? ls) (not (equal? "" (car ls)))
           (eqv? #\- (string-ref (car ls) 0)))
      (cond
       ((member (car ls) '("-d" "--data"))
        (lp (cddr ls) (cadr ls) derived out))
       ((member (car ls) '("-e" "--derived"))
        (lp (cddr ls) data (cadr ls) out))
       ((member (car ls) '("-o" "--output"))
        (lp (cddr ls) data derived (open-output-file (cadr ls))))
       ((member (car ls) '("f" "--default"))
        (lp (append default-char-sets (cdr ls)) data derived out))
       (else
        (error "unknown option: " (car ls)))))
     ((pair? ls)
      (let ((ls (string-split (car ls) #\= 2)))
        (cond
         ((= 1 (length ls))
          (process-char-set (car ls) (car ls) data derived out))
         (else
          (process-char-set (car ls) (cadr ls) data derived out))))
      (lp (cdr ls) data derived out))
     (else
      (close-output-port out)))))
