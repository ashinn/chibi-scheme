;;; Copyright (C) William D Clinger (2016).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

(define-library (srfi 135)

  (export

   ;; Predicates

   text?                 textual?
   textual-null? 
   textual-every         textual-any

   ;; Constructors

   make-text             text
   text-tabulate
   text-unfold           text-unfold-right

   ;; Conversion

   textual->text
   textual->string       textual->vector      textual->list
   string->text          vector->text         list->text    reverse-list->text
   textual->utf8         textual->utf16be
   textual->utf16        textual->utf16le
   utf8->text            utf16be->text
   utf16->text           utf16le->text

   ;; Selection

   text-length           textual-length
   text-ref              textual-ref
   subtext               subtextual
   textual-copy
   textual-take          textual-take-right
   textual-drop          textual-drop-right
   textual-pad           textual-pad-right 
   textual-trim          textual-trim-right   textual-trim-both

   ;; Replacement

   textual-replace

   ;; Comparison

   textual=?             textual-ci=?
   textual<?             textual-ci<?
   textual>?             textual-ci>?
   textual<=?            textual-ci<=?
   textual>=?            textual-ci>=?

   ;; Prefixes & suffixes

   textual-prefix-length textual-suffix-length
   textual-prefix?       textual-suffix?    

   ;; Searching

   textual-index         textual-index-right
   textual-skip          textual-skip-right
   textual-contains      textual-contains-right

   ;; Case conversion

   textual-upcase        textual-downcase
   textual-foldcase      textual-titlecase

   ;; Concatenation

   textual-append        textual-concatenate  textual-concatenate-reverse
   textual-join

   ;; Fold & map & friends

   textual-fold          textual-fold-right
   textual-map           textual-for-each
   textual-map-index     textual-for-each-index
   textual-count
   textual-filter        textual-remove
;  textual-reverse

   ;; Replication & splitting

   textual-replicate     textual-split
   )

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (srfi 135 kernel8))

  (cond-expand
   ((library (rnrs unicode))
    (import (only (rnrs unicode) string-titlecase)))
   ((library (srfi 129))
    (import (only (srfi 129) string-titlecase)))
   (else
    (begin
     (define (string-titlecase s)
       (%string-titlecase s)))))

  ;; textual-replicate needs a sensible mod procedure

  (cond-expand
   ((library (rnrs base))
    (import (only (rnrs base) div mod)))
   (else
    (begin

      (define (assertion-violation procname msg . irritants)
        (apply error msg irritants))

      ;; Restricted to exact integers, which is all we need here.

      (define (div-and-mod x y)
        (cond ((and (exact-integer? x) (exact-integer? y))
               (cond ((= y 0)
                      (error "mod: zero divisor" x y))
                     ((>= x 0)
                      (values (quotient x y) (remainder x y)))
                     ((< y 0)
                                        ; x < 0, y < 0
                      (let* ((q (quotient x y))
                             (r (- x (* q y))))
                        (if (= r 0)
                            (values q 0)
                            (values (+ q 1) (- r y)))))
                     (else
                                        ; x < 0, y > 0
                      (let* ((q (quotient x y))
                             (r (- x (* q y))))
                        (if (= r 0)
                            (values q 0)
                            (values (- q 1) (+ r y)))))))
              (else
               (error "div or mod: illegal arguments" x y))))

      (define (div x y)
        (cond ((and (exact-integer? x)
                    (exact-integer? y)
                    (>= x 0))
               (quotient x y))
              (else
               (call-with-values
                   (lambda () (div-and-mod x y))
                 (lambda (q r) q)))))

      (define (mod x y)
        (cond ((and (exact-integer? x)
                    (exact-integer? y)
                    (>= x 0))
               (remainder x y))
              (else
               (call-with-values
                   (lambda () (div-and-mod x y))
                 (lambda (q r) r))))))))

  ;; To run texts-search-test.sps, change the (or) to (and).

  (cond-expand ((or)
                (export
                 %textual-contains:naive
                 %textual-contains:rabin-karp
                 %textual-contains:boyer-moore

                 %textual-contains-right:naive
                 %textual-contains-right:boyer-moore
                 ))
               (else))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; The recommended external syntax cannot be implemented portably.
  ;;; Here is Larceny-specific code that generates the recommended
  ;;; syntax for output, but does not make the changes necessary to
  ;;; accept that recommended syntax for input or as a literal in
  ;;; programs.

  (cond-expand (larceny
                (import (scheme write)
                        (primitives rtd-printer-set!)))
               (else))

  (cond-expand (larceny
                (begin
                 (define (text-write txt p)
                   (let* ((q (open-output-string))
                          (s (begin (write (textual->string txt) q)
                                    (get-output-string q))))
                     (write-char (integer->char #x00ab) p)
                     (write-string (substring s 1 (- (string-length s) 1)) p)
                     (write-char (integer->char #x00bb) p)))

                 (define ignored-result-from-rtd-printer-set!    ; for R6RS
                   (rtd-printer-set! text-rtd text-write))))
               (else))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (include "135.scm"))

;;; eof
