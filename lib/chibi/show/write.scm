;; write.scm - written formatting, the default displayed for non-string/chars
;; Copyright (c) 2006-2019 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{String utilities}

(define (write-to-string x)
  (let ((out (open-output-string)))
    (write x out)
    (get-output-string out)))

(define (string-replace-all str ch1 ch2)
  (let ((out (open-output-string)))
    (string-for-each
     (lambda (ch) (display (if (eqv? ch ch1) ch2 ch) out))
     str)
    (get-output-string out)))

(define (string-intersperse-right str sep rule)
  (let ((start (string-cursor-start str)))
    (let lp ((i (string-cursor-end str))
             (rule rule)
             (res '()))
      (let* ((offset (if (pair? rule) (car rule) rule))
             (i2 (if offset (string-cursor-back str i offset) start)))
        (if (string-cursor<=? i2 start)
            (apply string-append (cons (substring-cursor str start i) res))
            (lp i2
                (if (and (pair? rule) (not (null? (cdr rule)))) (cdr rule) rule)
                (cons sep (cons (substring-cursor str i2 i) res))))))))

;;> Outputs the string str, escaping any quote or escape characters.
;;> If esc-ch, which defaults to #\\, is #f, escapes only the
;;> quote-ch, which defaults to #\", by doubling it, as in SQL strings
;;> and CSV values.  If renamer is provided, it should be a procedure
;;> of one character which maps that character to its escape value,
;;> e.g. #\newline => #\n, or #f if there is no escape value.

(define (escaped fmt . o)
  (let-optionals* o ((quot #\")
                     (esc #\\)
                     (rename (lambda (x) #f)))
    (let ((esc-str (cond ((char? esc) (string esc))
                         ((not esc) (string quot))
                         (else esc))))
      (fn (output)
        (define (output* str)
          (let ((start (string-cursor-start str))
                (end (string-cursor-end str)))
            (let lp ((i start) (j start))
              (define (collect)
                (if (eq? i j) "" (substring-cursor str i j)))
              (if (string-cursor>=? j end)
                  (output (collect))
                  (let ((c (string-cursor-ref str j))
                        (j2 (string-cursor-next str j)))
                    (cond
                     ((or (eqv? c quot) (eqv? c esc))
                      (each (output (collect))
                            (output esc-str)
                            (fn () (lp j j2))))
                     ((rename c)
                      => (lambda (c2)
                           (each (output (collect))
                                 (output esc-str)
                                 (output (if (char? c2) (string c2) c2))
                                 (fn () (lp j2 j2)))))
                     (else
                      (lp i j2))))))))
        (with ((output output*))
          fmt)))))

;;> Only escape if there are special characters, in which case also
;;> wrap in quotes.  For writing symbols in |...| escapes, or CSV
;;> fields, etc.  The predicate indicates which characters cause
;;> slashification - this is in addition to automatic slashifying when
;;> either the quote or escape char is present.

(define (maybe-escaped fmt pred . o)
  (let-optionals* o ((quot #\")
                     (esc #\\)
                     (rename (lambda (x) #f)))
    (define (esc? c) (or (eqv? c quot) (eqv? c esc) (rename c) (pred c)))
    (call-with-output
     fmt
     (lambda (str)
       (if (string-cursor<? (string-find str esc?) (string-cursor-end str))
           (each quot (escaped str quot esc rename) quot)
           (displayed str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numeric formatting

(define (char-mirror c)
  (case c ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else c)))

(define (integer-log a base)
  (if (zero? a)
      0
      ;; (exact (ceiling (/ (log (+ a 1)) (log base))))
      (do ((ndigits 1 (+ ndigits 1))
           (p base (* p base)))
          ((> p a) ndigits))))

;; The original fmt algorithm was based on "Printing Floating-Point
;; Numbers Quickly and Accurately" by Burger and Dybvig
;; (FP-Printing-PLDI96.pdf).  It had grown unwieldy with formatting
;; special cases, so the below is a simplification which tries to rely
;; on number->string for common cases.

(define unspec (list 'unspecified))

(define-syntax default
  (syntax-rules ()
    ((default var dflt) (if (eq? var unspec) dflt var))))

(define (numeric n . o)
  (let-optionals* o ((rad unspec) (prec unspec) (sgn unspec)
                     (comma unspec) (commasep unspec) (decsep unspec))
    (fn (radix precision sign-rule
               comma-rule comma-sep decimal-sep decimal-align)
      (let* ((radix (default rad radix))
             (precision (default prec precision))
             (sign-rule (default sgn sign-rule))
             (comma-rule (default comma comma-rule))
             (comma-sep (default commasep comma-sep))
             (dec-sep (default decsep
                        (or decimal-sep (if (eqv? comma-sep #\.) #\, #\.))))
             (dec-ls (if (char? dec-sep)
                         (list dec-sep)
                         (reverse (string->list dec-sep)))))
        ;; General formatting utilities.
        (define (get-scale q)
          (expt radix (- (integer-log q radix) 1)))
        (define (char-digit d)
          (cond ((char? d) d)
                ((< d 10) (integer->char (+ d (char->integer #\0))))
                (else (integer->char (+ (- d 10) (char->integer #\a))))))
        (define (digit-value ch)
          (let ((res (- (char->integer ch) (char->integer #\0))))
            (if (<= 0 res 9)
                res
                ch)))
        (define (round-up ls)
          (let lp ((ls ls) (res '()))
            (cond
             ((null? ls)
              (cons 1 res))
             ((not (number? (car ls)))
              (lp (cdr ls) (cons (car ls) res)))
             ((= (car ls) (- radix 1))
              (lp (cdr ls) (cons 0 res)))
             (else
              (append (reverse res) (cons (+ 1 (car ls)) (cdr ls)))))))
        (define (maybe-round n d ls)
          (let* ((q (quotient n d))
                 (digit (* 2 (if (>= q radix) (quotient q (get-scale q)) q))))
            (if (or (> digit radix)
                    (and (= digit radix)
                         (let ((prev (find integer? ls)))
                           (and prev (odd? prev)))))
                (round-up ls)
                ls)))
        (define (maybe-trim-zeros i res inexact?)
          (if (and (not precision) (positive? i))
              (let lp ((res res))
                (cond
                 ((and (pair? res) (eqv? 0 (car res))) (lp (cdr res)))
                 ((and (pair? res)
                       (eqv? (car dec-ls) (car res))
                       (null? (cdr dec-ls)))
                  (if inexact?
                      (cons 0 res)      ; "1.0"
                      (cdr res)))       ; "1"
                 (else res)))
              res))
        ;; General slow loop to generate digits one at a time, for
        ;; non-standard radixes or writing rationals with a fixed
        ;; precision.
        (define (gen-general n-orig)
          (let* ((p (exact n-orig))
                 (n (numerator p))
                 (d (denominator p)))
            (let lp ((n n)
                     (i (if (zero? p) -1 (- (integer-log p radix))))
                     (res '()))
              (cond
               ;; Use a fixed precision if specified, otherwise generate
               ;; 15 decimals.
               ((if precision (< i precision) (< i 16))
                (let ((res (if (zero? i)
                               (append dec-ls (if (null? res) (cons 0 res) res))
                               res))
                      (q (quotient n d)))
                  (cond
                   ((< i -1)
                    (let* ((scale (expt radix (- -1 i)))
                           (digit (quotient q scale))
                           (n2 (- n (* d digit scale))))
                      (lp n2 (+ i 1) (cons digit res))))
                   (else
                    (lp (* (remainder n d) radix)
                        (+ i 1)
                        (cons q res))))))
               (else
                (list->string
                 (map char-digit
                      (reverse (maybe-trim-zeros i (maybe-round n d res) (inexact? n-orig))))))))))
        ;; Generate a fixed precision decimal result by post-editing the
        ;; result of string->number.
        (define (gen-fixed n)
          (cond
           ((and (eqv? radix 10) (zero? precision) (inexact? n))
            (number->string (exact (round n))))
           ((and (eqv? radix 10) (or (integer? n) (inexact? n)))
            (let* ((s (number->string n))
                   (end (string-cursor-end s))
                   (dec (string-find s #\.))
                   (digits (- (string-cursor->index s end)
                              (string-cursor->index s dec))))
              (cond
               ((string-cursor<? (string-find s #\e) end)
                (gen-general n))
               ((string-cursor=? dec end)
                (string-append s (if (char? dec-sep) (string dec-sep) dec-sep)
                               (make-string precision #\0)))
               ((<= digits precision)
                (string-append s (make-string (- precision digits -1) #\0)))
               (else
                (let* ((last
                        (string-cursor-back s end (- digits precision 1)))
                       (res (substring-cursor s (string-cursor-start s) last)))
                  (if (and
                       (string-cursor<? last end)
                       (let ((next (digit-value (string-cursor-ref s last))))
                         (or (> next 5)
                             (and (= next 5)
                                  (string-cursor>? last (string-cursor-start s))
                                  (memv (digit-value
                                         (string-cursor-ref
                                          s (string-cursor-prev s last)))
                                        '(1 3 5 7 9))))))
                      (list->string
                       (reverse
                        (map char-digit
                             (round-up
                              (reverse (map digit-value (string->list res)))))))
                      res))))))
           (else
            (gen-general n))))
        ;; Generate any unsigned real number.
        (define (gen-positive-real n)
          (cond
           (precision
            (gen-fixed n))
           ((memv radix (if (exact? n) '(2 8 10 16) '(10)))
            (number->string n radix))
           (else
            (gen-general n))))
        ;; Insert commas according to the current comma-rule.
        (define (insert-commas str)
          (let* ((dec-pos (if (string? dec-sep)
                              (or (string-contains str dec-sep)
                                  (string-cursor-end str))
                              (string-find str dec-sep)))
                 (left (substring-cursor str (string-cursor-start str) dec-pos))
                 (right (substring-cursor str dec-pos))
                 (sep (cond ((char? comma-sep) (string comma-sep))
                            ((string? comma-sep) comma-sep)
                            ((eqv? #\, dec-sep) ".")
                            (else ","))))
            (string-append
             (string-intersperse-right left sep comma-rule)
             right)))
        ;; Post-process a positive real number with decimal char fixup
        ;; and commas as needed.
        (define (wrap-comma n)
          (if (and (not precision) (exact? n) (not (integer? n)))
              (string-append (wrap-comma (numerator n))
                             "/"
                             (wrap-comma (denominator n)))
              (let* ((s0 (gen-positive-real n))
                     (s1 (if (or (eqv? #\. dec-sep)
                                 (equal? "." dec-sep))
                             s0
                             (string-replace-all s0 #\. dec-sep))))
                (if comma-rule (insert-commas s1) s1))))
        ;; Wrap the sign of a real number, forcing a + prefix or using
        ;; parentheses (n) for negatives according to sign-rule.

        (define-syntax is-neg-zero?
          (syntax-rules ()
            ((_ n)
             (is-neg-zero? (-0.0) n))
            ((_ (0.0) n)                ; -0.0 is not distinguished?
             #f)
            ((_ (-0.0) n)
             (eqv? -0.0 n))))
        (define (negative?* n)
          (or (negative? n)
              (is-neg-zero? n)))
        (define (wrap-sign n sign-rule)
          (cond
           ((negative?* n)
            (cond
             ((char? sign-rule)
              (string-append (string sign-rule)
                             (wrap-comma (- n))
                             (string (char-mirror sign-rule))))
             ((pair? sign-rule)
              (string-append (car sign-rule)
                             (wrap-comma (- n))
                             (cdr sign-rule)))
             (else
              (string-append "-" (wrap-comma (- n))))))
           ((eq? #t sign-rule)
            (string-append "+" (wrap-comma n)))
           (else
            (wrap-comma n))))
        ;; Format a single real number with padding as necessary.
        (define (format n sign-rule)
          (cond
           ((finite? n)
            (let* ((s (wrap-sign n sign-rule))
                   (dec-pos (if decimal-align
                                (string-cursor->index
                                 s
                                 (if (char? dec-sep)
                                     (string-find s dec-sep)
                                     (or (string-contains s dec-sep)
                                         (string-cursor-end s))))
                                0))
                   (diff (- (or decimal-align 0) dec-pos 1)))
              (if (positive? diff)
                  (string-append (make-string diff #\space) s)
                  s)))
           (else
            (number->string n))))
        ;; Write any number.
        (define (write-complex n)
          (cond
           ((and radix (not (and (integer? radix) (<= 2 radix 36))))
            (error "invalid radix for numeric formatting" radix))
           ((zero? (imag-part n))
            (displayed (format (real-part n) sign-rule)))
           (else
            (each (format (real-part n) sign-rule)
                  (format (imag-part n) #t)
                  "i"))))
        (write-complex n)))))

(define numeric/si
  (let* ((names10 '#("" "k" "M" "G" "T" "E" "P" "Z" "Y"))
         (names-10 '#("" "m" "µ" "n" "p" "f" "a" "z" "y"))
         (names2 (list->vector
                  (cons ""
                        (cons "Ki" (map (lambda (s) (string-append s "i"))
                                        (cddr (vector->list names10)))))))
         (names-2 (list->vector
                   (cons ""
                         (map (lambda (s) (string-append s "i"))
                              (cdr (vector->list names-10)))))))
    (define (round-to n k)
      (/ (round (* n k)) k))
    (lambda (n . o)
      (let-optionals* o ((base 1024)
                         (separator ""))
        (let* ((log-n (log n))
               (names  (if (negative? log-n)
                           (if (= base 1024) names-2 names-10)
                           (if (= base 1024) names2 names10)))
               (k (min (exact ((if (negative? log-n) ceiling floor)
                               (/ (abs log-n) (log base))))
                       (- (vector-length names) 1)))
               (n2 (round-to (/ n (expt base (if (negative? log-n) (- k) k)))
                             10)))
          (each (if (integer? n2)
                    (number->string (exact n2))
                    (inexact n2))
                ;; (if (zero? k) "" separator)
                separator
                (vector-ref names k)))))))

;; Force a number into a fixed width, print as #'s if doesn't fit.
;; Needs to be wrapped in PADDED if you want to expand to the width.

(define (numeric/fitted width n . args)
  (call-with-output
   (apply numeric n args)
   (lambda (str)
     (if (> (string-length str) width)
         (fn (precision decimal-sep comma-sep)
           (let ((prec (if (and (pair? args) (pair? (cdr args)))
                           (cadr args)
                           precision)))
             (if (and prec (not (zero? prec)))
                 (let* ((dec-sep
                         (or decimal-sep
                             (if (eqv? #\. comma-sep) #\, #\.)))
                        (diff (- width (+ prec
                                          (if (char? dec-sep)
                                              1
                                              (string-length dec-sep))))))
                   (each (if (positive? diff) (make-string diff #\#) "")
                         dec-sep (make-string prec #\#)))
                 (displayed (make-string width #\#)))))
         (displayed str)))))

(define (numeric/comma n . o)
  (fn (comma-rule)
    (with ((comma-rule (or comma-rule 3)))
      (apply numeric n o))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shared structure utilities

(define (extract-shared-objects x cyclic-only?)
  (let ((seen (make-hash-table eq?)))
    ;; find shared references
    (let find ((x x))
      (cond ;; only interested in pairs and vectors (and records later)
       ((or (pair? x) (vector? x))
        ;; increment the count
        (hash-table-update!/default seen x (lambda (n) (+ n 1)) 0)
        ;; walk if this is the first time
        (cond
         ((> (hash-table-ref seen x) 1))
         ((pair? x)
          (find (car x))
          (find (cdr x)))
         ((vector? x)
          (do ((i 0 (+ i 1)))
              ((= i (vector-length x)))
            (find (vector-ref x i)))))
        ;; delete if this shouldn't count as a shared reference
        (if (and cyclic-only? (<= (hash-table-ref/default seen x 0) 1))
            (hash-table-delete! seen x)))))
    ;; extract shared references
    (let ((res (make-hash-table eq?))
          (count 0))
      (hash-table-walk
       seen
       (lambda (k v)
         (cond
          ((> v 1)
           (hash-table-set! res k (cons count #f))
           (set! count (+ count 1))))))
      (cons res 0))))

(define (maybe-gen-shared-ref cell shares)
  (cond
    ((pair? cell)
     (set-car! cell (cdr shares))
     (set-cdr! cell #t)
     (set-cdr! shares (+ (cdr shares) 1))
     (each "#" (number->string (car cell)) "="))
    (else nothing)))

(define (call-with-shared-ref obj shares proc)
  (let ((cell (hash-table-ref/default (car shares) obj #f)))
    (if (and (pair? cell) (cdr cell))
        (each "#" (number->string (car cell)) "#")
        (each (maybe-gen-shared-ref cell shares) proc))))

(define (call-with-shared-ref/cdr obj shares proc . o)
  (let ((sep (displayed (if (pair? o) (car o) "")))
        (cell (hash-table-ref/default (car shares) obj #f)))
    (cond
      ((and (pair? cell) (cdr cell))
       (each sep ". #" (number->string (car cell)) "#"))
      ((pair? cell)
       (each sep ". " (maybe-gen-shared-ref cell shares) "(" proc ")"))
      (else
       (each sep proc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; written

(define (write-with-shares obj shares)
  (fn (radix precision)
    (let ((write-number
           ;; Shortcut for numeric values.  Try to rely on
           ;; number->string for standard radixes and no precision,
           ;; otherwise fall back on numeric but resetting to a usable
           ;; radix.
           (cond
            ((and (not precision)
                  (assv radix '((16 . "#x") (10 . "") (8 . "#o") (2 . "#b"))))
             => (lambda (cell)
                  (lambda (n)
                    (cond
                     ((eqv? radix 10)
                      (displayed (number->string n (car cell))))
                     ((exact? n)
                      (each (cdr cell) (number->string n (car cell))))
                     (else
                      (with ((radix 10)) (numeric n)))))))
            (else (lambda (n) (with ((radix 10)) (numeric n)))))))
      ;; `wr' is the recursive writer closing over the shares.
      (let wr ((obj obj))
        (call-with-shared-ref
         obj shares
         (fn ()
           (cond
            ((pair? obj)
             (each "("
                   (fn ()
                     (let lp ((ls obj))
                       (let ((rest (cdr ls)))
                         (each (wr (car ls))
                               (cond
                                ((null? rest)
                                 nothing)
                                ((pair? rest)
                                 (each
                                  " "
                                  (call-with-shared-ref/cdr
                                   rest shares
                                   (fn () (lp rest)))))
                                (else
                                 (each " . " (wr rest))))))))
                   ")"))
            ((vector? obj)
             (let ((len (vector-length obj)))
               (if (zero? len)
                   (displayed "#()")
                   (each "#("
                         (wr (vector-ref obj 0))
                         (fn ()
                           (let lp ((i 1))
                             (if (>= i len)
                                 nothing
                                 (each " " (wr (vector-ref obj i))
                                       (fn () (lp (+ i 1)))))))
                         ")"))))
            ((number? obj)
             (write-number obj))
            (else
             (displayed (write-to-string obj))))))))))

;; The default formatter for `written', overriden with the `writer'
;; variable.  Intended to be equivalent to `write', using datum labels
;; for shared notation iff there are cycles in the object.

(define (written-default obj)
  (fn ()
    (write-with-shares obj (extract-shared-objects obj #t))))

;; Writes the object showing the full shared structure.

(define (written-shared obj)
  (fn ()
    (write-with-shares obj (extract-shared-objects obj #f))))

;; The only expensive part, in both time and memory, of handling
;; shared structures when writing is building the initial table, so
;; for the efficient version we just skip that and re-use the writing
;; code.

(define (written-simply obj)
  (fn ()
    (write-with-shares obj (extract-shared-objects #f #f))))

;; Local variables:
;; eval: (put 'fn 'scheme-indent-function 1)
;; End:
