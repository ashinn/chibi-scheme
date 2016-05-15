;; write.scm - written formatting, the default displayed for non-string/chars
;; Copyright (c) 2006-2013 Alex Shinn.  All rights reserved.
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
     (lambda (ch) (write-char (if (eqv? ch ch1) ch2 ch) out))
     str)
    (get-output-string out)))

(define (string-intersperse-right str sep rule)
  (let lp ((i (string-length str))
           (rule rule)
           (res '()))
    (let* ((offset (if (pair? rule) (car rule) rule))
           (i2 (if offset (- i offset) 0)))
      (if (<= i2 0)
          (apply string-append (cons (substring str 0 i) res))
          (lp i2
              (if (pair? rule) (cdr rule) rule)
              (cons sep (cons (substring str i2 i) res)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numeric formatting

(define (char-mirror c)
  (case c ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else c)))

(define (integer-log a base)
  (if (zero? a)
      0
      (exact (ceiling (/ (log (+ a 1)) (log base))))))

;; The original fmt algorithm was based on "Printing Floating-Point
;; Numbers Quickly and Accurately" by Burger and Dybvig
;; (FP-Printing-PLDI96.pdf).  It had grown unwieldy with formatting
;; special cases, so the below is a simplification which tries to rely
;; on number->string for common cases.

(define (numeric n)
  (fn (radix precision decimal-sep decimal-align comma-rule comma-sep sign-rule)
    (let ((dec-sep (or decimal-sep (if (eqv? comma-sep #\.) #\, #\.))))
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
      (define (maybe-trim-zeros i res)
        (if (and (not precision) (positive? i))
            (let lp ((res res))
              (cond
               ((and (pair? res) (eqv? 0 (car res))) (lp (cdr res)))
               ((and (pair? res) (eqv? dec-sep (car res))) (cdr res))
               (else res)))
            res))
      ;; General slow loop to generate digits one at a time, for
      ;; non-standard radixes or writing rationals with a fixed
      ;; precision.
      (define (gen-general n)
        (let* ((p (exact n))
               (n (numerator p))
               (d (denominator p)))
          (let lp ((n n)
                   (i (- (integer-log p radix)))
                   (res '()))
            (cond
             ;; Use a fixed precision if specified, otherwise generate
             ;; 15 decimals.
             ((if precision (< i precision) (< i 16))
              (let ((res (if (zero? i)
                             (cons dec-sep (if (null? res) (cons 0 res) res))
                             res))
                    (q (quotient n d)))
                (cond
                 ((>= q radix)
                  (let* ((scale (get-scale q))
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
                    (reverse (maybe-round n d (maybe-trim-zeros i res))))))))))
      ;; Generate a fixed precision decimal result by post-editing the
      ;; result of string->number.
      (define (gen-fixed n)
        (cond
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
              (string-append s "." (make-string precision #\0)))
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
                                (odd? (digit-value
                                       (string-cursor-ref
                                        s (string-cursor-prev last 1))))))))
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
         ((and (exact? n) (not (integer? n)))
          (string-append (number->string (numerator n) radix)
                         "/"
                         (number->string (denominator n) radix)))
         ((memv radix (if (exact? n) '(2 8 10 16) '(10)))
          (number->string n))
         (else
          (gen-general n))))
      ;; Insert commas according to the current comma-rule.
      (define (insert-commas str)
        (let* ((dec-pos (string-find str dec-sep))
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
        (let* ((s0 (gen-positive-real n))
               (s1 (if (and (char? dec-sep)
                            (not (eqv? #\. dec-sep)))
                       (string-replace-all s0 #\. dec-sep)
                       s0)))
          (if comma-rule (insert-commas s1) s1)))
      ;; Wrap the sign of a real number, forcing a + prefix or using
      ;; parentheses (n) for negatives according to sign-rule.
      (define (wrap-sign n sign-rule)
        (cond
         ((negative? n)
          (if (char? sign-rule)
              (string-append (string sign-rule)
                             (wrap-comma (abs n))
                             (string (char-mirror sign-rule)))
              (string-append "-" (wrap-comma (abs n)))))
         ((eq? #t sign-rule)
          (string-append "+" (wrap-comma n)))
         (else
          (wrap-comma n))))
      ;; Format a single real number with padding as necessary.
      (define (format n sign-rule)
        (let ((s (wrap-sign n sign-rule)))
          (let* ((dec-pos (if decimal-align
                              (string-cursor->index s (string-find s dec-sep))
                              0))
                 (diff (- (or decimal-align 0) dec-pos 1)))
            (if (positive? diff)
                (string-append (make-string diff #\space) s)
                s))))
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
      (write-complex n))))

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
                    (if (or (exact? n) (eqv? radix 10))
                        (each (cdr cell) (number->string n (car cell)))
                        (with ((radix 10)) (numeric n))))))
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
