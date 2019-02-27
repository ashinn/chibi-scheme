;; column.scm -- formatting columns and tables
;; Copyright (c) 2006-2017 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (string-split-words str separator?)
  (let ((start (string-cursor-start str))
        (end (string-cursor-end str)))
    (let lp ((sc start) (res '()))
      (cond
       ((string-cursor>=? sc end)
        (reverse res))
       (else
        (let ((sc2 (string-index str separator? sc)))
          (lp (string-cursor-next str sc2)
              (if (string-cursor=? sc sc2)
                  res
                  (cons (substring/cursors str sc sc2) res)))))))))

(define (call-with-output-generator producer consumer)
  (fn ()
    (let ((out (open-output-string))
          (queue (list-queue))
          (return #f)
          (resume #f))
      (define eof (read-char (open-input-string "")))
      (define (output* str)
        (fn (row col string-width)
          (list-queue-add-back! queue str)
          (each
           (let ((nl-index
                  (string-index-right str (lambda (ch) (eqv? ch #\newline)))))
             (if (string-cursor>? nl-index (string-cursor-start str))
                 (with!
                  (row (+ row (string-count str (lambda (ch) (eqv? ch #\newline)))))
                  (col (string-width str (string-cursor->index str nl-index))))
                 (with! (col (+ col (string-width str))))))
           (call-with-current-continuation
            (lambda (cc)
              (set! resume cc)
              (return nothing))))
          nothing))
      (define (generate)
        (when (and resume (list-queue-empty? queue))
          (call-with-current-continuation
           (lambda (cc)
             (set! return cc)
             (resume nothing))))
        (if (list-queue-empty? queue)
            eof
            (list-queue-remove-front! queue)))
      (forked (fn () (with ((port out) (output output*))
                       (call-with-current-continuation
                        (lambda (cc)
                          (set! return cc)
                          (each producer
                                (fn (output)
                                  (set! resume #f)
                                  (fn () (return nothing) nothing)))))))
               (consumer generate)))))

(define (call-with-output-generators producers consumer)
  (let lp ((ls producers) (generators '()))
    (if (null? ls)
        (consumer (reverse generators))
        (call-with-output-generator
         (car ls)
         (lambda (generator)
           (lp (cdr ls) (cons generator generators)))))))

(define (string->line-generator source)
  (let ((str '())
        (scanned? #f))
    (define (gen)
      (if (pair? str)
          (if scanned?
              (let ((res (source)))
                (cond
                 ((eof-object? res)
                  (let ((res (string-concatenate (reverse str))))
                    (set! str '())
                    res))
                 ((equal? res "")
                  (gen))
                 (else
                  (set! str (cons res str))
                  (set! scanned? #f)
                  (gen))))
              (let ((nl (string-index (car str) #\newline))
                    (end (string-cursor-end (car str))))
                (cond
                 ((string-cursor<? nl end)
                  (let* ((left (substring/cursors
                                (car str)
                                (string-cursor-start (car str))
                                nl))
                         (right (substring/cursors
                                 (car str)
                                 (string-cursor-next (car str) nl)
                                 end))
                         (res (string-concatenate
                               (reverse (cons left (cdr str))))))
                    (set! str (if (equal? right "") '() (list right)))
                    res))
                 (else
                  (set! scanned? #t)
                  (gen)))))
          (let ((res (source)))
            (cond
             ((eof-object? res)
              res)
             ((equal? res "")
              (gen))
             (else
              (set! str (cons res str))
              (set! scanned? #f)
              (gen))))))
    gen))

(define-record-type Column
  (make-column format generate infinite?)
  column?
  (format column-format)
  (generate column-generate)
  (infinite? column-infinite?))

;; (show-columns (fmt gen [infinite?]) ...)
(define (show-columns . ls)
  (fn ()
    (let* ((cols (map (lambda (x)
                        (make-column (or (car x) displayed)
                                     (displayed (cadr x))
                                     (and (pair? (cddr x)) (car (cddr x)))))
                      ls))
           (num-infinite (count column-infinite? cols)))
      (call-with-output-generators
       (map column-generate cols)
       (lambda (gens)
         (let ((gens (map string->line-generator gens)))
           (let lp ()
             (let* ((lines (map (lambda (gen) (gen)) gens))
                    (num-present (count string? lines)))
               (if (<= num-present num-infinite)
                   nothing
                   (each
                    (each-in-list
                     (map (lambda (col line)
                            ((column-format col)
                             (if (eof-object? line) "" line)))
                          cols
                          lines))
                    "\n"
                    (fn () (lp))))))))))))

;; (columnar ['infinite|'right|'left|'center|width] string-or-formatter ...)
(define (columnar . ls)
  (define (proportional-width? w)
    (and (number? w)
         (or (< 0 w 1)
             (and (inexact? w) (= w 1.0)))))
  (define (build-column ls)
    (let-optionals* ls ((fixed-width #f)
                        (col-width #f)
                        (last? #t)
                        (tail '())
                        (gen #f)
                        (prefix '())
                        (align 'left)
                        (infinite? #f))
      (define (scale-width width)
        (max 1 (exact (truncate (* col-width (- width fixed-width))))))
      (define (padder)
        (if (proportional-width? col-width)
            (case align
              ((right)
               (lambda (str) (fn (width) (padded/left (scale-width width) str))))
              ((center)
               (lambda (str) (fn (width) (padded/both (scale-width width) str))))
              (else
               (lambda (str) (fn (width) (padded/right (scale-width width) str)))))
            (case align
              ((right) (lambda (str) (padded/left col-width str)))
              ((center) (lambda (str) (padded/both col-width str)))
              (else (lambda (str) (padded/right col-width str))))))
      (define (affix x)
        (cond
         ((pair? tail)
          (lambda (str)
            (each (each-in-list prefix)
                  (x str)
                  (each-in-list tail))))
         ((pair? prefix)
          (lambda (str) (each (each-in-list prefix) (x str))))
         (else (displayed x))))
      (list
       ;; line formatter
       (affix
        (let ((pad (padder)))
          (if (and last? (not (pair? tail)) (eq? align 'left))
              (lambda (str)
                (fn (pad-char)
                  ((if (or (not pad-char) (char-whitespace? pad-char))
                       displayed
                       pad)
                   str)))
              pad)))
       ;; generator
       (if (proportional-width? col-width)
           (fn (width)
             (with ((width (scale-width width)))
               gen))
           (with ((width col-width)) gen))
       infinite?)))
  (define (adjust-widths ls border-width)
    (let* ((fixed-ls
            (filter (lambda (x) (and (number? (car x)) (>= (car x) 1))) ls))
           (fixed-total (fold + border-width (map car fixed-ls)))
           (scaled-ls (filter (lambda (x) (proportional-width? (car x))) ls))
           (denom (- (length ls) (+ (length fixed-ls) (length scaled-ls))))
           (rest (if (zero? denom)
                     0
                     (inexact
                      (/ (- 1 (fold + 0 (map car scaled-ls))) denom)))))
      (if (negative? rest)
          (error "fractional widths must sum to less than 1"
                 (map car scaled-ls)))
      (map
       (lambda (col)
         (cons fixed-total
               (if (not (number? (car col)))
                   (cons rest (cdr col))
                   col)))
       ls)))
  (define (finish ls border-width)
    (apply show-columns
           (map build-column (adjust-widths (reverse ls) border-width))))
  (let lp ((ls ls) (strs '()) (align 'left) (infinite? #f)
           (width #t) (border-width 0) (res '()))
    (cond
     ((null? ls)
      (if (pair? strs)
          (finish (cons (cons (caar res)
                              (cons #t (cons (append (reverse strs)
                                                     (cadr (cdar res)))
                                             (cddr (cdar res)))))
                        (cdr res))
                  border-width)
          (finish (cons (cons (caar res) (cons #t (cddr (car res)))) (cdr res))
                  border-width)))
     ((char? (car ls))
      (lp (cons (string (car ls)) (cdr ls)) strs align infinite?
          width border-width res))
     ((string? (car ls))
      (if (string-contains "\n" (car ls))
          (error "column string literals can't contain newlines")
          (lp (cdr ls) (cons (car ls) strs) align infinite?
              width (+ border-width (string-length (car ls))) res)))
     ((number? (car ls))
      (lp (cdr ls) strs align infinite? (car ls) border-width res))
     ((eq? (car ls) 'infinite)
      (lp (cdr ls) strs align #t width border-width res))
     ((symbol? (car ls))
      (lp (cdr ls) strs (car ls) infinite? width border-width res))
     ((procedure? (car ls))
      (lp (cdr ls) '() 'left #f #t border-width
          (cons (list width #f '() (car ls) (reverse strs) align infinite?)
                res)))
     (else
      (error "invalid column" (car ls))))))

(define (max-line-width string-width str)
  (let ((end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)) (hi 0))
      (let ((j (string-index str #\newline i)))
        (if (string-cursor<? j end)
            (lp (string-cursor-next str j)
                (max hi (string-width (substring/cursors str i j))))
            (max hi (string-width (substring/cursors str i end))))))))

(define (pad-finite proc width string-width k)
  (call-with-output
   proc
   (lambda (str)
     (let ((w (max-line-width (or string-width string-length) str)))
       (k (displayed str)
          (if (and (integer? width) (exact? width))
              (max width w)
              w))))))

(define (tabular . ls)
  (fn (string-width)
    (let lp ((ls ls) (infinite? #f) (width #t) (res '()))
      (cond
       ((null? ls)
        (apply columnar (reverse res)))
       ((number? (car ls))
        (lp (cdr ls) infinite? (car ls) res))
       ((eq? 'infinite (car ls))
        (lp (cdr ls) #t width (cons (car ls) res)))
       ((procedure? (car ls))
        (if infinite?
            (if width
                (lp (cdr ls) #f #t (cons (car ls) (cons width res)))
                (lp (cdr ls) #f #t (cons (car ls) res)))
            (pad-finite (car ls) width string-width
                        (lambda (gen width)
                          (lp (cdr ls) #f #t (cons gen (cons width res)))))))
       (else
        (lp (cdr ls) infinite? width (cons (car ls) res)))))))

;; break lines only, don't join short lines or justify
(define (wrapped/char . ls)
  (fn (output width string-width)
    (define (kons-in-line str)
      (fn (col)
        (let ((len ((or string-width string-length) str))
              (space (- width col)))
          (cond
           ((equal? "" str)
            nothing)
           ((or (<= len space) (not (positive? space)))
            (each (output str) (output "\n")))
           (else
            (each
             ;; TODO: when splitting by string-width, substring needs
             ;; to be provided
             (output (substring str 0 space))
             (output "\n")
             (fn () (kons-in-line (substring str space len)))))))))
    (with ((output
            (lambda (str)
              (let ((end (string-cursor-end str)))
                (let lp ((i (string-cursor-start str)))
                  (let ((nli (string-index str #\newline i)))
                    (cond
                     ((string-cursor>=? i end)
                      nothing)
                     ((string-cursor>=? nli end)
                      (kons-in-line (substring/cursors str i end)))
                     (else
                      (each
                       (fn () (kons-in-line (substring/cursors str i nli)))
                       (fn () (lp (string-cursor-next str nli))))))))))))
      (each-in-list ls))))

;; `seq' is a list or vector of pre-tokenized words.  `line' is called
;; on each wrapped line and the accumulator, starting with `knil'.
;; The optional `last-line' is used instead on the last line of the
;; paragraph.
(define (wrap-fold-words seq knil max-width get-width line . o)
  (let* ((last-line (if (pair? o) (car o) line))
         (vec (if (list? seq) (list->vector seq) seq))
         (len (vector-length vec))
         (len-1 (- len 1))
         (breaks (make-vector len #f))
         (penalties (make-vector len #f))
         (widths
          (list->vector
           (map get-width (if (list? seq) seq (vector->list vec))))))
    (define (largest-fit i)
      (let lp ((j (+ i 1)) (width (vector-ref widths i)))
        (let ((width (+ width 1 (vector-ref widths j))))
          (cond
            ((>= width max-width) (- j 1))
            ((>= j len-1) len-1)
            (else (lp (+ j 1) width))))))
    (define (min-penalty! i)
      (cond
        ((>= i len-1) 0)
        ((vector-ref penalties i))
        (else
         (vector-set! penalties i (expt (+ max-width 1) 3))
         (vector-set! breaks i i)
         (let ((k (largest-fit i)))
           (let lp ((j i) (width 0))
             (if (<= j k)
                 (let* ((width (+ width (vector-ref widths j)))
                        (break-penalty
                         (+ (max 0 (expt (- max-width (+ width (- j i))) 3))
                            (min-penalty! (+ j 1)))))
                   (cond
                     ((< break-penalty (vector-ref penalties i))
                      (vector-set! breaks i j)
                      (vector-set! penalties i break-penalty)))
                   (lp (+ j 1) width)))))
         (if (>= (vector-ref breaks i) len-1)
             (vector-set! penalties i 0))
         (vector-ref penalties i))))
    (define (sub-list i j)
      (let lp ((i i) (res '()))
        (if (> i j)
            (reverse res)
            (lp (+ i 1) (cons (vector-ref vec i) res)))))
    (cond
     ((zero? len)
      ;; degenerate case
      (last-line '() knil))
     (else
      ;; compute optimum breaks
      (vector-set! breaks len-1 len-1)
      (vector-set! penalties len-1 0)
      (min-penalty! 0)
      ;; fold
      (let lp ((i 0) (acc knil))
        (let ((break (vector-ref breaks i)))
          (if (>= break len-1)
              (last-line (sub-list i len-1) acc)
              (lp (+ break 1) (line (sub-list i break) acc)))))))))

(define (wrapped/list ls)
  (fn (width string-width pad-char)
    (joined/suffix
     (lambda (ls) (joined displayed ls pad-char))
     (reverse
      (wrap-fold-words ls '() width (or string-width string-length) cons))
     "\n")))

(define (wrapped . ls)
  (call-with-output
   (each-in-list ls)
   (lambda (str)
     (fn (word-separator?)
       (wrapped/list
        (string-split-words str (or word-separator? char-whitespace?)))))))

(define (justified . ls)
  (fn (output width string-width)
    (define (justify-line ls)
      (if (null? ls)
          nl
          (let* ((sum (fold (lambda (s n)
                              (+ n ((or string-width string-length) s)))
                            0 ls))
                 (len (length ls))
                 (diff (max 0 (- width sum)))
                 (sep (make-string (if (= len 1)
                                       0
                                       (quotient diff (- len 1)))
                                   #\space))
                 (rem (if (= len 1)
                          diff
                          (remainder diff (- len 1))))
                 (p (open-output-string)))
            (display (car ls) p)
            (let lp ((ls (cdr ls)) (i 1))
              (when (pair? ls)
                (display sep p)
                (if (<= i rem) (write-char #\space p))
                (display (car ls) p)
                (lp (cdr ls) (+ i 1))))
            (displayed (get-output-string p)))))
    (define (justify-last ls)
      (each (joined displayed ls " ") "\n"))
    (call-with-output
     (each-in-list ls)
     (lambda (str)
       (fn (word-separator?)
         (joined/last
          justify-line
          justify-last
          (reverse
           (wrap-fold-words
            (string-split-words str (or word-separator? char-whitespace?))
            '() width (or string-width string-length)
            cons))
          "\n"))))))

(define (from-file path . ls)
  (let-optionals* ls ((sep nl))
    (fn ()
      (let ((in (open-input-file path)))
        (let lp ()
          (let ((line (read-line in)))
            (if (eof-object? line)
                (begin (close-input-port in) nothing)
                (each line sep
                      (fn () (lp))))))))))

(define (line-numbers . o)
  (let ((start (if (pair? o) (car o) 1)))
    (joined/range displayed start #f "\n")))
