;; pretty.scm -- pretty printing format combinator
;; Copyright (c) 2006-2018 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define (take* ls n)   ; handles dotted lists and n > length
  (cond ((zero? n) '())
        ((pair? ls) (cons (car ls) (take* (cdr ls) (- n 1))))
        (else '())))

(define (drop* ls n)   ; may return the dot
  (cond ((zero? n) ls)
        ((pair? ls) (drop* (cdr ls) (- n 1)))
        (else ls)))

(define (make-space n) (make-string n #\space))
(define (make-nl-space n) (string-append "\n" (make-string n #\space)))

(define (joined/shares fmt ls shares . o)
  (let ((sep (displayed (if (pair? o) (car o) " "))))
    (fn ()
      (if (null? ls)
          nothing
          (let lp ((ls ls))
            (each
             (fmt (car ls))
             (let ((rest (cdr ls)))
               (cond
                ((null? rest) nothing)
                ((pair? rest)
                 (call-with-shared-ref/cdr rest
                                           shares
                                           (fn () (lp rest))
                                           sep))
                (else (each sep ". " (fmt rest)))))))))))

(define (string-find/index str pred i)
  (string-cursor->index
   str
   (string-find str pred (string-index->cursor str i))))

(define (try-fitted2 proc fail)
  (fn (width output)
    (let ((out (open-output-string)))
      (call-with-current-continuation
       (lambda (abort)
         ;; Modify output to accumulate to an output string port,
         ;; and escape immediately with failure if we exceed the
         ;; column width.
         (define (output* str)
           (fn (col)
             (let lp ((i 0) (col col))
               (let ((nli (string-find/index str #\newline i))
                     (len (string-length str)))
                 (if (< nli len)
                     (if (> (+ (- nli i) col) width)
                         (abort fail)
                         (lp (+ nli 1) 0))
                     (let ((col (+ (- len i) col)))
                       (cond
                        ((> col width)
                         (abort fail))
                        (else
                         (output-default str)))))))))
         (forked
          (with ((output output*)
                 (port out))
            proc)
          ;; fitted successfully
          (output (get-output-string out))))))))

(define (try-fitted proc . fail)
  (if (null? fail)
      proc
      (try-fitted2 proc (apply try-fitted fail))))

(define (fits-in-width width proc)
  (call-with-current-continuation
   (lambda (abort)
     (show
      #f
      (fn (output)
        (define (output* str)
          (each (output str)
                (fn (col)
                  (if (>= col width)
                      (abort #f)
                      nothing))))
        (with ((output output*))
          proc))))))

(define (fits-in-columns width ls writer)
  (let ((max-w (quotient width 2)))
    (let lp ((ls ls) (res '()) (widest 0))
      (cond
       ((pair? ls)
        (let ((str (fits-in-width max-w (writer (car ls)))))
          (and str
               (lp (cdr ls)
                   (cons str res)
                   (max (string-length str) widest)))))
       ((null? ls) (cons widest (reverse res)))
       (else #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; style

(define syntax-abbrevs
  '((quote . "'") (quasiquote . "`")
    (unquote . ",") (unquote-splicing . ",@")
    ))

(define (pp-let ls pp shares)
  (if (and (pair? (cdr ls)) (symbol? (cadr ls)))
      (pp-with-indent 2 ls pp shares)
      (pp-with-indent 1 ls pp shares)))

(define indent-rules
  `((lambda . 1) (define . 1)
    (let . ,pp-let) (loop . ,pp-let)
    (let* . 1) (letrec . 1) (letrec* . 1) (and-let* . 1) (let1 . 2)
    (let-values . 1) (let*-values . 1) (receive . 2) (parameterize . 1)
    (let-syntax . 1) (letrec-syntax . 1) (syntax-rules . 1) (syntax-case . 2)
    (match . 1) (match-let . 1) (match-let* . 1)
    (if . 3) (when . 1) (unless . 1) (case . 1) (while . 1) (until . 1)
    (do . 2) (dotimes . 1) (dolist . 1) (test . 1)
    (condition-case . 1) (guard . 1) (rec . 1)
    (call-with-current-continuation . 0)
    ))

(define indent-prefix-rules
  `(("with-" . -1) ("call-with-" . -1) ("define-" . 1))
  )

(define indent-suffix-rules
  `(("-case" . 1))
  )

(define (pp-indentation form)
  (let ((indent
         (cond
          ((assq (car form) indent-rules) => cdr)
          ((and (symbol? (car form))
                (let ((str (symbol->string (car form))))
                  (or (find (lambda (rx) (string-prefix? (car rx) str))
                            indent-prefix-rules)
                      (find (lambda (rx) (string-suffix? (car rx) str))
                            indent-suffix-rules))))
           => cdr)
          (else #f))))
    (if (and (number? indent) (negative? indent))
        (max 0 (- (+ (or (length+ form) +inf.0) indent) 1))
        indent)))

(define (with-reset-shares shares proc)
  (let ((orig-count (cdr shares)))
    (fn ()
      (let ((new-count (cdr shares)))
        (cond
         ((> new-count orig-count)
          (hash-table-walk
           (car shares)
           (lambda (k v)
             (if (and (cdr v) (>= (car v) orig-count))
                 (set-cdr! v #f))))
          (set-cdr! shares orig-count)))
        proc))))

(define (pp-with-indent indent-rule ls pp shares)
  (fn ((col1 col))
    (each
     "("
     (pp (car ls))
     (fn ((col2 col) width string-width)
       (let ((fixed (take* (cdr ls) (or indent-rule 1)))
             (tail (drop* (cdr ls) (or indent-rule 1)))
             (default
               (let ((sep (make-nl-space (+ col1 1))))
                 (each sep (joined/shares pp (cdr ls) shares sep))))
             ;; reset in case we don't fit on the first line
             (reset-shares (with-reset-shares shares nothing)))
         (call-with-output
          (trimmed/lazy (- width col2)
                        (each " "
                              (joined/shares
                               (lambda (x) (pp-flat x pp shares)) fixed shares " "))
                        )
          (lambda (first-line)
            (cond
             ((< (+ col2 (string-width first-line)) width)
              ;; fixed values on first line
              (let ((sep (make-nl-space
                          (if indent-rule (+ col1 2) (+ col2 1)))))
                (each first-line
                      (cond
                       ((not (or (null? tail) (pair? tail)))
                        (each ". " (pp tail)))
                       ((> (or (length+ (cdr ls)) +inf.0) (or indent-rule 1))
                        (each sep (joined/shares pp tail shares sep)))
                       (else
                        nothing)))))
             (indent-rule
              ;; fixed values lined up, body indented two spaces
              (try-fitted
               (each
                reset-shares
                " "
                (joined/shares pp fixed shares (make-nl-space (+ col2 1)))
                (if (pair? tail)
                    (let ((sep (make-nl-space (+ col1 2))))
                      (each sep (joined/shares pp tail shares sep)))
                    nothing))
               (each reset-shares default)))
             (else
              ;; all on separate lines
              (each reset-shares default)))))))
     ")")))

(define (pp-app ls pp shares)
  (let ((indent-rule (pp-indentation ls)))
    (if (procedure? indent-rule)
        (indent-rule ls pp shares)
        (pp-with-indent indent-rule ls pp shares))))

;; the elements may be shared, just checking the top level list
;; structure
(define (proper-non-shared-list? ls shares)
  (let ((tab (car shares)))
    (let lp ((ls ls))
      (or (null? ls)
          (and (pair? ls)
               (not (hash-table-ref/default tab ls #f))
               (lp (cdr ls)))))))

(define (non-app? x)
  (if (pair? x)
      (or (not (or (null? (cdr x)) (pair? (cdr x))))
          (non-app? (car x)))
      (not (symbol? x))))

(define (pp-data-list ls pp shares)
  (each
   "("
   (fn (col width string-width)
     (let ((avail (- width col)))
       (cond
        ((and (pair? (cdr ls)) (pair? (cddr ls)) (pair? (cdr (cddr ls)))
              (fits-in-columns width ls (lambda (x) (pp-flat x pp shares))))
         => (lambda (ls)
              ;; at least four elements which can be broken into columns
              (let* ((prefix (make-nl-space col))
                     (widest (+ 1 (car ls)))
                     (columns (quotient width widest))) ; always >= 2
                (let lp ((ls (cdr ls)) (i 1))
                  (cond
                   ((null? ls)
                    nothing)
                   ((null? (cdr ls))
                    (displayed (car ls)))
                   ((>= i columns)
                    (each (car ls)
                          prefix
                          (fn () (lp (cdr ls) 1))))
                   (else
                    (let ((pad (- widest (string-width (car ls)))))
                      (each (car ls)
                            (make-space pad)
                            (lp (cdr ls) (+ i 1))))))))))
        (else
         ;; no room, print one per line
         (joined/shares pp ls shares (make-nl-space col))))))
   ")"))

(define (pp-flat x pp shares)
  (cond
   ((pair? x)
    (cond
     ((and (pair? (cdr x)) (null? (cddr x))
           (assq (car x) syntax-abbrevs))
      => (lambda (abbrev)
           (each (cdr abbrev)
                 (call-with-shared-ref
                  (cadr x)
                  shares
                  (pp-flat (cadr x) pp shares)))))
     (else
      (each "("
            (joined/shares (lambda (x) (pp-flat x pp shares)) x shares " ")
            ")"))))
   ((vector? x)
    (each "#("
          (joined/shares
           (lambda (x) (pp-flat x pp shares)) (vector->list x) shares " ")
          ")"))
   (else
    (pp x))))

(define (pp-pair ls pp shares)
  (cond
   ;; one element list, no lines to break
   ((null? (cdr ls))
    (each "(" (pp (car ls)) ")"))
   ;; quote or other abbrev
   ((and (pair? (cdr ls)) (null? (cddr ls))
         (assq (car ls) syntax-abbrevs))
    => (lambda (abbrev)
         (each (cdr abbrev) (pp (cadr ls)))))
   (else
    (try-fitted
     (fn () (pp-flat ls pp shares))
     (with-reset-shares
      shares
      (fn ()
        (if (and (non-app? ls)
                 (proper-non-shared-list? ls shares))
            (pp-data-list ls pp shares)
            (pp-app ls pp shares))))))))

(define (pp-vector vec pp shares)
  (each "#" (pp-data-list (vector->list vec) pp shares)))

;; adapted from `write-with-shares'
(define (pp obj shares)
  (fn (radix precision)
    (let ((write-number
           (cond
            ((and (not precision)
                  (assv radix '((16 . "#x") (10 . "") (8 . "#o") (2 . "#b"))))
             => (lambda (cell)
                  (lambda (n)
                    (if (or (exact? n) (eqv? radix 10))
                        (each (cdr cell) (number->string n (car cell)))
                        (with ((radix 10)) (numeric n))))))
            (else (lambda (n) (with ((radix 10)) (numeric n)))))))
      (let pp ((obj obj))
        (call-with-shared-ref
         obj shares
         (fn ()
           (cond
            ((pair? obj)
             (pp-pair obj pp shares))
            ((vector? obj)
             (pp-vector obj pp shares))
            ((number? obj)
             (write-number obj))
            (else
             (write-with-shares obj shares)))))))))

(define (pretty obj)
  (fn ()
    (call-with-output
     (each (pp obj (extract-shared-objects obj #t))
           fl)
     displayed)))

(define (pretty-shared obj)
  (fn ()
    (call-with-output
     (each (pp obj (extract-shared-objects obj #f))
           fl)
     displayed)))

(define (pretty-simply obj)
  (fn ()
    (each (pp obj (extract-shared-objects #f #f))
          fl)))
