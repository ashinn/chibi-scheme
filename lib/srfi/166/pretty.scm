;; pretty.scm -- pretty printing format combinator
;; Copyright (c) 2006-2020 Alex Shinn.  All rights reserved.
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

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (let ((res (get-output-string out)))
      (close-output-port out)
      res)))

(define (joined/shares fmt ls shares . o)
  (let ((sep (displayed (if (pair? o) (car o) " "))))
    (fn ()
      (cond
       ((null? ls)
        nothing)
       ((pair? ls)
        (fn ()
          (let lp ((ls ls))
            (each
             (fmt (car ls))
             (let ((rest (cdr ls)))
               (cond
                ((null? rest) nothing)
                ((pair? rest)
                 (call-with-shared-ref/cdr rest
                                           shares
                                           each
                                           (fn () (lp rest))
                                           sep))
                (else (each sep ". " (fmt rest)))))))))
       (else (fmt ls))))))

(define (string-find/index str pred i)
  (string-cursor->index
   str
   (string-index str pred (string-index->cursor str i))))

(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

(define (try-fitted2 proc fail)
  (fn (width string-width (orig-output output))
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
                     (len (string-width str)))
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
          (fn () (orig-output (get-output-string out)))))))))

(define (try-fitted proc . fail)
  (let lp ((proc proc) (ls fail))
    (if (null? ls)
        proc
        (try-fitted2 proc (lp (car ls) (cdr ls))))))

(define (fits-in-width width proc set-failed!)
  (call-with-current-continuation
   (lambda (abort)
     (fn ((orig-output output))
       (define (output* str)
         (each (orig-output str)
               (fn (col)
                 (if (>= col width)
                     (begin (set-failed! #t) (abort #f))
                     nothing))))
       (with ((output output*))
         proc)))))

(define (fits-in-columns width ls writer set-result!)
  (let ((max-w (quotient width 2)))
    (fn (string-width)
      (let lp ((ls ls) (res '()) (widest 0))
        (cond
         ((pair? ls)
          (let ((failed? #f))
            (call-with-output
             (fits-in-width max-w
                            (writer (car ls))
                            (lambda (x) (set! failed? x)))
             (lambda (str)
               (if failed?
                   (begin
                     (set-result! #f)
                     nothing)
                   (lp (cdr ls)
                       (cons str res)
                       (max (string-width str) widest)))))))
         ((null? ls) (set-result! (cons widest (reverse res))) nothing)
         (else (set-result! #f) nothing))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; style

(define syntax-abbrevs
  '((quote . "'") (quasiquote . "`")
    (unquote . ",") (unquote-splicing . ",@")
    ))

(define (pp-let ls pp shares color?)
  (if (and (pair? (cdr ls)) (symbol? (cadr ls)))
      (pp-with-indent 2 ls pp shares color?)
      (pp-with-indent 1 ls pp shares color?)))

(define indent-rules
  `((lambda . 1) (define . 1) (define-syntax . 1)
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

(define pp-macros
  (append
   (map car indent-rules)
   '(quote quasiquote unquote unquote-splicing set! cond-expand cond )))

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
        (when (> new-count orig-count)
          (hash-table-walk
           (car shares)
           (lambda (k v)
             (if (and (cdr v) (>= (car v) orig-count))
                 (set-cdr! v #f))))
          (set-cdr! shares orig-count))
        proc))))

(define (pp-with-indent indent-rule ls pp shares color?)
  (fn ((col1 col))
    (each
     "("
     ((if (and color? (memq (car ls) pp-macros)) as-blue displayed)
      (pp (car ls)))
     (fn ((col2 col) width string-width)
       (let ((fixed (take* (cdr ls) (or indent-rule 1)))
             (tail (drop* (cdr ls) (or indent-rule 1)))
             (default
               (let ((sep (make-nl-space (+ col1 1))))
                 (fn () (each sep (joined/shares pp (cdr ls) shares sep)))))
             ;; reset in case we don't fit on the first line
             (reset-shares (with-reset-shares shares nothing)))
         (call-with-output
          (trimmed/lazy (- width col2)
                        (each (if (or (null? fixed) (pair? fixed)) " " " . ")
                              (joined/shares
                               (lambda (x) (pp-flat x pp shares color?))
                               fixed shares " ")))
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

(define (pp-app ls pp shares color?)
  (let ((indent-rule (pp-indentation ls)))
    (if (procedure? indent-rule)
        (indent-rule ls pp shares color?)
        (pp-with-indent indent-rule ls pp shares color?))))

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
        ((and (pair? (cdr ls)) (pair? (cddr ls)) (pair? (cdr (cddr ls))))
         (let ((out (open-output-string))
               (result #f))
           (call-with-output
            (fits-in-columns width ls (lambda (x) (pp-flat x pp shares #f))
                             (lambda (res) (set! result res)))
            (lambda (str)
              (fn ()
                (if (not result)
                    ;; no room, print one per line
                    (joined/shares pp ls shares (make-nl-space col))
                    ;; at least four elements which can be broken into columns
                    (let* ((prefix (make-nl-space col))
                           (widest (+ 1 (car result)))
                           (columns (quotient width widest))) ; always >= 2
                      (let lp ((ls (cdr result)) (i 1))
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
                                  (lp (cdr ls) (+ i 1))))))))))))))
        (else
         ;; no room, print one per line
         (joined/shares pp ls shares (make-nl-space col))))))
   ")"))

(define (pp-flat x pp shares color?)
  (define (ppf x)
    (pp-flat x pp shares color?))
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
                  each
                  (pp-flat (cadr x) pp shares color?)))))
     (else
      (fn ()
        (each "("
              ((if (and color? (memq (car x) pp-macros)) as-blue displayed)
               (pp (car x)))
              (if (null? (cdr x))
                  nothing
                  (call-with-shared-ref/cdr
                   (cdr x)
                   shares
                   each
                   (cond
                    ((pair? (cdr x))
                     (each "" (joined/shares ppf (cdr x) shares " ")))
                    (else
                     (each ". " (joined/shares ppf (cdr x) shares " "))))
                   " "))
              ")")))))
   ((vector? x)
    (each "#("
          (joined/shares ppf (vector->list x) shares " ")
          ")"))
   (else
    (pp x))))

(define (pp-pair ls pp shares color?)
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
    (let ((reset-shares (with-reset-shares shares nothing)))
      (try-fitted
       (pp-flat ls pp shares color?)
       (each
        reset-shares
        (fn ()
          (if (and (non-app? ls)
                   (proper-non-shared-list? ls shares))
              (pp-data-list ls pp shares)
              (pp-app ls pp shares color?)))))))))

(define (pp-vector vec pp shares)
  (each "#" (pp-data-list (vector->list vec) pp shares)))

;; adapted from `write-with-shares'
(define (pp obj shares color?)
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
         obj shares each
         (fn ()
           (cond
            ((pair? obj)
             (pp-pair obj pp shares color?))
            ((vector? obj)
             (pp-vector obj pp shares))
            ((number? obj)
             (write-number obj))
            ((and color? (string? obj))
             (as-green (write-to-string obj)))
            (else
             (displayed (write-to-string obj))))))))))

(define (pretty obj)
  (fn ()
    (call-with-output
     (each (pp obj (extract-shared-objects obj #t) #f)
           fl)
     displayed)))

(define (pretty-shared obj)
  (fn ()
    (call-with-output
     (each (pp obj (extract-shared-objects obj #f) #f)
           fl)
     displayed)))

(define (pretty-simply obj)
  (fn ()
    (each (pp obj (extract-shared-objects #f #f) #f)
          fl)))

(define (pretty-with-color obj)
  (fn ()
    (call-with-output
     (each (pp obj (extract-shared-objects obj #t) #t)
           fl)
     displayed)))
