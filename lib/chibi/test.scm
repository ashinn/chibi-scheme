;; Copyright (c) 2010-2012 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Simple testing framework adapted from the Chicken @scheme{test}
;;> module.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list utilities

;; Simplified version of SRFI-1 every.
(define (every pred ls)
  (or (null? ls)
      (if (null? (cdr ls))
          (pred (car ls))
          (if (pred (car ls)) (every pred (cdr ls)) #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exception utilities

(define (warning msg . args)
  (display msg (current-error-port))
  (for-each (lambda (x)
              (write-char #\space (current-error-port))
              (write x (current-error-port)))
            args)
  (newline (current-error-port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string utilities

(define (string-search pat str)
  (let* ((pat-len (string-length pat))
         (limit (- (string-length str) pat-len)))
    (let lp1 ((i 0))
      (cond
       ((>= i limit) #f)
       (else
        (let lp2 ((j i) (k 0))
          (cond ((>= k pat-len) #t)
                ((not (eqv? (string-ref str j) (string-ref pat k)))
                 (lp1 (+ i 1)))
                (else (lp2 (+ j 1) (+ k 1))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test interface

;;> @subsubsubsection{@scheme{(test [name] expect expr)}}

;;> Evaluate @var{expr} and check that it is @scheme{equal?}
;;> to @var{expect}.  @var{name} is used in reporting, and
;;> defaults to a printed summary of @var{expr}.

(define-syntax test
  (syntax-rules ()
    ((test expect expr)
     (test #f expect expr))
    ((test name expect (expr ...))
     (test-propagate-info name expect (expr ...) ()))
    ((test name (expect ...) expr)
     (test-syntax-error
      'test
      "the test expression should come last: (test <expected> (<expr> ...)) "
      (test name (expect ...) expr)))
    ((test name expect expr)
     (test-propagate-info name expect expr ()))
    ((test a ...)
     (test-syntax-error 'test "test requires 2 or 3 arguments" (test a ...)))))

;;> @subsubsubsection{@scheme{(test-equal equal [name] expect expr)}}

;;> Equivalent to test, using @var{equal} for comparison instead of
;;> @scheme{equal?}.

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal equal . args)
     (parameterize ((current-test-comparator equal))
       (test . args)))))

;;> @subsubsubsection{@scheme{(test-assert [name] expr)}}

;;> Like @scheme{test} but evaluates @var{expr} and checks that it's true.

(define-syntax test-assert
  (syntax-rules ()
    ((_ expr)
    (test-assert #f expr))
    ((_ name expr)
     (test-propagate-info name #f expr ((assertion . #t))))
    ((test a ...)
     (test-syntax-error 'test-assert "1 or 2 arguments required"
                        (test a ...)))))

;;> @subsubsubsection{@scheme{(test-not [name] expr)}}

;;> Like @scheme{test} but evaluates @var{expr} and checks that it's false.

(define-syntax test-not
  (syntax-rules ()
    ((_ expr) (test-assert (not expr)))
    ((_ name expr) (test-assert name (not expr)))))

;;> @subsubsubsection{@scheme{(test-values [name] expect expr)}}

;;> Like @scheme{test} but @var{expect} and @var{expr} can both
;;> return multiple values.

(define-syntax test-values
  (syntax-rules ()
    ((_ expect expr)
    (test-values #f expect expr))
    ((_ name expect expr)
     (test name (call-with-values (lambda () expect) (lambda results results))
       (call-with-values (lambda () expr) (lambda results results))))))

;;> @subsubsubsection{@scheme{(test-error [name] expr)}}

;;> Like @scheme{test} but evaluates @var{expr} and checks that it
;;> raises an error.

(define-syntax test-error
  (syntax-rules ()
    ((_ expr)
     (test-error #f expr))
    ((_ name expr)
     (test-propagate-info name #f expr ((expect-error . #t))))
    ((test a ...)
     (test-syntax-error 'test-error "1 or 2 arguments required"
                        (test a ...)))))

;; TODO: Extract interesting variables so we can show their values on
;; failure.
(define-syntax test-propagate-info
  (syntax-rules ()
    ((test-propagate-info name expect expr info)
     (test-vars () name expect expr info))))

(define-syntax test-vars
  (syntax-rules ()
    ((_ (vars ...) n expect expr ((key . val) ...))
     (test-run (lambda () expect)
               (lambda () expr)
               (cons (cons 'name n)
                     '((source . expr)
                       ;;(var-names . (vars ...))
                       ;;(var-values . ,(list vars))
                       (key . val) ...))))))

;;> @subsubsubsection{@scheme{(test-exit)}}

;;> Exits with a failure status if any tests have failed,
;;> and a successful status otherwise.

(define (test-exit)
  (exit (zero? (test-failure-count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group interface

;;> Wraps @var{body} as a single test group, which can be filtered
;;> and summarized separately.

(define-syntax test-group
  (syntax-rules ()
    ((_ name-expr body ...)
     (let ((name name-expr)
           (old-group (current-test-group)))
       (if (not (string? name))
           (error "a name is required, got " 'name-expr name))
       (test-begin name)
       (guard
        (exn
         (else
           (warning "error in group outside of tests")
           (print-exception e (current-error-port))
           (test-group-inc! (current-test-group) 'count)
           (test-group-inc! (current-test-group) 'ERROR)))
        body ...)
       (test-end name)
       (current-test-group old-group)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define-syntax test-syntax-error
  (syntax-rules ()
    ((_) (syntax-error "invalid use of test-syntax-error"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-group representation

;; (name (prop value) ...)
(define (make-test-group name . o)
  (let ((parent (and (pair? o) (car o)))
        (group (list name (cons 'start-time (current-second)))))
    (test-group-set! group 'parent parent)
    (test-group-set! group 'verbose
                     (if parent
                         (test-group-ref parent 'verbose)
                         (current-test-verbosity)))
    (test-group-set! group 'level
                     (if parent
                         (+ 1 (test-group-ref parent 'level 0))
                         0))
    (test-group-set!
     group
     'skip-group?
     (or (and parent (test-group-ref parent 'skip-group?))
         (not (every (lambda (f) (f group)) (current-test-group-filters)))))
    group))

(define test-group-name car)

(define (test-group-ref group field . o)
  (if group
      (apply assq-ref (cdr group) field o)
      (and (pair? o) (car o))))

(define (test-group-set! group field value)
  (cond
   ((assq field (cdr group))
    => (lambda (x) (set-cdr! x value)))
   (else (set-cdr! group (cons (cons field value) (cdr group))))))

(define (test-group-inc! group field)
  (cond
   ((assq field (cdr group))
    => (lambda (x) (set-cdr! x (+ 1 (cdr x)))))
   (else (set-cdr! group (cons (cons field 1) (cdr group))))))

(define (test-group-push! group field value)
  (cond
   ((assq field (cdr group))
    => (lambda (x) (set-cdr! x (cons value (cdr x)))))
   (else (set-cdr! group (cons (cons field (list value)) (cdr group))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define (assq-ref ls key . o)
  (cond ((assq key ls) => cdr)
        ((pair? o) (car o))
        (else #f)))

(define (approx-equal? a b epsilon)
  (< (abs (- 1 (abs (if (zero? b) (+ 1 a) (/ a b)))))
     epsilon))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

;; partial pretty printing to abbreviate `quote' forms and the like
(define (write-to-string x)
  (call-with-output-string
    (lambda (out)
      (let wr ((x x))
        (if (pair? x)
            (cond
              ((and (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))
                    (assq (car x)
                          '((quote . "'") (quasiquote . "`")
                            (unquote . ",") (unquote-splicing . ",@"))))
               => (lambda (s) (display (cdr s) out) (wr (cadr x))))
              (else
               (display "(" out)
               (wr (car x))
               (let lp ((ls (cdr x)))
                 (cond ((pair? ls)
                        (display " " out)
                        (wr (car ls))
                        (lp (cdr ls)))
                       ((not (null? ls))
                        (display " . " out)
                        (write ls out))))
               (display ")" out)))
            (write x out))))))

;; if we need to truncate, try first dropping let's to get at the
;; heart of the expression
(define (truncate-source x width . o)
  (let* ((str (write-to-string x))
         (len (string-length str)))
    (cond
      ((<= len width)
       str)
      ((and (pair? x) (eq? 'let (car x)))
       (if (and (pair? o) (car o))
           (truncate-source (car (reverse x)) width #t)
           (string-append "..."
                          (truncate-source (car (reverse x)) (- width 3) #t))))
      ((and (pair? x) (eq? 'call-with-current-continuation (car x)))
       (truncate-source (cons 'call/cc (cdr x)) width (and (pair? o) (car o))))
      ((and (pair? x) (eq? 'call-with-values (car x)))
       (string-append
        "..."
        (truncate-source (if (and (pair? (cadr x)) (eq? 'lambda (car (cadr x))))
                             (car (reverse (cadr x)))
                             (cadr x))
                         (- width 3)
                         #t)))
      (else
       (string-append
        (substring str 0 (min (max 0 (- width 3)) (string-length str)))
        "...")))))

(define (test-get-name! info)
  (or
   (assq-ref info 'name)
   (assq-ref info 'gen-name)
   (let ((name
          (cond
            ((assq-ref info 'source)
             => (lambda (src)
                  (truncate-source src (- (current-column-width) 12))))
            ((current-test-group)
             => (lambda (g)
                  (display "no source in: " (current-error-port))
                  (write info (current-error-port))
                  (display "\n" (current-error-port))
                  (string-append
                   "test-"
                   (number->string (test-group-ref g 'count 0)))))
            (else ""))))
     (if (pair? info)
         (set-cdr! info (cons (cons 'gen-name name) (cdr info))))
     name)))

(define (test-print-name info . indent)
  (let ((width (- (current-column-width)
                  (or (and (pair? indent) (car indent)) 0)))
        (name (test-get-name! info)))
    (display name)
    (display " ")
    (let ((diff (- width 9 (string-length name))))
      (cond
       ((positive? diff)
        (display (make-string diff #\.)))))
    (display " ")
    (flush-output-port)))

(define (test-group-indent-width group)
  (let ((level (max 0 (+ 1 (- (test-group-ref group 'level 0)
                              (test-first-indentation))))))
    (* 4 (min level (test-max-indentation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi tools

(define (display-to-string x)
  (if (string? x) x (call-with-output-string (lambda (out) (display x out)))))

(define (ansi-color code)
  (lambda (x)
    (string-append "\x1B;[" (number->string code) "m"
                   (display-to-string x) "\x1B;[0m")))
(define red (ansi-color 31))
(define green (ansi-color 32))
(define yellow (ansi-color 33))
(define bold (ansi-color 1))
(define underline (ansi-color 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-expand-info info)
  (let ((expr (assq-ref info 'source)))
    (if (and (pair? expr)
             (pair-source expr)
             (not (assq-ref info 'line-number)))
        `((file-name . ,(car (pair-source expr)))
          (line-number . ,(cdr (pair-source expr)))
          ,@info)
        info)))

(define (test-run expect expr info)
  (let ((info (test-expand-info info)))
    (if (and (cond ((current-test-group)
                    => (lambda (g) (not (test-group-ref g 'skip-group?))))
                   (else #t))
             (every (lambda (f) (f info)) (current-test-filters)))
        ((current-test-applier) expect expr info)
        ((current-test-skipper) info))))

(define (test-default-applier expect expr info)
  (let* ((group (current-test-group))
         (indent (and group (test-group-indent-width group))))
    (cond
     ((test-group-ref group 'verbose)
      (if (and indent (positive? indent))
          (display (make-string indent #\space)))
      (test-print-name info indent)))
    (let ((expect-val
           (guard
            (exn
             (else
               (warning "bad expect value")
               (print-exception exn (current-error-port))
               #f))
            (expect))))
      (guard
       (exn
        (else
         ((current-test-handler)
          (if (assq-ref info 'expect-error) 'PASS 'ERROR)
          (append `((exception . ,exn)) info))))
       (let ((res (expr)))
         (let ((status
                (if (and (not (assq-ref info 'expect-error))
                         (if (assq-ref info 'assertion)
                             res
                             ((current-test-comparator) expect-val res)))
                    'PASS
                    'FAIL))
               (info `((result . ,res) (expected . ,expect-val) ,@info)))
           ((current-test-handler) status info)))))))

(define (test-default-skipper info)
  ((current-test-handler) 'SKIP info))

(define (test-status-color status)
  (if (test-ansi?)
      (case status
        ((ERROR) (lambda (x) (underline (red x))))
        ((FAIL) red)
        ((SKIP) yellow)
        (else (lambda (x) x)))
      (lambda (x) x)))

(define (test-status-message status)
  ((test-status-color status) status))

(define (test-status-code status)
  ((test-status-color status)
   (case status
     ((ERROR) "!")
     ((FAIL) "x")
     ((SKIP) "-")
     (else "."))))

(define (test-print-explanation indent status info)
  (cond
   ((eq? status 'ERROR)
    (display indent)
    (cond ((assq 'exception info)
           => (lambda (e)
                (print-exception (cdr e) (current-output-port))))))
   ((and (eq? status 'FAIL) (assq-ref info 'assertion))
    (display indent)
    (display "assertion failed\n"))
   ((and (eq? status 'FAIL) (assq-ref info 'expect-error))
    (display indent)
    (display "expected an error but got ")
    (write (assq-ref info 'result)) (newline))
   ((eq? status 'FAIL)
    (display indent)
    (display "expected ") (write (assq-ref info 'expected))
    (display " but got ") (write (assq-ref info 'result)) (newline))))

(define (test-print-source indent status info)
  (case status
    ((FAIL ERROR)
     (cond
      ((assq-ref info 'line-number)
       => (lambda (line)
            (display "    on line ")
            (write line)
            (cond ((assq-ref info 'file-name)
                   => (lambda (file) (display " of file ") (write file))))
            (newline))))
     (cond
      ((assq-ref info 'source)
       => (lambda (s)
            (cond
             ((or (assq-ref info 'name)
                  (> (string-length (write-to-string s))
                     (current-column-width)))
              (display (write-to-string s))
              (newline))))))
     (cond
      ((assq-ref info 'values)
       => (lambda (v)
            (for-each
             (lambda (v)
               (display "    ") (display (car v))
               (display ": ") (write (cdr v)) (newline))
             v)))))))

(define (test-print-failure indent status info)
  ;; display status explanation
  (test-print-explanation indent status info)
  ;; display line, source and values info
  (test-print-source indent status info))

(define (test-print-header-line str . indent)
  (let* ((header (string-append
                  (make-string (if (pair? indent) (car indent) 0) #\space)
                  "-- " str " "))
         (len (string-length header)))
      (display (if (test-ansi?) (bold header) header))
      (display (make-string (max 0 (- (current-column-width) len)) #\-))
      (newline)))

(define (test-default-handler status info)
  (define indent
    (make-string
     (+ 4 (cond ((current-test-group)
                 => (lambda (group) (or (test-group-indent-width group) 0)))
                (else 0)))
     #\space))
  ;; update group info
  (cond ((current-test-group)
         => (lambda (group)
              (if (not (eq? 'SKIP status))
                  (test-group-inc! group 'count))
              (test-group-inc! group status))))
  (if (and (current-test-group)
           (zero?
            (modulo
             (+ (string-length (test-group-name (current-test-group)))
                (or (test-group-ref (current-test-group) 'count) 0)
                1)
             (current-column-width))))
      (display (string-append "\n" (string-copy indent 4))))
  ;; update global failure count for exit status
  (cond
   ((or (eq? status 'FAIL) (eq? status 'ERROR))
    (test-failure-count (+ 1 (test-failure-count)))))
  (cond
   ((eq? status 'SKIP))
   ((test-group-ref (current-test-group) 'verbose)
    ;; display status
    (display "[")
    (if (not (eq? status 'ERROR)) (display " ")) ; pad
    (display (test-status-message status))
    (display "]")
    (newline)
    (test-print-failure indent status info))
   (else
    (display (test-status-code status))
    (cond
     ((and (memq status '(FAIL ERROR)) (current-test-group))
      => (lambda (group)
           (test-group-push! group 'failures (list indent status info)))))))
  (flush-output-port)
  status)

(define (test-default-group-reporter group)
  (define (plural word n)
    (if (= n 1) word (string-append word "s")))
  (define (percent n d)
    (string-append " (" (number->string (/ (round (* 1000.0 (/ n d))) 10)) "%)"))
  (let* ((end-time (current-second))
         (start-time (test-group-ref group 'start-time))
         (duration (- end-time start-time))
         (count (or (test-group-ref group 'count) 0))
         (pass (or (test-group-ref group 'PASS) 0))
         (fail (or (test-group-ref group 'FAIL) 0))
         (err (or (test-group-ref group 'ERROR) 0))
         (skip (or (test-group-ref group 'SKIP) 0))
         (subgroups-count (or (test-group-ref group 'subgroups-count) 0))
         (subgroups-pass (or (test-group-ref group 'subgroups-pass) 0))
         (indent (make-string (or (test-group-indent-width group) 0) #\space)))
    (if (not (test-group-ref group 'verbose))
        (newline))
    (cond
     ((or (positive? count) (positive? subgroups-count))
      (if (not (= count (+ pass fail err)))
          (warning "inconsistent count:" count pass fail err))
      (display indent)
      (cond
       ((positive? count)
        (write count) (display (plural " test" count))))
      (if (and (positive? count) (positive? subgroups-count))
          (display " and "))
      (cond
       ((positive? subgroups-count)
        (write subgroups-count)
        (display (plural " subgroup" subgroups-count))))
      (display " completed in ") (write duration) (display " seconds")
      (cond
       ((not (zero? skip))
        (display " (") (write skip) (display (plural " test" skip))
        (display " skipped)")))
      (display ".") (newline)
      (cond ((positive? fail)
             (display indent)
             (display
              ((if (test-ansi?) red (lambda (x) x))
               (string-append
                (number->string fail) (plural " failure" fail)
                (percent fail count) ".")))
             (newline)))
      (cond ((positive? err)
             (display indent)
             (display
              ((if (test-ansi?) (lambda (x) (underline (red x))) (lambda (x) x))
               (string-append
                (number->string err) (plural " error" err)
                (percent err count) ".")))
             (newline)))
      (cond
       ((not (test-group-ref group 'verbose))
        (for-each
         (lambda (failure)
           (display indent)
           (display ((if (test-ansi?) red (lambda (x) x))
                     (string-append (display-to-string (cadr failure)) ": ")))
           (display (test-get-name! (car (cddr failure))))
           (newline)
           (apply test-print-failure failure))
         (reverse (or (test-group-ref group 'failures) '())))))
      (cond
       ((positive? count)
        (display indent)
        (display
         ((if (and (test-ansi?) (= pass count)) green (lambda (x) x))
          (string-append
           (number->string pass) " out of " (number->string count)
           (percent pass count) (plural " test" pass) " passed.")))
        (newline)))
      (cond
       ((positive? subgroups-count)
        (display indent)
        (display
         ((if (and (test-ansi?) (= subgroups-pass subgroups-count))
              green (lambda (x) x))
          (string-append
           (number->string subgroups-pass) " out of "
           (number->string subgroups-count)
           (percent subgroups-pass subgroups-count)
           (plural " subgroup" subgroups-pass) " passed.")))
        (newline)))))
    (cond
     ((test-group-ref group 'verbose)
      (test-print-header-line
       (string-append "done testing " (or (test-group-name group) ""))
       (or (test-group-indent-width group) 0))
      (newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-equal? expect res)
  (or (equal? expect res)
      (if (real? expect)
          (and (inexact? expect)
               (real? res)
               (approx-equal? expect res (current-test-epsilon)))
          (and (complex? res)
               (test-equal? (real-part expect) (real-part res))
               (test-equal? (imag-part expect) (imag-part res))))))

;;> Begin testing a new group until the closing @scheme{(test-end)}.

(define (test-begin . o)
  (let* ((name (if (pair? o) (car o) ""))
         (parent (current-test-group))
         (group (make-test-group name parent)))
    (cond
     ((and parent
           ;; (zero? (test-group-ref parent 'count 0))
           (zero? (test-group-ref parent 'subgroups-count 0)))
      (newline)))
    (cond
     ((test-group-ref group 'verbose)
      (test-print-header-line
       (string-append "testing " name)
       (or (test-group-indent-width group) 0)))
     (else
      (display
       (make-string (or (test-group-indent-width group) 0)
                    #\space))
      (display (bold (string-append name ": ")))))
    (current-test-group group)))

;;> Ends testing group introduced with @scheme{(test-begin)}, and
;;> summarizes the results.

(define (test-end . o)
  (cond
   ((current-test-group)
    => (lambda (group)
         (if (and (pair? o) (not (equal? (car o) (test-group-name group))))
             (warning "mismatched test-end:" (car o) (test-group-name group)))
         (let ((parent (test-group-ref group 'parent)))
           (cond
            ((not (test-group-ref group 'skip-group?))
             ;; only report if there's something to say
             ((current-test-group-reporter) group)
             (cond
              (parent
               (test-group-inc! parent 'subgroups-count)
               (cond
                ((and (zero? (test-group-ref group 'FAIL 0))
                      (zero? (test-group-ref group 'ERROR 0))
                      (= (test-group-ref group 'subgroups-pass 0)
                         (test-group-ref group 'subgroups-count 0)))
                 (test-group-inc! parent 'subgroups-pass)))))))
           (current-test-group parent)
           group)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(define current-test-group (make-parameter #f))
(define current-test-verbosity
  (make-parameter
   (cond ((get-environment-variable "TEST_VERBOSE")
          => (lambda (s) (not (member s "" "0"))))
         (else #f))))
(define current-test-epsilon (make-parameter 1e-5))
(define current-test-comparator (make-parameter test-equal?))
(define current-test-applier (make-parameter test-default-applier))
(define current-test-handler (make-parameter test-default-handler))
(define current-test-skipper (make-parameter test-default-skipper))
(define current-test-group-reporter
  (make-parameter test-default-group-reporter))
(define test-failure-count (make-parameter 0))

(define test-first-indentation
  (make-parameter
   (or (cond ((get-environment-variable "TEST_FIRST_INDENTATION")
              => string->number)
             (else #f))
       1)))

(define test-max-indentation
  (make-parameter
   (or (cond ((get-environment-variable "TEST_MAX_INDENTATION")
              => string->number)
             (else #f))
       5)))

(define (string->info-matcher str)
  (lambda (info)
    (cond ((test-get-name! info)
           => (lambda (n) (string-search str n)))
          (else #f))))

(define (string->group-matcher str)
  (lambda (group) (string-search str (car group))))

(define (getenv-filter-list proc name . o)
  (cond
    ((get-environment-variable name)
     => (lambda (s)
          (guard
           (exn
            (else
             (warning
              (string-append "invalid filter '" s
                             "' from environment variable: " name))
             (print-exception exn (current-error-port))
             '()))
           (let ((f (proc s)))
             (list (if (and (pair? o) (car o))
                       (lambda (x) (not (f x)))
                       f))))))
    (else '())))

(define current-test-filters
  (make-parameter
   (append (getenv-filter-list string->info-matcher "TEST_FILTER")
           (getenv-filter-list string->info-matcher "TEST_REMOVE" #t))))

(define current-test-group-filters
  (make-parameter
   (append (getenv-filter-list string->group-matcher "TEST_GROUP_FILTER")
           (getenv-filter-list string->group-matcher "TEST_GROUP_REMOVE" #t))))

(define current-column-width
  (make-parameter
   (or (cond ((get-environment-variable "TEST_COLUMN_WIDTH")
              => string->number)
             (else #f))
       78)))

(define test-ansi?
  (make-parameter
   (cond
    ((get-environment-variable "TEST_USE_ANSI")
     => (lambda (s) (not (equal? s "0"))))
    (else
     (member (get-environment-variable "TERM")
             '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
               "linux" "screen" "screen-256color" "vt100"))))))
