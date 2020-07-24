;; Copyright (c) 2010-2020 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Simple but extensible testing framework with advanced reporting.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list utilities

;; Simplified version of SRFI-1 any.
(define (any pred ls)
  (and (pair? ls)
       (or (pred (car ls))
           (any pred (cdr ls)))))

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

;;> \section{Testing}

;;> \macro{(test [name] expect expr)}

;;> The primary interface to testing.  Evaluate \var{expr} and check
;;> that it is equal to \var{expect}, and report the result, using
;;> \var{name} or a printed summary of \var{expr}.
;;>
;;> If used inside a group this will contribute to the overall group
;;> reporting, but can be used standalone:
;;>
;;> \example{(test 4 (+ 2 2))}
;;> \example{(test "add two and two" 4 (+ 2 2))}
;;> \example{(test 3 (+ 2 2))}
;;> \example{(test 4 (+ 2 "2"))}
;;>
;;> The equality comparison is made with
;;> \scheme{current-test-comparator}, defaulting to
;;> \scheme{test-equal?}, which is the same as \scheme{equal?} but
;;> more permissive on floating point comparisons).  Returns the
;;> status of the test (one of the symbols \scheme{'PASS},
;;> \scheme{'FAIL}, \scheme{'SKIP}, \scheme{'ERROR}).

(define-syntax test
  (syntax-rules (quote)
    ((test expect expr)
     (test #f expect expr))
    ((test name expect (expr ...))
     (test-propagate-info name expect (expr ...) ()))
    ((test name 'expect expr)
     (test-propagate-info name 'expect expr ()))
    ((test name (expect ...) expr)
     (test-syntax-error
      'test
      "the test expression should come last: (test <expected> (<expr> ...)) "
      (test name (expect ...) expr)))
    ((test name expect expr)
     (test-propagate-info name expect expr ()))
    ((test a ...)
     (test-syntax-error 'test "test requires 2 or 3 arguments" (test a ...)))))

;;> \macro{(test-equal equal [name] expect expr)}

;;> Equivalent to test, using \var{equal} for comparison instead of
;;> \scheme{equal?}.

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal equal . args)
     (parameterize ((current-test-comparator equal))
       (test . args)))))

;;> \macro{(test-assert [name] expr)}

;;> Like \scheme{test} but evaluates \var{expr} and checks that it's true.

(define-syntax test-assert
  (syntax-rules ()
    ((_ expr)
     (test-assert #f expr))
    ((_ name expr)
     (test-propagate-info name #f expr ((assertion . #t))))
    ((test a ...)
     (test-syntax-error 'test-assert "1 or 2 arguments required"
                        (test a ...)))))

;;> \macro{(test-not [name] expr)}

;;> Like \scheme{test} but evaluates \var{expr} and checks that it's false.

(define-syntax test-not
  (syntax-rules ()
    ((_ expr) (test-assert (not expr)))
    ((_ name expr) (test-assert name (not expr)))))

;;> \macro{(test-values [name] expect expr)}

;;> Like \scheme{test} but \var{expect} and \var{expr} can both
;;> return multiple values.

(define-syntax test-values
  (syntax-rules ()
    ((_ expect expr)
     (test-values #f expect expr))
    ((_ name expect expr)
     (test name (call-with-values (lambda () expect) (lambda results results))
       (call-with-values (lambda () expr) (lambda results results))))))

;;> \macro{(test-error [name] expr)}

;;> Like \scheme{test} but evaluates \var{expr} and checks that it
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

;;> Low-level macro to pass alist info to the underlying \var{test-run}.

(define-syntax test-propagate-info
  (syntax-rules ()
    ;; TODO: Extract interesting variables so we can show their values
    ;; on failure.  Vars are empty for now.
    ((test-propagate-info name expect expr info)
     (test-vars () name expect expr info))))

(define-syntax test-vars
  (syntax-rules ()
    ((_ (vars ...) n expect expr ((key . val) ...))
     (test-run (lambda () expect)
               (lambda () expr)
               `((name . ,n)
                 (source . expr)
                 (var-names . (vars ...))
                 (var-values . ,(list vars ...))
                 (key . val) ...)))))

;;> The procedural interface to testing.  \var{expect} and \var{expr}
;;> should be thunks, and \var{info} is an alist of properties used in
;;> test reporting.

(define (test-run expect expr info)
  (let ((info (test-expand-info info)))
    (if (and (cond ((current-test-group)
                    => (lambda (g) (not (test-group-ref g 'skip-group?))))
                   (else #t))
             (or (and (not (any (lambda (f) (f info)) (current-test-removers)))
                      (or (pair? (current-test-removers))
                          (null? (current-test-filters))))
                 (any (lambda (f) (f info)) (current-test-filters))))
        ((current-test-applier) expect expr info)
        ((current-test-skipper) info))))

;;> Returns true if either \scheme{(equal? expect res)}, or
;;> \var{expect} is inexact and \var{res} is within
;;> \scheme{current-test-epsilon} of \var{expect}.

(define (test-equal? expect res)
  (or (equal? expect res)
      (if (real? expect)
          (and (inexact? expect)
               (real? res)
               ;; tests which expect an inexact value can
               ;; accept an equivalent exact value
               ;; (inexact? res)
               (approx-equal? expect res (current-test-epsilon)))
          (and (complex? res)
               (complex? expect)
               (test-equal? (real-part expect) (real-part res))
               (test-equal? (imag-part expect) (imag-part res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group interface

;;> \section{Test Groups}

;;> Tests can be collected in groups for

;;> Wraps \var{body} as a single test group, which can be filtered
;;> and summarized separately.

;;> \example{
;;> (test-group "pi"
;;>   (test 3.14159 (acos -1))
;;>   (test 3 (acos -1))
;;>   (test 3.14159 (acos "-1")))
;;> }

(define-syntax test-group
  (syntax-rules ()
    ((_ name-expr body ...)
     (let ((name name-expr)
           (old-group (current-test-group)))
       (when (not (string? name))
         (error "a name is required, got " 'name-expr name))
       (test-begin name)
       (guard
           (exn
            (else
             (warning "error in group outside of tests")
             (print-exception exn (current-error-port))
             (test-group-inc! (current-test-group) 'count)
             (test-group-inc! (current-test-group) 'ERROR)
             (test-failure-count (+ 1 (test-failure-count)))))
         body ...)
       (test-end name)
       (current-test-group old-group)))))

;;> Begin testing a new group until the closing \scheme{(test-end)}.

(define (test-begin . o)
  (let* ((name (if (pair? o) (car o) ""))
         (parent (current-test-group))
         (group (make-test-group name parent)))
    ;; include a newline if we are directly nested in a parent with no
    ;; tests yet
    (when (and parent
               (zero? (test-group-ref parent 'subgroups-count 0))
               (not (test-group-ref parent 'verbose)))
      (newline))
    ;; header
    (cond
     ((test-group-ref group 'skip-group?)
      (display (make-string (or (test-group-indent-width group) 0) #\space))
      (display (strikethrough (bold (string-append name ":"))))
      (display " SKIP"))
     ((test-group-ref group 'verbose)
      (display
       (test-header-line
        (string-append "testing " name)
        (or (test-group-indent-width group) 0))))
     (else
      (display
       (string-append
        (make-string (or (test-group-indent-width group) 0)
                     #\space)
        (bold (string-append name ": "))))))
    ;; set the current test group
    (current-test-group group)))

;;> Ends testing group introduced with \scheme{(test-begin)}, and
;;> summarizes the results.  The \var{name} is optional, but if
;;> present should match the corresponding \scheme{test-begin} name,
;;> or a warning is printed.

(define (test-end . o)
  (let ((name (and (pair? o) (car o))))
    (cond
     ((current-test-group)
      => (lambda (group)
           (when (and name (not (equal? name (test-group-name group))))
             (warning "mismatched test-end:" name (test-group-name group)))
           (let ((parent (test-group-ref group 'parent)))
             (when (and (test-group-ref group 'skip-group?)
                        (zero? (test-group-ref group 'subgroups-count 0)))
               (newline))
             ;; only report if there's something to say
             ((current-test-group-reporter) group)
             (when parent
               (test-group-inc! parent 'subgroups-count)
               (cond
                ((test-group-ref group 'skip-group?)
                 (test-group-inc! parent 'subgroups-skip))
                ((and (zero? (test-group-ref group 'FAIL 0))
                      (zero? (test-group-ref group 'ERROR 0))
                      (= (test-group-ref group 'subgroups-pass 0)
                         (test-group-ref group 'subgroups-count 0)))
                 (test-group-inc! parent 'subgroups-pass))))
             (current-test-group parent)
             group))))))

;;> Exits with a failure status if any tests have failed,
;;> and a successful status otherwise.

(define (test-exit)
  (when (current-test-group)
    (warning "calling test-exit with unfinished test group:"
             (test-group-name (current-test-group))))
  (exit (zero? (test-failure-count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define-syntax test-syntax-error
  (syntax-rules ()
    ((_) (syntax-error "invalid use of test-syntax-error"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-group representation

;;> \section{Accessors}

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
     (and (or (and parent (test-group-ref parent 'skip-group?))
              (any (lambda (f) (f group)) (current-test-group-removers))
              (and (null? (current-test-group-removers))
                   (pair? (current-test-group-filters))))
          (not (any (lambda (f) (f group)) (current-test-group-filters)))))
    group))

;;> Returns the name of a test group info object.

(define (test-group-name group) (car group))

;;> Returns the value of a \var{field} in a test var{group} info
;;> object.  \var{field} should be a symbol, and predefined fields
;;> include \scheme{parent}, \scheme{verbose}, \scheme{level},
;;> \scheme{start-time}, \scheme{skip-group?}, \scheme{count},
;;> \scheme{total-pass}, \scheme{total-fail}, \scheme{total-error}.

(define (test-group-ref group field . o)
  (if group
      (apply assq-ref (cdr group) field o)
      (and (pair? o) (car o))))

;;> Sets the value of a \var{field} in a test \var{group} info object.

(define (test-group-set! group field value)
  (cond
   ((assq field (cdr group))
    => (lambda (x) (set-cdr! x value)))
   (else (set-cdr! group (cons (cons field value) (cdr group))))))

;;> Increments the value of a \var{field} in a test \var{group} info
;;> object by \var{amount}, defaulting to 1.

(define (test-group-inc! group field . o)
  (let ((amount (if (pair? o) (car o) 1)))
    (cond
     ((assq field (cdr group))
      => (lambda (x) (set-cdr! x (+ amount (cdr x)))))
     (else (set-cdr! group (cons (cons field amount) (cdr group)))))))

;;> Updates a \var{field} in a test group info object by consing
;;> \var{value} onto it.

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
  (cond
   ((> (abs a) (abs b))
    (approx-equal? b a epsilon))
   ((zero? a)
    (< (abs b) epsilon))
   (else
    (< (abs (/ (- a b) b)) epsilon))))

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

(define (display-to-string x)
  (if (string? x) x (call-with-output-string (lambda (out) (display x out)))))

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
           ((assq 'source info)
            => (lambda (src)
                 (truncate-source (cdr src) (- (current-column-width) 12))))
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

(define (test-expand-info info)
  (let ((expr (assq-ref info 'source)))
    (if (and (pair? expr)
             (pair-source expr)
             (not (assq-ref info 'line-number)))
        `((file-name . ,(car (pair-source expr)))
          (line-number . ,(cdr (pair-source expr)))
          ,@info)
        info)))

(define (test-default-applier expect expr info)
  (let* ((group (current-test-group))
         (indent (and group (test-group-indent-width group))))
    (cond
     ((or (not group) (test-group-ref group 'verbose))
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
            ((current-test-reporter)
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
            ((current-test-reporter) status info)))))))

(define (test-default-skipper info)
  ((current-test-reporter) 'SKIP info))

(define (test-status-color status)
  (case status
    ((ERROR) (lambda (x) (underline (red x))))
    ((FAIL) red)
    ((SKIP) yellow)
    (else (lambda (x) x))))

(define (test-status-message status)
  ((test-status-color status) (symbol->string status)))

(define (test-status-code status)
  ((test-status-color status)
   ;; alternatively: ❗, ✗, ‒, ✓
   ;; unfortunately, these have ambiguous width
   (case status
     ((ERROR) "!")
     ((FAIL) "x")
     ((SKIP) "-")
     (else "."))))

(define (display-expected/actual expected actual)
  (let* ((e-str (write-to-string expected))
         (a-str (write-to-string actual))
         (diff (diff e-str a-str read-char)))
    (write-string "expected ")
    (write-string (edits->string/color (car diff) (car (cddr diff)) 1))
    (write-string " but got ")
    (write-string (edits->string/color (cadr diff) (car (cddr diff)) 2))))

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
    (display-expected/actual (assq-ref info 'expected) (assq-ref info 'result))
    (newline)))
  ;; print variables
  (cond
   ((and (memq status '(FAIL ERROR)) (assq-ref info 'var-names))
    => (lambda (names)
         (let ((values (assq-ref info 'var-values)))
           (if (and (pair? names)
                    (pair? values)
                    (= (length names) (length values)))
               (let ((indent2
                      (string-append indent (make-string 2 #\space))))
                 (for-each
                  (lambda (name value)
                    (display indent2) (write name) (display ": ")
                    (write value) (newline))
                  names values))))))))

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

(define (test-header-line str . indent)
  (let* ((header (string-append
                  (make-string (if (pair? indent) (car indent) 0) #\space)
                  "-- " str " "))
         (len (string-length header)))
    (string-append (bold header)
                   (make-string (max 0 (- (current-column-width) len)) #\-))))

(define (test-default-handler status info)
  (define indent
    (make-string
     (+ 4 (cond ((current-test-group)
                 => (lambda (group) (or (test-group-indent-width group) 0)))
                (else 0)))
     #\space))
  ;; update group info
  (cond
   ((current-test-group)
    => (lambda (group)
         (if (not (eq? 'SKIP status))
             (test-group-inc! group 'count))
         (test-group-inc! group status)
         ;; maybe wrap long status lines
         (let ((width (max (- (current-column-width)
                              (or (test-group-indent-width group) 0))
                           4))
               (column
                (+ (string-length (or (test-group-name group) ""))
                   (or (test-group-ref group 'count) 0)
                   1)))
           (if (and (zero? (modulo column width))
                    (not (test-group-ref group 'verbose)))
               (display (string-append "\n" (string-copy indent 4))))))))
  ;; update global failure count for exit status
  (cond
   ((or (eq? status 'FAIL) (eq? status 'ERROR))
    (test-failure-count (+ 1 (test-failure-count)))))
  (cond
   ((or (not (current-test-group))
        (test-group-ref (current-test-group) 'verbose))
    ;; display status
    (display "[")
    (if (not (eq? status 'ERROR)) (display " ")) ; pad
    (display (test-status-message status))
    (display "]")
    (newline)
    (test-print-failure indent status info))
   ((eq? status 'SKIP))
   (else
    (display (test-status-code status))
    (cond
     ((and (memq status '(FAIL ERROR)) (current-test-group))
      => (lambda (group)
           (test-group-push! group 'failures (list indent status info)))))
    (cond ((current-test-group)
           => (lambda (group) (test-group-set! group 'trailing #t))))))
  (flush-output-port)
  status)

(define (test-default-group-reporter group)
  (define (plural word n)
    (if (= n 1) word (string-append word "s")))
  (define (percent n d)
    (string-append " (" (number->string (/ (round (* 1000.0 (/ n d))) 10))
                   "%)"))
  (let* ((end-time (current-second))
         (start-time (test-group-ref group 'start-time))
         (duration (- end-time start-time))
         (base-count (or (test-group-ref group 'count) 0))
         (base-pass (or (test-group-ref group 'PASS) 0))
         (base-fail (or (test-group-ref group 'FAIL) 0))
         (base-err (or (test-group-ref group 'ERROR) 0))
         (skip (or (test-group-ref group 'SKIP) 0))
         (pass (+ base-pass (or (test-group-ref group 'total-pass) 0)))
         (fail (+ base-fail (or (test-group-ref group 'total-fail) 0)))
         (err (+ base-err (or (test-group-ref group 'total-error) 0)))
         (count (+ pass fail err))
         (subgroups-count (or (test-group-ref group 'subgroups-count) 0))
         (subgroups-skip (or (test-group-ref group 'subgroups-skip) 0))
         (subgroups-run (- subgroups-count subgroups-skip))
         (subgroups-pass (or (test-group-ref group 'subgroups-pass) 0))
         (indent (make-string (or (test-group-indent-width group) 0) #\space)))
    (if (and (not (test-group-ref group 'verbose))
             (test-group-ref group 'trailing))
        (newline))
    (cond
     ((or (positive? count) (positive? subgroups-count))
      (if (not (= base-count (+ base-pass base-fail base-err)))
          (warning "inconsistent count:"
                   base-count base-pass base-fail base-err))
      (cond
       ((positive? count)
        (display indent)
        (display
         ((if (= pass count) green (lambda (x) x))
          (string-append
           (number->string pass) " out of " (number->string count)
           (percent pass count))))
        (display
         (string-append
          (plural " test" pass) " passed in "
          (number->string duration) " seconds"
          (cond
           ((zero? skip) "")
           (else (string-append " (" (number->string skip)
                                (plural " test" skip) " skipped)")))
          ".\n"))))
      (cond ((positive? fail)
             (display indent)
             (display
              (red
               (string-append
                (number->string fail) (plural " failure" fail)
                (percent fail count) ".\n")))))
      (cond ((positive? err)
             (display indent)
             (display
              ((lambda (x) (underline (red x)))
               (string-append
                (number->string err) (plural " error" err)
                (percent err count) ".\n")))))
      (cond
       ((not (test-group-ref group 'verbose))
        (for-each
         (lambda (failure)
           (display indent)
           (display (red
                     (string-append (display-to-string (cadr failure)) ": ")))
           (display (test-get-name! (car (cddr failure))))
           (newline)
           (apply test-print-failure failure))
         (reverse (or (test-group-ref group 'failures) '())))))
      (cond
       ((positive? subgroups-run)
        (display indent)
        (display
         ((if (= subgroups-pass subgroups-run)
              green (lambda (x) x))
          (string-append
           (number->string subgroups-pass) " out of "
           (number->string subgroups-run)
           (percent subgroups-pass subgroups-run))))
        (display (plural " subgroup" subgroups-pass))
        (display " passed.\n")))))
    (cond
     ((test-group-ref group 'verbose)
      (display
       (test-header-line
        (string-append "done testing " (or (test-group-name group) ""))
        (or (test-group-indent-width group) 0)))
      (newline)))
    (cond
     ((test-group-ref group 'parent)
      => (lambda (parent)
           (test-group-set! parent 'trailing #f)
           (test-group-inc! parent 'total-pass pass)
           (test-group-inc! parent 'total-fail fail)
           (test-group-inc! parent 'total-error err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

;;> \section{Parameters}

;;> The current test group as started by \scheme{test-group} or
;;> \scheme{test-begin}.

(define current-test-group (make-parameter #f))

;;> If true, show more verbose output per test.  Inferred from the
;;> environment variable TEST_VERBOSE.

(define current-test-verbosity
  (make-parameter
   (cond ((get-environment-variable "TEST_VERBOSE")
          => (lambda (s) (not (member s '("" "0")))))
         (else #f))))

;;> The epsilon used for floating point comparisons.

(define current-test-epsilon (make-parameter 1e-5))

;;> The underlying comparator used in testing, defaults to
;;> \scheme{test-equal?}.

(define current-test-comparator (make-parameter test-equal?))

;;> The test applier - what we do with non-skipped tests.  Takes the
;;> same signature as \scheme{test-run}, should be responsible for
;;> evaluating the thunks, determining the status of the test, and
;;> passing this information to \scheme{current-test-reporter}.

(define current-test-applier (make-parameter test-default-applier))

;;> The test skipper - what we do with non-skipped tests.  This should
;;> not evaluate the thunks and simply pass off to
;;> \scheme{current-test-reporter}.

(define current-test-skipper (make-parameter test-default-skipper))

;;> Takes two arguments, the symbol status of the test and the info
;;> alist.  Reports the result of the test and updates bookkeeping in
;;> the current test group for reporting.

(define current-test-reporter (make-parameter test-default-handler))

;;> Takes one argument, a test group, and prints a summary of the test
;;> results for that group.

(define current-test-group-reporter
  (make-parameter test-default-group-reporter))

;;> A running count of all test failures and errors across all groups
;;> (and threads).  Used by \scheme{test-exit}.

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
  (lambda (group) (string-search str (test-group-name group))))

;; simplified version from SRFI 130
(define (string-split str ch)
  (let ((end (string-length str)))
    (let lp ((from 0) (to 0) (res '()))
      (cond
       ((>= to end)
        (reverse (if (> to from) (cons (substring str from to) res) res)))
       ((eqv? ch (string-ref str to))
        (lp (+ to 1) (+ to 1) (cons (substring str from to) res)))
       (else
        (lp from (+ to 1) res))))))

(define (getenv-filter-list proc name)
  (cond
   ((get-environment-variable name)
    => (lambda (s)
         (let lp ((ls (string-split s #\,))
                  (res '()))
           (cond
            ((null? ls) (reverse res))
            (else
             (let* ((s (car ls))
                    (f (guard
                           (exn
                            (else
                             (warning
                              (string-append "invalid filter '" s
                                             "' from environment variable: "
                                             name))
                             (print-exception exn (current-error-port))
                             #f))
                         (proc s))))
               (lp (cdr ls) (if f (cons f res) res))))))))
   (else '())))

(define current-test-group-filters
  (make-parameter
   (getenv-filter-list string->group-matcher "TEST_GROUP_FILTER")))

(define current-test-group-removers
  (make-parameter
   (getenv-filter-list string->group-matcher "TEST_GROUP_REMOVE")))

;;> Parameters controlling which test groups are skipped.  Each
;;> parameter is a list of procedures of one argument, a test group
;;> info, which can be queried with \var{test-group-name} and
;;> \var{test-group-ref}.  Analogous to SRFI 1, a filter selects a
;;> group for inclusion and a removers for exclusion.  The defaults
;;> are set automatically from the environment variables
;;> TEST_GROUP_FILTER and TEST_GROUP_REMOVE, which should be
;;> comma-delimited lists of strings which are checked for a substring
;;> match in the test group name.  A test group is skipped if it does
;;> not match any filter and:
;;> \itemlist[
;;> \item{its parent group is skipped, or}
;;> \item{it matches a remover, or}
;;> \item{no removers are specified but some filters are}
;;> ]
;;/

(define current-test-filters
  (make-parameter (getenv-filter-list string->info-matcher "TEST_FILTER")))

(define current-test-removers
  (make-parameter (getenv-filter-list string->info-matcher "TEST_REMOVE")))

;;> Parameters controlling which tests are skipped.  Each parameter is
;;> a list of procedures of one argument, a test info alist, which can
;;> be queried with \scheme{test-get-name!} or \scheme{assq}.
;;> Analogous to SRFI 1, a filter selects a test for inclusion and a
;;> removers for exclusion.  The defaults are set automatically from
;;> the environment variables TEST_FILTER and TEST_REMOVE, which
;;> should be comma-delimited lists of strings which are checked for a
;;> substring match in the test name.  A test is skipped if its group
;;> is skipped, or if it does not match a filter and:
;;> \itemlist[
;;> \item{it matches a remover, or}
;;> \item{no removers are specified but some filters are}
;;> ]
;;/

;;> Parameter controlling the current column width for test output,
;;> can be set from the environment variable TEST_COLUMN_WIDTH,
;;> otherwise defaults to 78.  For portability of implementation (and
;;> resulting output), does not attempt to use termios to determine
;;> the actual available width.

(define current-column-width
  (make-parameter
   (or (cond ((get-environment-variable "TEST_COLUMN_WIDTH")
              => string->number)
             (else #f))
       78)))
