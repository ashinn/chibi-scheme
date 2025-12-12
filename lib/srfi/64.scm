;; Copyright (c) 2005, 2006, 2007, 2012, 2013 Per Bothner
;; Added "full" support for Chicken, Gauche, Guile and SISC.
;;   Alex Shinn, Copyright (c) 2005.
;; Modified for Scheme Spheres by Álvaro Castro-Castilla, Copyright (c) 2012.
;; Support for Guile 2 by Mark H Weaver <mhw@netris.org>, Copyright (c) 2014.
;; Modified for Chibi-Scheme by Ekaitz Zarraga Copyright (c) 2023.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-syntax %test-export
  (syntax-rules ()
    ((%test-export . names) (if #f #f))))

(cond-expand
 (srfi-9
  (define-syntax %test-record-define
    (syntax-rules ()
      ((%test-record-define tname alloc runner? (name index getter setter) ...)
       (define-record-type tname
	 (alloc)
	 runner?
	 (name getter setter) ...)))))
 (else
  (define %test-runner-cookie (list "test-runner"))
  (define-syntax %test-record-define
    (syntax-rules ()
      ((%test-record-define tname alloc runner? (name index getter setter) ...)
       (begin
	 (define (runner? obj)
	   (and (vector? obj)
		(> (vector-length obj) 1)
		(eq? (vector-ref obj 0) %test-runner-cookie)))
	 (define (alloc)
	   (let ((runner (make-vector 23)))
	     (vector-set! runner 0 %test-runner-cookie)
	     runner))
	 (begin
	   (define (getter runner)
	     (vector-ref runner index)) ...)
	 (begin
	   (define (setter runner value)
	     (vector-set! runner index value)) ...)))))))

(%test-record-define test-runner
 %test-runner-alloc test-runner?
 ;; Cumulate count of all tests that have passed and were expected to.
 (pass-count 1 test-runner-pass-count test-runner-pass-count!)
 (fail-count 2 test-runner-fail-count test-runner-fail-count!)
 (xpass-count 3 test-runner-xpass-count test-runner-xpass-count!)
 (xfail-count 4 test-runner-xfail-count test-runner-xfail-count!)
 (skip-count 5 test-runner-skip-count test-runner-skip-count!)
 (skip-list 6 %test-runner-skip-list %test-runner-skip-list!)
 (fail-list 7 %test-runner-fail-list %test-runner-fail-list!)
 ;; Normally #t, except when in a test-apply.
 (run-list 8 %test-runner-run-list %test-runner-run-list!)
 (skip-save 9 %test-runner-skip-save %test-runner-skip-save!)
 (fail-save 10 %test-runner-fail-save %test-runner-fail-save!)
 (group-stack 11 test-runner-group-stack test-runner-group-stack!)
 (on-test-begin 12 test-runner-on-test-begin test-runner-on-test-begin!)
 (on-test-end 13 test-runner-on-test-end test-runner-on-test-end!)
 ;; Call-back when entering a group. Takes (runner suite-name count).
 (on-group-begin 14 test-runner-on-group-begin test-runner-on-group-begin!)
 ;; Call-back when leaving a group.
 (on-group-end 15 test-runner-on-group-end test-runner-on-group-end!)
 ;; Call-back when leaving the outermost group.
 (on-final 16 test-runner-on-final test-runner-on-final!)
 ;; Call-back when expected number of tests was wrong.
 (on-bad-count 17 test-runner-on-bad-count test-runner-on-bad-count!)
 ;; Call-back when name in test=end doesn't match test-begin.
 (on-bad-end-name 18 test-runner-on-bad-end-name test-runner-on-bad-end-name!)
 ;; Cumulate count of all tests that have been done.
 (total-count 19 %test-runner-total-count %test-runner-total-count!)
 ;; Stack (list) of (count-at-start . expected-count):
 (count-list 20 %test-runner-count-list %test-runner-count-list!)
 (result-alist 21 test-result-alist test-result-alist!)
 ;; Field can be used by test-runner for any purpose.
 ;; test-runner-simple uses it for a log file.
 (aux-value 22 test-runner-aux-value test-runner-aux-value!)
)

(define (test-runner-reset runner)
  (test-result-alist! runner '())
  (test-runner-pass-count! runner 0)
  (test-runner-fail-count! runner 0)
  (test-runner-xpass-count! runner 0)
  (test-runner-xfail-count! runner 0)
  (test-runner-skip-count! runner 0)
  (%test-runner-total-count! runner 0)
  (%test-runner-count-list! runner '())
  (%test-runner-run-list! runner #t)
  (%test-runner-skip-list! runner '())
  (%test-runner-fail-list! runner '())
  (%test-runner-skip-save! runner '())
  (%test-runner-fail-save! runner '())
  (test-runner-group-stack! runner '()))

(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define (%test-null-callback runner) #f)

(define (test-runner-null)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner (lambda (runner name count) #f))
    (test-runner-on-group-end! runner %test-null-callback)
    (test-runner-on-final! runner %test-null-callback)
    (test-runner-on-test-begin! runner %test-null-callback)
    (test-runner-on-test-end! runner %test-null-callback)
    (test-runner-on-bad-count! runner (lambda (runner count expected) #f))
    (test-runner-on-bad-end-name! runner (lambda (runner begin end) #f))
    runner))

;; Not part of the specification.  FIXME
;; Controls whether a log file is generated.
(define test-log-to-file #t)

(define (test-runner-simple)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner test-on-group-begin-simple)
    (test-runner-on-group-end! runner test-on-group-end-simple)
    (test-runner-on-final! runner test-on-final-simple)
    (test-runner-on-test-begin! runner test-on-test-begin-simple)
    (test-runner-on-test-end! runner test-on-test-end-simple)
    (test-runner-on-bad-count! runner test-on-bad-count-simple)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

(cond-expand
 (srfi-39
  (define test-runner-current (make-parameter #f))
  (define test-runner-factory (make-parameter test-runner-simple)))
 (else
  (define %test-runner-current #f)
  (define-syntax test-runner-current
    (syntax-rules ()
      ((test-runner-current)
       %test-runner-current)
      ((test-runner-current runner)
       (set! %test-runner-current runner))))
  (define %test-runner-factory test-runner-simple)
  (define-syntax test-runner-factory
    (syntax-rules ()
      ((test-runner-factory)
       %test-runner-factory)
      ((test-runner-factory runner)
       (set! %test-runner-factory runner))))))

;; A safer wrapper to test-runner-current.
(define (test-runner-get)
  (let ((r (test-runner-current)))
    (if (not r)
        (cond-expand
         (srfi-23 (error "test-runner not initialized - test-begin missing?"))
         (else #t)))
      r))

(define (%test-specifier-matches spec runner)
  (spec runner))

(define (test-runner-create)
  ((test-runner-factory)))

(define (%test-any-specifier-matches list runner)
  (let ((result #f))
    (let loop ((l list))
      (cond ((null? l) result)
	    (else
	     (if (%test-specifier-matches (car l) runner)
		 (set! result #t))
	     (loop (cdr l)))))))

;; Returns #f, #t, or 'xfail.
(define (%test-should-execute runner)
  (let ((run (%test-runner-run-list runner)))
    (cond ((or
	    (not (or (eqv? run #t)
		     (%test-any-specifier-matches run runner)))
	    (%test-any-specifier-matches
	     (%test-runner-skip-list runner)
	     runner))
	    (test-result-set! runner 'result-kind 'skip)
	    #f)
	  ((%test-any-specifier-matches
	    (%test-runner-fail-list runner)
	    runner)
	   (test-result-set! runner 'result-kind 'xfail)
	   'xfail)
	  (else #t))))

(define (%test-begin suite-name count)
  (if (not (test-runner-current))
      (let ((r (test-runner-create)))
	(test-runner-current r)
	(test-runner-on-final! r
	  (let ((old-final (test-runner-on-final r)))
	    (lambda (r) (old-final r) (test-runner-current #f))))))
  (let ((runner (test-runner-current)))
    ((test-runner-on-group-begin runner) runner suite-name count)
    (%test-runner-skip-save! runner
			       (cons (%test-runner-skip-list runner)
				     (%test-runner-skip-save runner)))
    (%test-runner-fail-save! runner
			       (cons (%test-runner-fail-list runner)
				     (%test-runner-fail-save runner)))
    (%test-runner-count-list! runner
			     (cons (cons (%test-runner-total-count runner)
					 count)
				   (%test-runner-count-list runner)))
    (test-runner-group-stack! runner (cons suite-name
					(test-runner-group-stack runner)))))
(define-syntax test-begin
  (syntax-rules ()
    ((test-begin suite-name)
     (%test-begin suite-name #f))
    ((test-begin suite-name count)
     (%test-begin suite-name count))))

(define (test-on-group-begin-simple runner suite-name count)
  (if (null? (test-runner-group-stack runner))
      (begin
	(display "%%%% Starting test ")
	(display suite-name)
	(if test-log-to-file
	    (let* ((log-name (if (string? test-log-to-file) test-log-to-file
                                 (string-append suite-name ".log")))
                   ;; Replace "bad" characters in log file name with #\_
                   (fix-invalid-char
                    (lambda (ch)
                      (if (or (char-alphabetic? ch)
                              (char-numeric? ch)
                              (char=? ch #\space)
                              (char=? ch #\-)
                              (char=? ch #\+)
                              (char=? ch #\_)
                              (char=? ch #\.)
                              (char=? ch #\,))
                          ch
                          #\_)))
                   (log-file-name
                    (cond-expand (r7rs
                                  (string-map fix-invalid-char log-name))
                                 (else
                                  (let ((t (string-copy log-name))
                                        (tlen (string-length log-name)))
                                    (do ((i 0 (+ i 1))) ((>= i tlen) t)
                                      (string-set! t i (fix-invalid-char
                                                        (string-ref t i))))))))
		   (log-file
				 (open-output-file log-file-name)))
	      (display "%%%% Starting test " log-file)
	      (display suite-name log-file)
	      (newline log-file)
	      (test-runner-aux-value! runner log-file)
	      (display "  (Writing full log to \"")
	      (display log-file-name)
	      (display "\")")))
	(newline)))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(begin
	  (display "Group begin: " log)
	  (display suite-name log)
	  (newline log))))
  #f)

(define (test-on-group-end-simple runner)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(begin
	  (display "Group end: " log)
	  (display (car (test-runner-group-stack runner)) log)
	  (newline log))))
  #f)

(define (%test-on-bad-count-write runner count expected-count port)
  (display "*** Total number of tests was " port)
  (display count port)
  (display " but should be " port)
  (display expected-count port)
  (display ". ***" port)
  (newline port)
  (display "*** Discrepancy indicates testsuite error or exceptions. ***" port)
  (newline port))

(define (test-on-bad-count-simple runner count expected-count)
  (%test-on-bad-count-write runner count expected-count (current-output-port))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(%test-on-bad-count-write runner count expected-count log))))

(define (test-on-bad-end-name-simple runner begin-name end-name)
  (let ((msg (string-append (%test-format-line runner) "test-end " begin-name
			    " does not match test-begin " end-name)))
    (cond-expand
     (srfi-23 (error msg))
     (else (display msg) (newline)))))


(define (%test-final-report1 value label port)
  (if (> value 0)
      (begin
	(display label port)
	(display value port)
	(newline port))))

(define (%test-final-report-simple runner port)
  (%test-final-report1 (test-runner-pass-count runner)
		      "# of expected passes      " port)
  (%test-final-report1 (test-runner-xfail-count runner)
		      "# of expected failures    " port)
  (%test-final-report1 (test-runner-xpass-count runner)
		      "# of unexpected successes " port)
  (%test-final-report1 (test-runner-fail-count runner)
		      "# of unexpected failures  " port)
  (%test-final-report1 (test-runner-skip-count runner)
		      "# of skipped tests        " port))

(define (test-on-final-simple runner)
  (%test-final-report-simple runner (current-output-port))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(%test-final-report-simple runner log))))

(define (%test-format-line runner)
   (let* ((line-info (test-result-alist runner))
	  (source-file (assq 'source-file line-info))
	  (source-line (assq 'source-line line-info))
	  (file (if source-file (cdr source-file) "")))
     (if source-line
	 (string-append file ":"
			(number->string (cdr source-line)) ": ")
	 "")))

(define (%test-end suite-name line-info)
  (let* ((r (test-runner-get))
	 (groups (test-runner-group-stack r))
	 (line (%test-format-line r)))
    (test-result-alist! r line-info)
    (if (null? groups)
	(let ((msg (string-append line "test-end not in a group")))
	  (cond-expand
	   (srfi-23 (error msg))
	   (else (display msg) (newline)))))
    (if (and suite-name (not (equal? suite-name (car groups))))
	((test-runner-on-bad-end-name r) r suite-name (car groups)))
    (let* ((count-list (%test-runner-count-list r))
	   (expected-count (cdar count-list))
	   (saved-count (caar count-list))
	   (group-count (- (%test-runner-total-count r) saved-count)))
      (if (and expected-count
	       (not (= expected-count group-count)))
	  ((test-runner-on-bad-count r) r group-count expected-count))
      ((test-runner-on-group-end r) r)
      (test-runner-group-stack! r (cdr (test-runner-group-stack r)))
      (%test-runner-skip-list! r (car (%test-runner-skip-save r)))
      (%test-runner-skip-save! r (cdr (%test-runner-skip-save r)))
      (%test-runner-fail-list! r (car (%test-runner-fail-save r)))
      (%test-runner-fail-save! r (cdr (%test-runner-fail-save r)))
      (%test-runner-count-list! r (cdr count-list))
      (if (null? (test-runner-group-stack r))
	  ((test-runner-on-final r) r)))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name . body)
     (let ((r (or (test-runner-current)
                  (begin
                   (test-runner-current (test-runner-create))
                   (test-runner-current)))))
       ;; Ideally should also set line-number, if available.
       (test-result-alist! r (list (cons 'test-name suite-name)))
       (if (%test-should-execute r)
	   (dynamic-wind
	       (lambda () (test-begin suite-name))
	       (lambda () . body)
	       (lambda () (test-end  suite-name))))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name form cleanup-form)
     (test-group suite-name
		    (dynamic-wind
			(lambda () #f)
			(lambda () form)
			(lambda () cleanup-form))))
    ((test-group-with-cleanup suite-name cleanup-form)
     (test-group-with-cleanup suite-name #f cleanup-form))
    ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
     (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest))))

(define (test-on-test-begin-simple runner)
 (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(let* ((results (test-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (source-form (assq 'source-form results))
	       (test-name (assq 'test-name results)))
	  (display "Test begin:" log)
	  (newline log)
	  (if test-name (%test-write-result1 test-name log))
	  (if source-file (%test-write-result1 source-file log))
	  (if source-line (%test-write-result1 source-line log))
	  (if source-form (%test-write-result1 source-form log))))))

(define-syntax test-result-ref
  (syntax-rules ()
    ((test-result-ref runner pname)
     (test-result-ref runner pname #f))
    ((test-result-ref runner pname default)
     (let ((p (assq pname (test-result-alist runner))))
       (if p (cdr p) default)))))

(define (test-on-test-end-simple runner)
  (let ((log (test-runner-aux-value runner))
	(kind (test-result-ref runner 'result-kind)))
    (if (memq kind '(fail xpass))
	(let* ((results (test-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (test-name (assq 'test-name results)))
	  (if (or source-file source-line)
	      (begin
		(if source-file (display (cdr source-file)))
		(display ":")
		(if source-line (display (cdr source-line)))
		(display ": ")))
	  (display (if (eq? kind 'xpass) "XPASS" "FAIL"))
	  (if test-name
	      (begin
		(display " ")
		(display (cdr test-name))))
	  (newline)))
    (if (output-port? log)
	(begin
	  (display "Test end:" log)
	  (newline log)
	  (let loop ((list (test-result-alist runner)))
	    (if (pair? list)
		(let ((pair (car list)))
		  ;; Write out properties not written out by on-test-begin.
		  (if (not (memq (car pair)
				 '(test-name source-file source-line source-form)))
		      (%test-write-result1 pair log))
		  (loop (cdr list)))))))))

(define (%test-write-result1 pair port)
  (display "  " port)
  (display (car pair) port)
  (display ": " port)
  (write (cdr pair) port)
  (newline port))

(define (test-result-set! runner pname value)
  (let* ((alist (test-result-alist runner))
	 (p (assq pname alist)))
    (if p
	(set-cdr! p value)
	(test-result-alist! runner (cons (cons pname value) alist)))))

(define (test-result-actual-value! runner value)
  (test-result-set! runner 'actual-value value))

(define (test-result-expected-value! runner value)
  (test-result-set! runner 'expected-value value))

(define (test-result-clear runner)
  (test-result-alist! runner '()))

(define (test-result-remove runner pname)
  (let* ((alist (test-result-alist runner))
	 (p (assq pname alist)))
    (if p
	(test-result-alist! runner
				   (let loop ((r alist))
				     (if (eq? r p) (cdr r)
					 (cons (car r) (loop (cdr r)))))))))

(define (test-result-kind . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-current))))
    (test-result-ref runner 'result-kind)))

(define (test-passed? . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-get))))
    (memq (test-result-ref runner 'result-kind) '(pass xpass))))

(define (%test-report-result)
  (let* ((r (test-runner-get))
	 (result-kind (test-result-kind r)))
    (case result-kind
      ((pass)
       (test-runner-pass-count! r (+ 1 (test-runner-pass-count r))))
      ((fail)
       (test-runner-fail-count!	r (+ 1 (test-runner-fail-count r))))
      ((xpass)
       (test-runner-xpass-count! r (+ 1 (test-runner-xpass-count r))))
      ((xfail)
       (test-runner-xfail-count! r (+ 1 (test-runner-xfail-count r))))
      (else
       (test-runner-skip-count! r (+ 1 (test-runner-skip-count r)))))
    (%test-runner-total-count! r (+ 1 (%test-runner-total-count r)))
    ((test-runner-on-test-end r) r)))

(cond-expand
 (srfi-34
  (define-syntax %test-evaluate-with-catch
    (syntax-rules ()
      ((%test-evaluate-with-catch test-expression)
       (guard (err (else #f)) test-expression)))))
 (else
  (define-syntax %test-evaluate-with-catch
    (syntax-rules ()
      ((%test-evaluate-with-catch test-expression)
       test-expression)))))

(define (%test-source-line2 form)
  '())

(define (%test-on-test-begin r)
  (%test-should-execute r)
  ((test-runner-on-test-begin r) r)
  (not (eq? 'skip (test-result-ref r 'result-kind))))

(define (%test-on-test-end r result)
    (test-result-set! r 'result-kind
		      (if (eq? (test-result-ref r 'result-kind) 'xfail)
			  (if result 'xpass 'xfail)
			  (if result 'pass 'fail))))

(define (test-runner-test-name runner)
  (test-result-ref runner 'test-name ""))

(define-syntax %test-comp2body
  (syntax-rules ()
		((%test-comp2body r comp expected expr)
		 (let ()
		   (if (%test-on-test-begin r)
		       (let ((exp expected))
			 (test-result-expected-value! r exp)
			 (let ((res (%test-evaluate-with-catch expr)))
			   (test-result-actual-value! r res)
			   (%test-on-test-end r (comp exp res)))))
		   (%test-report-result)))))

(define (%test-approximate= error)
  (lambda (value expected)
    (let ((rval (real-part value))
          (ival (imag-part value))
          (rexp (real-part expected))
          (iexp (imag-part expected)))
      (and (>= rval (- rexp error))
           (>= ival (- iexp error))
           (<= rval (+ rexp error))
           (<= ival (+ iexp error))))))

(define-syntax %test-comp1body
  (syntax-rules ()
    ((%test-comp1body r expr)
     (let ()
       (if (%test-on-test-begin r)
	   (let ()
	     (let ((res (%test-evaluate-with-catch expr)))
	       (test-result-actual-value! r res)
	       (%test-on-test-end r res))))
       (%test-report-result)))))



(define-syntax test-end
  (syntax-rules ()
    ((test-end)
     (%test-end #f '()))
    ((test-end suite-name)
     (%test-end suite-name '()))))
(define-syntax test-assert
  (syntax-rules ()
    ((test-assert tname test-expression)
     (let* ((r (test-runner-get))
      (name tname))
 (test-result-alist! r '((test-name . tname)))
 (%test-comp1body r test-expression)))
    ((test-assert test-expression)
     (let* ((r (test-runner-get)))
 (test-result-alist! r '())
 (%test-comp1body r test-expression)))))
(define-syntax %test-comp2
  (syntax-rules ()
    ((%test-comp2 comp tname expected expr)
     (let* ((r (test-runner-get))
      (name tname))
 (test-result-alist! r (list (cons 'test-name tname)))
 (%test-comp2body r comp expected expr)))
    ((%test-comp2 comp expected expr)
     (let* ((r (test-runner-get)))
 (test-result-alist! r '())
 (%test-comp2body r comp expected expr)))))
(define-syntax test-equal
  (syntax-rules ()
    ((test-equal . rest)
     (%test-comp2 equal? . rest))))
(define-syntax test-eqv
  (syntax-rules ()
    ((test-eqv . rest)
     (%test-comp2 eqv? . rest))))
(define-syntax test-eq
  (syntax-rules ()
    ((test-eq . rest)
     (%test-comp2 eq? . rest))))
(define-syntax test-approximate
  (syntax-rules ()
    ((test-approximate tname expected expr error)
     (%test-comp2 (%test-approximate= error) tname expected expr))
    ((test-approximate expected expr error)
     (%test-comp2 (%test-approximate= error) expected expr))))

(cond-expand
 ((and srfi-34 srfi-35)
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (%test-comp1body r (guard (ex ((condition-type? etype)
		   (and (condition? ex) (condition-has-type? ex etype)))
		  ((procedure? etype)
		   (etype ex))
		  ((equal? etype #t)
		   #t)
		  (else #t))
	      expr #f))))))
 (srfi-34
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (%test-comp1body r (guard (ex (else #t)) expr #f))))))
 (else
   (define-syntax %test-error
     (syntax-rules ()
       ((%test-error r etype expr)
        (begin
          ((test-runner-on-test-begin r) r)
          (call-with-current-continuation
            (lambda (k)
              (with-exception-handler
                (lambda (x) (k (test-result-set! r 'result-kind 'pass)))
                (lambda () expr (k (test-result-set! r 'result-kind 'fail))))))
          (%test-report-result)))))))



(define-syntax test-error
  (syntax-rules ()
    ((test-error name etype expr)
     (let ((r (test-runner-get)))
       (test-result-alist! r `((test-name . ,name)))
       (%test-error r etype expr)))
    ((test-error etype expr)
     (let ((r (test-runner-get)))
       (test-result-alist! r '())
       (%test-error r etype expr)))
    ((test-error expr)
     (let ((r (test-runner-get)))
       (test-result-alist! r '())
       (%test-error r #t expr)))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner form ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
           (lambda () (test-runner-current runner))
           (lambda () form ...)
           (lambda () (test-runner-current saved-runner)))))))

(define (test-apply first . rest)
  (if (test-runner? first)
      (test-with-runner first (apply test-apply rest))
      (let ((r (test-runner-current)))
	(if r
	    (let ((run-list (%test-runner-run-list r)))
	      (cond ((null? rest)
		     (%test-runner-run-list! r (reverse run-list))
		     (first)) ;; actually apply procedure thunk
		    (else
		     (%test-runner-run-list!
		      r
		      (if (eq? run-list #t) (list first) (cons first run-list)))
		     (apply test-apply rest)
		     (%test-runner-run-list! r run-list))))
	    (let ((r (test-runner-create)))
	      (test-with-runner r (apply test-apply first rest))
	      ((test-runner-on-final r) r))))))

;;; Predicates

(define (%test-match-nth n count)
  (let ((i 0))
    (lambda (runner)
      (set! i (+ i 1))
      (and (>= i n) (< i (+ n count))))))

(define-syntax test-match-nth
  (syntax-rules ()
    ((test-match-nth n)
     (test-match-nth n 1))
    ((test-match-nth n count)
     (%test-match-nth n count))))

(define (%test-match-all . pred-list)
  (lambda (runner)
    (let ((result #t))
      (let loop ((l pred-list))
	(if (null? l)
	    result
	    (begin
	      (if (not ((car l) runner))
		  (set! result #f))
	      (loop (cdr l))))))))

(define-syntax test-match-all
  (syntax-rules ()
    ((test-match-all pred ...)
     (%test-match-all (%test-as-specifier pred) ...))))

(define (%test-match-any . pred-list)
  (lambda (runner)
    (let ((result #f))
      (let loop ((l pred-list))
	(if (null? l)
	    result
	    (begin
	      (if ((car l) runner)
		  (set! result #t))
	      (loop (cdr l))))))))

(define-syntax test-match-any
  (syntax-rules ()
    ((test-match-any pred ...)
     (%test-match-any (%test-as-specifier pred) ...))))

;; Coerce to a predicate function:
(define (%test-as-specifier specifier)
  (cond ((procedure? specifier) specifier)
	((integer? specifier) (test-match-nth 1 specifier))
	((string? specifier) (test-match-name specifier))
	(else
	 (error "not a valid test specifier"))))

(define-syntax test-skip
  (syntax-rules ()
    ((test-skip pred ...)
     (let ((runner (test-runner-get)))
       (%test-runner-skip-list! runner
				  (cons (test-match-all (%test-as-specifier pred)  ...)
					(%test-runner-skip-list runner)))))))

(define-syntax test-expect-fail
  (syntax-rules ()
    ((test-expect-fail pred ...)
     (let ((runner (test-runner-get)))
       (%test-runner-fail-list! runner
				  (cons (test-match-all (%test-as-specifier pred)  ...)
					(%test-runner-fail-list runner)))))))

(define (test-match-name name)
  (lambda (runner)
    (equal? name (test-runner-test-name runner))))

(define (test-read-eval-string string)
  (let* ((port (open-input-string string))
	 (form (read port)))
    (if (eof-object? (read-char port))
	 (eval form)
	(cond-expand
	 (srfi-23 (error "(not at eof)"))
	 (else "error")))))

